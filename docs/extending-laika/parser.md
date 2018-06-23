
Implementing a Parser
=====================

This document describes the best practices for adding an entirely new parser to the toolkit.
It is only useful if you either plan to implement a parser for a markup language not (yet)
supported by Laika, want to replace one of the existing parsers, or are just
curious about the inner workings of the library. None of the information here is required
for standard usage of Laika.


Factory Contract
----------------

The contract a parser factory has to adhere to is quite simple, it has mix in the following
trait:

    trait ParserFactory {
  
      def fileSuffixes: Set[String]

      def newParser: Input => Document
      
    }
    
The `fileSuffixes` method should simply return the set of supported file
suffixes (without the '.'). For Markdown this would be `Set("md","markdown")`
for example. It is not recommended to support generic suffixes like `txt` as this
could lead to conflicts with other parsers.

The `newParser` method is the actual factory method. It should return
a function that is safe to use repeatedly and concurrently.
    
`Input` is a little IO abstraction provided by Laika so that you do not have to
deal with the details of whether the text comes from a string or file or other 
types of streams.
 
`Document` is a case class representing the root node of the document tree.

The way you implement this function depends entirely on your requirements and preferences.
You may use the parser combinators from Laika or some other parsing technology.

Only if you intend to create a parser that you'd want to contribute to the Laika core, it would
be required to build upon the available base traits unless this is impractical for some reason.    
    
If you are not using the base traits all you need to know from this document is the next section
about providing an API. The remaining sections deal with the optional base traits provided by Laika.



Providing an API
----------------

When you build a new parser you should provide the following features for your users:

* An easy way to use your parser with the Transform API

* An easy way to use it with the Parse API

* A fluent API for specifying options (in case your parser is configurable)

The first two come for free when you create an object that extends `Input => RawDocument`.
The built-in `Markdown` object is an example. Since it does extend that function,
you can easily use it in expressions like this:

    val transform = Transform from Markdown to HTML

You can achieve this by providing a trait that offers all the available configuration
hooks and returns `this` for each of these methods for easy chaining. Additionally
you create a companion object that represents the default configuration.

As an example, this is how a simplified class and object for the Markdown parser 
could look (Scaladoc comments, imports and various extension hooks removed for brevity):

    package laika.parse.markdown
    
    class Markdown private (isStrict: Boolean) extends ParserFactory {

      val fileSuffixes = Set("md", "markdown")
      
      def strict = new Markdown(true)
  
      private lazy val parser = {
        lazy val blockDirectives = ...
        lazy val spanDirectives = ...
        
        new RootParser(blockDirectives, spanDirectives, isStrict)
      }

      val newParser: Input => Document = 
         (input: Input) => parser.parseDocument(input.asParserInput, input.path)
  
    }

    object Markdown extends Markdown(false) 

As you see, all the low-level parsing details are left in the root parser, this is
just a wrapper for providing a convenient public API. Support for reStructuredText
is implemented in a similar way.

It calls `asParserInput` on the `Input` instance which is the most convenient way
if you use parser combinators, as it directly gives you a `ParserContext` no matter
where the text is actually read from. When not using combinators, you can use
`Input.asReader` to obtain a plain `java.io.Reader`.



Text Parsers
------------

The functionality provided by these parsers is not strictly required for implementing
parsers for any markup language. But the parsers are convenient helpers as they are more tailored 
for the special requirements of parsing text markup, which is quite different from parsing programming 
languages for example. 

In particular for parsing inline markup (like \*this\* for adding emphasis) Laika's parsers deviate
from the standard approach of combinators, which in this case would often mean to build
a long list of (flat) choices of (often) regex parsers which are all tried on each character.
The downside of this approach is that this is often quite slow, and not easily extensible, 
if you want to support new or customized markup for an existing parser without touching the parser
implementation.

For typical basic regex parsers there is usually a corresponding option in the `TextParsers`
object. You can see the full list of provided parsers in the [Scaladoc][text-scaladoc].
We'll just show a few examples here:

Parsing three or more lower-case characters:

    "[a-z]{3,}".r              // regex
    
    anyIn('a' to 'z') min 3    // Laika alternative
    
Parsing any character apart from space or tab:

    "[^ \t]*".r               // regex
    
    anyBut(' ','\t')          // Laika alternative
    
Note that for the Laika base parsers the default is to parse any number of characters,
as this is the most common case. To read just one you can write:

    anyBut(' ','\t') take 1
    
The parsers of this trait will be faster than regex parsers in many scenarios, but
there will be edge cases where it is the other way round. To be really sure it's best
to do some benchmarks first. 


[text-scaladoc]: ../api/#laika.parse.core.text.TextParsers


Span Parsers
------------

Laika parses text markup in two phases: in the first phase it only looks for markup which is
significant for identifying a block type (like a blockquote, an ordered list, a code block
or a regular paragraph for example). It then further parses the text of each block to look
for inline markup, which is what this trait does.

Like described in the previous section, Laika inline parsers avoid the performance bottleneck
of trying a long list of choices for each character. Instead it builds a map of available
span parsers, mapping from the first character to the parser, and then performs a simple
map lookup for each character. If no parser is mapped to the character it simply continues
reading. This works for nested structures, too.

To provide all span parsers for a custom markup parser based on Laika's `RootParserBase`
you only have to implement the abstract method

    protected lazy val spanParsers: Map[Char,Parser[Span]]
    
For Markdown the default span parser map is created like this:

    protected lazy val spanParsers: Map[Char, Parser[Span]] = Map(
        '*' -> (strong('*') | em('*')),    
        '_' -> (strong('_') | em('_')),
        '`' -> (literalEnclosedByDoubleChar | literalEnclosedBySingleChar), 
        '\\'-> (lineBreak | (escapedChar ^^ { Text(_) })),
        '[' -> link,
        '<' -> simpleLink,
        '!' -> image
      )
    
The parsers mapped to the characters are then pretty standard, apart from the fact
that they are usually built upon the base parsers provided by the `TextParsers`
object and the `RecursiveSpanParsers` trait. These parsers must not look for
the initial character as this will be consumed by the map lookup already. And they
are still allowed to fail, in which case the special character will be treated as
normal text input.



Block Parsers
-------------

Laika offers a `BlockParsers` object with convenience methods for creating
a typical block parser. This is the signature of the first one:
  
    def block (firstLinePrefix: Parser[Any], 
               linePrefix: Parser[Any], 
               nextBlockPrefix: Parser[Any]): Parser[List[String]]

It allows to parse a block based on three simple conditions, provided in form of
the three parser parameters: detecting the first line of a block, any subsequent
line and finally whether the block continues after a blank line has been seen.
Often a blank line marks the end of a block, but there are exceptions, like code
blocks or list items that span multiple parapraphs.

Finally there is a second utility that can be used for indented blocks:

    def indentedBlock (minIndent: Int = 1,
              linePredicate: => Parser[Any] = success(()),
              endsOnBlankLine: Boolean = false,
              firstLineIndented: Boolean = false,
              maxIndent: Int = Int.MaxValue): Parser[String]

Like the other utility it allows to specify a few predicates. This method
is not used for parsing Markdown's indented blocks, though, as Markdown has
a very special way of treating whitespace.

To provide all block parsers for a custom markup parser based on Laika's `RootParserBase`
you have to implement the abstract methods

    protected def topLevelBlock: Parser[Block]
    
    protected def nestedBlock: Parser[Block]

    protected def nonRecursiveBlock: Parser[Block]

The first two methods allow to make a distinction between a block that is only
allowed to occur on the top level and those which can appear anywhere. 
Of course any block can potentially appear in both parsers.

The third method `nonRecursiveBlock` is rarely ever used, it exists
as a safeguard against malicious input. If a certain (configurable) nest level
has been reached, only block elements that do not potentially contain further
nested blocks are considered for subsequent parsing.
