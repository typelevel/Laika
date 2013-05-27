
Implementing a Parser
=====================

This document describes the best practices for adding an entirely new parser to the toolkit.
It is only useful if you either plan to implement a parser for a markup language not (yet)
supported by Laika, want to replace one of the existing parsers, or are just
curious about the inner workings of the library. None of the information here is required
for standard usage of Laika.

The contract a parser has to adhere to is quite simple, it has to be a function
with this signature:

    Input => Document
    
`Input` is a little IO abstraction provided by Laika so that you do not have to
deal with the details of whether the text comes from a string or file or other 
types of streams.
 
`Document` is a case class representing the root node of the document tree.

The way you implement this function depends entirely on your requirements and preferences.
You may use the parser combinators from the Scala SDK or some other parsing technology.
You may also use the base traits Laika provides when using combinators, or you may ignore them.

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

The first two come for free when you create an object that extends `Input => Document`.
The built-in `Markdown` object is an example. Since it does extend that function,
you can easily use it in expressions like this:

    val transform = Transform from Markdown to HTML
    
When you want to specify options (`Markdown` currently has only one) you can do this
inline:

    val transform = Transform from (Markdown withVerbatimHTML) to HTML

You can achieve this by providing a trait that offers all the available configuration
hooks and returns `this` for each of these methods for easy chaining. Additionally
you create a companion object that represents the default configuration.

This is how the trait and object look for the Markdown parser as an example (Scaladoc
comments removed for brevity):

    package laika.parse.markdown
    
    import laika.io.Input
    import laika.parse.markdown.html.HTMLParsers
    import laika.tree.Elements.Document

    class Markdown private (verbatimHTML: Boolean) extends (Input => Document) {

      def withVerbatimHTML = new Markdown(true)
  
      private lazy val parser = {
        if (verbatimHTML) new BlockParsers with InlineParsers with HTMLParsers
        else              new BlockParsers with InlineParsers
      }

      def apply (input: Input) = parser.parseDocument(input.asParserInput)
  
    }

    object Markdown extends Markdown(false) 

As you see, all the low-level parsing details are left in the three traits, this is
just a wrapper for providing a convenient public API. Support for reStructuredText
is implemented in a similar way.

It calls `asParserInput` on the `Input` instance which is the most convenient way
if you use parser combinators, as it directly gives you a `Reader[Char]` no matter
where the text is actually read from. When not using combinators, you can use
`Input.asReader` to obtain a plain `java.io.Reader`.



Trait MarkupParsers
-------------------

This is the base trait for both the `InlineParsers` and `BlockParsers` sub-traits,
but its parsers can also be used directly.

The functionality provided by these parsers is not strictly required for implementing
parsers for any markup language. But it comes with several advantages over using
only the parsers from the Scala SDK. The parsers are more tailored for the special 
requirements of parsing text markup, which is quite different from parsing programming 
languages for example. 

In particular for parsing inline markup (like \*this\* for adding emphasis) Laika's parsers deviate
from the standard approach of combinators, which in this case would often mean to build
a long list of (flat) choices of (often) regex parsers which are all tried on each character.
The downside of this approach is that this is often quite slow, and not easily extensible, 
if you want to support new or customized markup for an existing parser without touching the parser
implementation.

For typical basic regex parsers there is usually a corresponding option in the `MarkupParsers`
trait. You can see the full list of provided parsers in the [Scaladoc][markup-scaladoc].
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


[markup-scaladoc]: api/#laika.parse.MarkupParsers


Trait InlineParsers
-------------------

Laika parses text markup in two phases: in the first phase it only looks for markup which is
significant for identifying a block type (like a blockquote, an ordered list, a code block
or a regular paragraph for example). It then further parses the text of each block to look
for inline markup, which is what this trait does.

Like described in the previous section, Laika inline parsers avoid the performance bottleneck
of trying a long list of choices for each character. Instead it builds a map of available
span parsers, mapping from the first character to the parser, and then performs a simple
map lookup for each character. If no parser is mapped to the character it simply continues
reading. This works for nested structures, too.

The main method in the `InlineParsers` trait that block parsers use to parse their text is:

    def parseInline (source: String, spanParsers: Map[Char, Parser[Span]])
    
The second parameter is the parser map we mentioned. The return type is `List[Span]`, 
with `Span` being a sub-trait of `Element`, the base class for all node types. 
Examples for case classes mixing in `Span` are `Link`, `Image` or `Emphasized`.

For Markdown this map is created like this:

    protected def newSpanParserMap = Map(
      '*' -> (strong('*') | em('*')),    
      '_' -> (strong('_') | em('_')),
      '`' -> (codeEnclosedByDoubleChar | codeEnclosedBySingleChar), 
      '\\'-> (escapedChar ^^ { Text(_) }),
      '[' -> link,
      '<' -> simpleLink,
      '!' -> image
    )
    
The parsers mapped to the characters are then pretty standard, apart from the fact
that they are usually built upon the base parsers provided by the `MarkupParsers`
trait. These parsers must not look for
the initial character as this will be consumed by the map lookup already. And they
are still allowed to fail, in which case the special character will be treated as
normal text input.

Sub-traits can override this method and add more parsers to the map.

If any of the parsers need to parse nested structures, `InlineParsers` comes
with an additional method that allows to specify both, a condition (in form of a parser)
for when the span ends and a map for available nested parsers, which may or may not
be identical to the map for top level spans, depending on the markup specification.
The signature is as follows:

    def spans (parser: TextParser, nested: Map[Char, Parser[Span]]) 

`TextParser` is a sub-trait of `Parser[String]`, and the type of parser produced
by most methods in `MarkupParsers`. The rules for the map are identical
to those for top-level spans. The text parser parses the current span, but may get
suspended for parsing nesting structures based on the parser map. 



Trait BlockParsers
------------------

This is the base trait for parsing blocks, the first phase of the two-phase parsing
process. It adds a few utility parsers like `eol` (end-of-line), `blankLine`
or `restOfLine`. See the [Scaladoc][block-scaladoc] for details on those.

If you mix in this trait you have to implement the two abstract parsers:

    def topLevelBlock: Parser[Block]
 
    def nestedBlock: Parser[Block]

This can be a parser build as a list of choices. In contrast to the inline
parsers the performance impact is less of a concern here. The names should be
self-explanatory. Depending on the concrete markup language specification
these two parsers may be identical, if each block type is allowed as a top-level
or nested block.

 Like `Span`, `Block` is a sub-trait of `Element`, the base class for all node types. 
Examples for case classes mixing in `Block` are `Paragraph`, `CodeBlock` or `OrderedList`.

Finally this trait provides an (entirely optional) convenience method for creating
a typical block parser. This is the signature:

    def block (firstLinePrefix: Parser[Any], 
               linePrefix: Parser[Any], 
               nextBlockPrefix: Parser[Any]): Parser[List[String]]

It allows to parse a block based on three simple conditions, provided in form of
the three parser parameters: detecting the first line of a block, any subsequent
line and finally whether the block continues after a blank line has been seen.
Often a blank line marks the end of a block, but there are exceptions, like code
blocks or list items that span multiple parapraphs.

[block-scaladoc]: api/#laika.parse.BlockParsers

