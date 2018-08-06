
Implementing a Parser
=====================

This document describes the best practices for adding an entirely new parser to the toolkit.
It is only useful if you either plan to implement a parser for a markup language not (yet)
supported by Laika, want to replace one of the existing parsers, or are just
curious about the inner workings of the library. None of the information here is required
for standard usage of Laika.


Factory Contract
----------------

The contract a parser factory has to adhere to is captured in the following
trait:

    trait MarkupParser {
  
      def fileSuffixes: Set[String]

      def blockParsers: Seq[BlockParserBuilder]
      
      def spanParsers: Seq[SpanParserBuilder]
  
      def extensions: Seq[ExtensionBundle]
      
    }

These are the four abstract method each parser has to implement.    
    
The `fileSuffixes` method should simply return the set of supported file
suffixes (without the '.'). For Markdown this would be `Set("md", "markdown")`
for example. It is not recommended to support generic suffixes like `txt` as this
could lead to conflicts with other parsers.

The `blockParsers` and `spanParsers` collections provide the definitions for the
actual markup parsers. They are based on Laika's own parser combinator implementation
that is optimized for parsing of text markup. See the sections below for how to 
create such a definition.

The `extensions` collection allows to add functionality beyond just markup parsing.
If, for example, the markup parser inserts AST nodes into the final result that
are not known to the default renderers for HTML or PDF, an extension might provide
additional renderers for these custom nodes. 

See the [ExtensionBundle Scaladoc][bundle-scaladoc] for an overview of the available
extension hooks.

Finally there are two concrete methods that may be overridden if required:

    def escapedChar: Parser[String] = TextParsers.any.take(1)

    def createBlockListParser (parser: Parser[Block]): Parser[Seq[Block]] = 
      (parser <~ opt(blankLines))*

The first method controls the parsing of an escape sequence (the character after
a backslash). The default implementation accepts any character.

The second controls how a parser for a single block is turned into a parser for
a sequence of blocks. The default implementation just skips whitespace between
the blocks and repeats the same parser. The parser passed to this method already
includes all the block parsers this trait has specified as well as any extensions
a user might have installed.


[bundle-scaladoc]: ../api/#laika.bundle.ExtensionBundle



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

This is captured in the API for creating a parser:

    SpanParser.forStartChar('*').standalone {
      delimitedBy("*")
    }    

The parser above simply parses text between two asterisk. The `standalone` method means
that the parser is independent of any parsers in the host language.

If the parser is allowed to contain nested spans with any of the parsers of the host language
or any installed extensions, your definition must ask for the recursive parser to be provided
as you cannot anticipate the extensions a user might have configured:

    SpanParser.forStartChar('*').recursive { recParsers =>
      recParsers.delimitedRecursiveSpans(delimitedBy("*"))
    } 

Here you simply pass the same text parser from the previous example to the recursive
parsers provided by Laika which lifts the text parser into a span parser.

It's important that parsers do not look for the initial character as this will be consumed by the map lookup already. 
And they are still allowed to fail, in which case the special character will be treated as
normal text input.



Block Parsers
-------------

The API for creating block parsers is similar to that for span parsers. One difference
is that the start character is optional, as many types of blocks, like a plain paragraph
for example, do not have a concrete start character. In those cases you can create the block
parser like this:

    BlockParser.withoutStartChar.standalone {
      // your parser impl
    }    

If the parser allows the nesting of other blocks you can rely on the library to pass
the recursive parsers for all installed block parsers to your parser definition, similar
to the span parser mechanism:

    BlockParser.forStartChar('*').recursive { recParsers =>
      // your parser impl
    } 

Laika offers a `BlockParsers` object with convenience methods for creating
a typical block parser. This is the signature of the first one:
  
    def block (firstLinePrefix: Parser[Any], 
               linePrefix: Parser[Any], 
               nextBlockPrefix: Parser[Any]): Parser[List[String]]

It allows to parse a block based on three simple conditions, provided in form of
the three parser parameters: detecting the first line of a block, any subsequent
line and finally whether the block continues after a blank line has been seen.
Often a blank line marks the end of a block, but there are exceptions, like code
blocks or list items that span multiple paragraphs.

The following code is an example for a parser for quoted blocks, decorated
by a `>` character at the start of each line:

    BlockParser.forStartChar('>').recursive { recParsers =>
      
      val textAfterDeco = TextParsers.ws       // any whitespace
      val decoratedLine = '>' ~ textAfterDeco  // '>' followed by whitespace
      val afterBlankLine = Parsers.failure("blank line ends block")
      
      val textBlock = BlockParsers.block(textAfterDeco, decoratedLine, afterBlankLine)
      
      recParsers.recursiveBlocks(textBlock) ^^ (QuotedBlock(_, Nil))
    }
    
This implementation uses the `BlockParsers` helper. It passes just the whitespace
parser as the condition for the first line, as the start character has already 
been consumed. For subsequent lines, the decoration needs to be present, so
we pass `decoratedLine` as the condition for them.

Finally we pass the `textBlock` parser to the recursive parsers which lift our
`Parser[String]` to a `Parser[Seq[Block]]` and the result of that parser is 
passed to the `QuotedBlock` AST node.

Finally there is a second utility that can be used for indented blocks:

    def indentedBlock (minIndent: Int = 1,
              linePredicate: => Parser[Any] = success(()),
              endsOnBlankLine: Boolean = false,
              firstLineIndented: Boolean = false,
              maxIndent: Int = Int.MaxValue): Parser[String]

Like the other utility it allows to specify a few predicates. This method
is not used for parsing Markdown's indented blocks, though, as Markdown has
a very special way of treating whitespace.
