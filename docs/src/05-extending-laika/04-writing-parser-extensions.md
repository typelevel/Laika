
Writing Parser Extensions
=========================

Implementing a custom parser is one of two possible ways to extend the syntax of a text markup language.

The second option is [Implementing Directives] and in many cases that approach offers more convenience.
It is based on a common syntax for declaring a directive and its attributes and body elements.
Therefore directives can be implemented without writing a custom parser and without getting familiar
with Laika's parser combinators.

There are usually only two scenarios where you may prefer to write a parser extension instead:

* You have very special requirements for the syntax. 
  If, for example, you want to design a new table markup format, 
  this is quite impossible to express with the fixed syntax of a directive.

* You want the most concise syntax for elements that are used frequently. 
  One example is to support short references to issues in a ticket system in the format `#123`.
  The shortest syntax that is possible with directives would be something like `@:ticket(123)` 
  which you may still find too verbose.
  
  
Multi-Pass Markup Parsing
-------------------------

Before delving deeper into the practicalities of writing a parser extension, it is helpful to understand how
markup parsing works under the hood in Laika.

In contrast to Laika's HOCON and CSS parsers which are single-pass, markup parsing is always a multi-pass operation.

At the minimum it is parsing in two passes: the first looks for markup which is significant for demarcating
a block type (like a blockquote, an ordered list, a code block or a regular paragraph for example). 
It then further parses the text of each block to look for inline markup.

But it may require any number of additional passes, as blocks or spans themselves can be nested inside each other.
An example for a recursive block structure is a list, where each list item may contain nested lists.
A span may be recursive where an element like a link allows further inline markup for the link text.

The distinction between block and span parsers is also reflected in the APIs.
The document AST comes with base traits `Block` and `Span` (both extending the root `Element` trait)
and registration of span and block parsers (which produce `Block` or `Span` AST nodes respectively) 
is separate and comes with slightly different configuration options as shown in the following sections. 


Prerequisites
-------------

The content of this chapter builds on top of concepts introduced in other chapters,
therefore reading those first might help with following through the examples.

First, all parsers build on top of [Laika's Parser Combinators].
Having its own implementation helps with keeping all functionality tightly integrated and adding
some optimizations useful for the specific use case of markup parsing right into the base parsers.

Second, since the parsers we discuss in this chapter all produce AST nodes, 
it might help to get familiar with [The Document AST] first.


Span Parsers
------------

Span parsers participate in inline parsing. 
Examples for built-in span parsers are those for emphasized text, literal spans or links.

Adding a span parser to an existing text markup language requires two steps:

* Write the actual implementation of the parser as a type of `PrefixedParser[Span]`.
* Add this declaration to an `ExtensionBundle`.

Let's go through this step by step with an example. 
We are going to build a simple span parser for a ticket reference in the form of `#123`.


### Span Parser Implementation

We require a mandatory `#` symbol followed by one or more digits.

```scala mdoc:silent
import laika.ast._
import laika.parse.implicits._
import laika.parse.text._
import TextParsers.someOf

val ticketParser: PrefixedParser[Span] = 
  ("#" ~> someOf(CharGroup.digit)).map { num =>
    val url = s"http://our-tracker.com/$num"
    SpanLink(Seq(Text("#" + num)), ExternalTarget(url))
  }
```

* We first look for a literal `"#"`.
* We then require at least one digit. 
  The `someOf` parser covers this as it means "one or more of the specified characters".
* We combine the two parsers with `~>` which means: 
  "concatenate the two parsers, but only keep the result of the right one".
  See [Laika's Parser Combinators] for details.
* We map the result and create a `SpanLink` node (which implements `Span`).
  We use the literal input as the link text and then build the URL as an external target.
  
This first example hard-codes the base URL. If your parser extension is for internal use, this may not be a problem.
But if you require a configurable base URL, we later show an enhanced example that has access to the configuration.


### Registering a Span Parser

For bundling all your Laika extensions, you need to extend `ExtensionBundle`.
In our case we only need to override the `parsers` property 
and leave everything else at the empty default implementations.

```scala mdoc:silent
import laika.bundle._

object TicketSyntax extends ExtensionBundle {

  val description: String = "Parser Extension for Tickets"

  override val parsers: ParserBundle = ParserBundle(
    spanParsers = Seq(SpanParser.standalone(ticketParser))
  )

}
```

The `SpanParser.standalone` method can be used in cases where your parser does not need access to the parser
of the host language for recursive parsing.

Finally you can register your extension together with any built-in extensions you may use:

@:select(config)

@:choice(sbt)
```scala mdoc:invisible
import laika.sbt.LaikaPlugin.autoImport._
```

```scala mdoc:compile-only
import laika.markdown.github.GitHubFlavor

laikaExtensions := Seq(
  GitHubFlavor,
  TicketSyntax
)
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .using(TicketSyntax)
  .build
```
@:@


### Access to Configuration

This will enhance the previous example by making the base URL configurable.

Access to configuration or other AST nodes is not possible in the parsing phase itself, 
as each parser executes in isolation and multiple input documents are processed in parallel.

But your parser can return an instance that implements `SpanResolver` instead of directly producing a link.
Such a resolver will then participate in the AST transformation phase where its `resolve` method will be invoked:

```scala mdoc:silent
import laika.parse.SourceFragment

case class TicketResolver (num: String, 
                           source: SourceFragment, 
                           options: Options = NoOpt) extends SpanResolver {

  type Self = TicketResolver

  def resolve (cursor: DocumentCursor): Span = {
    cursor.config.get[String]("ticket.baseURL").fold(
      error => InvalidSpan(s"Invalid base URL: $error", source),
      baseURL => SpanLink(Seq(Text("#"+num)), ExternalTarget(s"$baseURL$num"))
    )
  }
  
  val unresolvedMessage: String = s"Unresolved reference to ticket $num"
  
  def runsIn(phase: RewritePhase): Boolean          = true
  def withOptions(options: Options): TicketResolver = copy(options = options)
}
```

The `DocumentCursor` passed to the `resolve` method provides access to the project configuration which we use
in this example, but also to all ASTs of the input tree, including other documents. 
It can therefore be used for advanced functionality like producing a table of contents.

The API of the `cursor.config` property is documented in @:api(laika.config.Config).

In our case we expect a string, but we also need to handle errors now, as the access might fail
when the value is missing or it's not a string. 
We return an `InvalidSpan` for errors, which is a useful kind of AST node as it allows the user to control the
error handling. 
The presence of such an element will by default cause the transformation to fail with the provided error message
shown alongside any other errors encountered.
But users can also switch to a "visual debugging" mode by rendering all errors in place. 
See [Error Handling] for details. 

The `options` property of our implementation is a mandatory property of each `Span` or `Block` element.
Options are a generic hook that allows users to add an id or multiple styles to a node.
It's always best to have an empty default argument like in our example.

With this change in place, the user can now provide the base URL in the builder of the `Transformer`:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
laikaConfig := LaikaConfig.defaults
  .withConfigValue("ticket.baseURL", "https://example.com/issues")
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue("ticket.baseURL", "https://example.com/issues")
  .build
```
@:@

The original ticket parser then only needs to be adjusted to return our resolver instead:

```scala mdoc:silent
val tickets: PrefixedParser[Span] = 
  ("#" ~> someOf(CharGroup.digit)).withCursor.map { case (num, source) =>
    TicketResolver(num, source)
  }
```

The registration steps are identical to the previous example.

The `withCursor` combinator is a convenient helper for any AST node that can trigger a failed state 
in a later stage of AST transformation. 
The additional `SourceFragement` instance that this combinator provides preserves context info
for the parsed input that can be used to print precise line and column location in case this node is invalid.


### Detecting Markup Boundaries

The examples so far came with a major simplification to keep the code concise.
The logic we implemented would also match on a link embedded in markup for example: `http://somwhere.com/page#123`.
In such a case we would not want the `#123` substring to be recognized as a ticket link.

For this reason text markup languages usually define a set of rules which define how to detect markup boundaries.
In reStructuredText these are defined in a very clear and strict way ([rst-markup-recognition-rules]), 
while in Markdown the definition is somewhat more fuzzy.

Laika's parser combinators come with convenient helpers to check conditions on preceding and following characters
without consuming them. Let's fix our parser implementation:

```scala mdoc:silent
import laika.parse.text.TextParsers.{ delimiter, nextNot }

val parser: PrefixedParser[Span] = {
  val letterOrDigit: Char => Boolean = { c => 
    Character.isDigit(c) || Character.isLetter(c)
  }
  val delim = delimiter("#").prevNot(letterOrDigit)
  val ticketNum = someOf(CharGroup.digit)
  val postCond = nextNot(letterOrDigit)
  
  (delim ~> ticketNum <~ postCond).withCursor.map { case (num, source) =>
    TicketResolver(num, source)
  }
}
```

Instead of just using a literal parser for the `"#"` we use the `delimiter` parser which comes with convenient
shortcuts to check conditions before and after the delimiter.

The end of our span has no explicit delimiter, so we use a standalone `nextNot` condition.

Checking that neither the preceding nor the following character is a letter or digit increases the likeliness
we only match on constructs the user actually meant to be ticket references.

[rst-markup-recognition-rules]: https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules


### Recursive Parsing

In our example we used the `SpanParser.standalone` method for registration.
In cases where your parser needs access to the parser of the host language for recursive parsing
we need to use the `SpanParser.recursive` entry point instead:

```scala mdoc:silent
import laika.parse.implicits._
import laika.parse.text.TextParsers.delimitedBy

SpanParser.recursive { recParsers =>
  ("*" ~> recParsers.recursiveSpans(delimitedBy("*"))).map(Emphasized(_))
} 
```

This parser parses markup between two asterisk.
It also detects and parses any span kinds provided by either the host markup language or other installed extensions
between these two delimiters.
The result from the parser obtained with the call to `recursiveSpans` is always of type `Seq[Span]`.

Only this entry point gives you access to a parser that is fully configured with all extensions the user has specified.
This parser needs to be injected as you would otherwise hard-code a concrete, fixed set of inline parsers.


Block Parsers
-------------

Block parsers participate in parsing block level elements. 
Examples for built-in block parsers are those for lists, headers, tables or code blocks.

Adding a block parser to an existing text markup language requires two steps:

* Write the actual implementation of the parser as a type of `PrefixedParser[Block]`.
* Add this declaration to an `ExtensionBundle`.

Let's again go through this step by step with an example.
We are going to build a block parser for a quoted block.
In practice you would not need such an extension, as both supported markup languages in Laika already contain
a quoted block element. 
But it's a good example as the syntax is so simple.
In our case we require that each line of such a block starts with the `>` character:

```
> This line is part of the quotation
> This line, too.
But this line isn't
```


### Block Parser Implementation

Let's look at the implementation and examine it line by line:

```scala mdoc:silent
import laika.ast._
import laika.bundle.BlockParser
import laika.parse.implicits._
import laika.parse.markup.BlockParsers
import laika.parse.text.TextParsers.ws

val quotedBlockParser = BlockParser.recursive { recParsers =>

  val decoratedLine = ">" ~ ws  // '>' followed by whitespace
  val textBlock = BlockParsers.block(decoratedLine, decoratedLine)
    
  recParsers.recursiveBlocks(textBlock).map(QuotedBlock(_))
}
```

* Our quoted block is a recursive structure, therefore we need to use the corresponding entry point
  `BlockParser.recursive`.
  Like the recursive entry point for span parsers, it provides access to the parser of the host language 
  that is fully configured with all extensions the user has specified.
  
* Next we create the parser for the condition that each line needs to meet to be part of the quoted block.
  In our case that is the `">"` character, optionally followed by whitespace (the `ws` parser consumes
  zero or more whitespace characters).
  
* We then create the parser for the text block based on these predicates. 
  `BlockParsers.block` is a shortcut that will parse lines until a line does not meet the predicate.
  The result is `List[String]`. See [Base Parsers for Block Elements] below for details about this method. 
  
* Finally we use the recursive parsers we got injected.
  The call to `recursiveBlocks` "lifts" the specified `Parser[List[String]]` to a `Parser[List[Block]]`

* We map the result and create a `QuotedBlock` node (which implements `Block`).
  The nested blocks we parsed simply become the children of the quoted block.

Like with span parsers, for blocks which are not recursive you can use the `BlockParser.standalone` entry point.


### Registering a Block Parser

For bundling all your Laika extensions, you need to extend `ExtensionBundle`.
In our case we only need to override the `parsers` property 
and leave everything else at the empty default implementations.

```scala mdoc:silent
object QuotedBlocks extends ExtensionBundle {

  val description: String = "Parser extension for quoted blocks"
  
  override val parsers: ParserBundle = 
    ParserBundle(blockParsers = Seq(quotedBlockParser))

}
```

Finally you can register your extension together with any built-in extensions you may use:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.markdown.github.GitHubFlavor

laikaExtensions := Seq(
  GitHubFlavor,
  QuotedBlocks
)
```

@:choice(library)
```scala mdoc:silent
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor

val extendedTransformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .using(QuotedBlocks)
  .build
```
@:@


### Base Parsers for Block Elements

Laika offers a `BlockParsers` object with convenience methods for creating a typical block parser.

One of the most common patterns is parsing a range of lines while removing any decoration that just serves
as markup identifying the block type, e.g. the `*` starting a Markdown list or the `>` starting a quotation.
And then using the parser result to continue parsing recursively, either nested blocks
or the inline elements of the block.

For these kind of block elements Laika offers the following method for convenience,
which is the utility we used in our example for parsing a quoted block:

```scala
def block (firstLinePrefix: Parser[Any], 
           linePrefix: => Parser[Any]): Parser[BlockSource]
```

It expects two parsers, one for parsing the prefix of the first line, one for parsing it for all subsequent lines.
These parsers may be identical, like in our example for the quoted block.
They are both of type `Parser[Any]` as the result will be discarded anyway.

The result of this parser is of type `List[String]` and contains all lines where the specified conditions were met,
**minus** the input consumed by the prefix parser.
The prefix parsers are not required to consume any input though, 
if the logic for a particular block does not require stripping off decoration, 
you can alternatively pass parsers that only check some pre-conditions, but leave all input for the result.

The method above always stops parsing when encountering a blank line on the input,
which is common for many types of block elements.
For cases where parsing may need to continue beyond blank lines,
there is a second overload of this method that allows this: 

```scala
def block(
  firstLinePrefix: Parser[Any],
  linePrefix: => Parser[Any],
  nextBlockPrefix: => Parser[Any]
): Parser[BlockSource] = {
```

It simply adds a third prefix parser that gets invoked on the beginning of a line following a blank line
and is expected to succeed when the line should be treated as a continuation of the block element.

Finally, there is a second utility that can be used for indented blocks:

```scala
def indentedBlock(
    minIndent: Int = 1,
    linePredicate: => Parser[Any] = success(()),
    endsOnBlankLine: Boolean = false,
    firstLineIndented: Boolean = false,
    maxIndent: Int = Int.MaxValue
): Parser[BlockSource] =
```

Like the other utility it allows to specify a few predicates. 
This method is not used for parsing Markdown's indented blocks, though, 
as Markdown has a special way of treating whitespace.


Precedence
----------

Both, block and span parsers can specify a precedence:

```scala mdoc:silent
BlockParser.recursive { recParsers =>
  ??? // parser impl here
}.withLowPrecedence
```

The default is that extension parsers have higher precedence than host language parsers, 
which means that they will be tried first and only when they fail the host language parsers will be invoked.

When setting low precedence like in the example above, this is reversed.

Note that precedence only refers to the ordering between the parsers of your extension and those of the host language.
The precedence between your extensions, in case you install multiple, is determined by the order you specify them in.

```scala
override val parsers: ParserBundle = ParserBundle(
  spanParsers = Seq(parserA, parserB)
)
```

In this case `parserB` will only be invoked when `parserA` fails.
Normally the syntax between inline constructs is different enough that the precedence does not matter.
But in some cases extra care is needed.
If, for example, you provide parsers for spans between double asterisk `**` and between single asterisk `*`,
the former must be specified first, as otherwise the single asterisk parser would 100% shadow the double one
and consume all matching input itself.


Internal Design & Performance
-----------------------------

For span parsers the parser you are passing to the `standalone` or `recursive` methods must be 
of type `PrefixedParser[Span]`, which is a subtype of the base trait `Parser[T]`. 

This is a trait implemented by all parsers with a stable prefix that can be optimized, like literal string parsers
or many other base parsers like the `someOf` or `delimiter` parsers. 
In most cases this is nothing you need to worry about, as most of the building blocks you use already implement
this trait. 
In the rare occasions where this is not the case, you might need to wrap your inline parser in a custom implementation
of the `PrefixedParser` trait.

This restriction is necessary as inline parsers cause checks to be performed on each character.
This is therefore the key hot spot for performance optimizations. 
For details see [Performance Optimizations] in the chapter about Laika's parser combinators.

For block parsers this restriction is lifted, you can pass a regular `Parser[Block]` to the registration entry points.
One reason is that optimizing block parsers is much less critical, 
as they are only invoked (more or less) after a blank line has been read, not on each input character.
Another reason is that the nature of some block elements makes this type of optimization impractical.
An underlines header for example does not have any concrete set of start characters.

But even though it is not required, if you do actually pass `PrefixedParser[Block]` it will nevertheless
get optimized under the hood, just that the impact of this optimization will be smaller than for span parsers.
