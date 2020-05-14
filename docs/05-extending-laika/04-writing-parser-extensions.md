
Writing Parser Extensions
=========================

Implementing a custom parser is one of two possible ways to extend the syntax of a text markup language.

The second option is @ref:(Implementing Directives) and in many cases that approach offers more convenience.
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

First, all parsers build on top of @:ref(Laika's Parser Combinators).
Having its own implementation helps with keeping all functionality tightly integrated and adding
some optimizations useful for the specific use case of markup parsing right into the base parsers.

Second, since the parsers we discuss in this chapter all produce AST nodes, 
it might help to get familiar with @:ref(The Document AST) first.


Span Parsers
------------

Span parsers participate in inline parsing. 
Examples for built-in span parsers are those for emphasized text, literal spans or links.

Adding a span parser to an existing text markup language requires two steps:

* Write the actual implementation of the parser as a type of `PrefixedParser[Span]`.
* Add this declaration to an `ExtensionBundle`.

Let's go through this step by step with an example. 
We are going to build a simple span parser for a ticket reference in the form of `#123`.


### The Parser Implementation

We require a mandatory `#` symbol followed by one or more digits.

```scala
import laika.ast._
import laika.parse.implicits._

val ticketParser: PrefixedParser[Span] = 
  ("#" ~> someOf(CharGroup.digits)).map { num =>
    SpanLink(Seq(Text("#" + num)), ExternalTarget(s"http://our-tracker.com/$num"))
  }
```

* We first look for a literal `"#"`.
* We then require at least one digit. 
  The `someOf` parser covers this as it means "one or more of the specified characters".
* We combine the two parsers with `~>` which means: 
  "concatenate the two parsers, but only keep the result of the right one".
  See @:ref(Laika's Parser Combinators) for details.
* We map the result and create a `SpanLink` node (which implements `Span`).
  We use the literal input as the link text and then build the URL as an external target.
  
This first example hard-codes the base URL. If your parser extension is for internal use, this may not be a problem.
But if you require a configurable base URL, we later show an enhanced example that has access to the configuration.


### Registering the Parser

For bundling all your Laika extensions, you need to extend `ExtensionBundle`.
In our case we only need to override the `parsers` property 
and leave everything else at the empty default implementations.

```scala
object TicketSyntax extends ExtensionBundle {

  override val parsers: ParserBundle = ParserBundle(
    spanParsers = Seq(SpanParser.standalone(tickerParser))
  )

}
```

The `SpanParser.standalone` method can be used in cases where your parser does not need access to the parser
of the host language for recursive parsing.

Finally you can register your extension together with any built-in extensions you may use:

```scala
TODO - plugin + library
```


### Access to Configuration

This will enhance the previous example by making the base URL configurable.

Access to configuration or other AST nodes is not possible in the parsing phase itself, 
as each parser executes in isolation and multiple input documents are processed in parallel.

But your parser can return an instance that implements `SpanResolver` instead of directly producing a link.
Such a resolver will then participate in the AST transformation phase where its `resolve` method will be invoked:

```scala
case class TicketResolver (num: String, options: Options = NoOpt) extends SpanResolver {

  def resolve (cursor: DocumentCursor): Span = {
    cursor.config.get[String]("ticket.baseURL").fold(
      error => InvalidElement(s"Unable to read base URL for tickets: $error", "#" + num).asSpan,
      baseURL => SpanLink(Seq(Text("#" + num)), ExternalTarget(s"$baseURL$num"))
    )
  }
}
```

The `DocumentCursor` passed to the `resolve` method provides access to the project configuration which we use
in this example, but also to all ASTs of the input tree, including other documents. 
It can therefore be used for advanced functionality like producing table of contents.

The API of the `cursor.config` property is documented in @:api(laika.config.Config).

In our case we expect a string, but we also need to handle errors now, as the access might fail
when the value is missing or it's not a string. 
We return an `InvalidElement` for errors, which is a useful kind of AST node as it allows the user to control the
error handling. 
The presence of such an element will by default cause the transformation to fail with the provided error message
shown alongside any other errors encountered.
But users can also switch to a "visual debugging" mode by rendering all errors in place. 
See [TODO - link] for details. 

The `options` property of our implementation is a mandatory property of each `Span` or `Block` element.
Options are a generic hook that allows users to add an id or multiple styles to a node.
It's always best to have an empty default argument like in our example.

With this change in place, the user can now provide the base URL in the builder of the `Transformer`:

```scala
TODO - plugin + library
```

The original ticket parser then only needs to be adjusted to return our resolver instead:

```scala
val ticketParser: PrefixedParser[Span] = 
  ("#" ~> someOf(CharGroup.digits)).map(TicketResolver(_))
```

The registration steps are identical to the previous example.


### Detecting Markup Boundaries

The examples so far came with a major simplification to keep the code concise.
The logic we implemented would also match on a link embedded in markup for example: `http://somwhere.com/page#123`.
In such a case we would not want the `#123` substring to be recognized as a ticket link.

For this reason text markup languages usually define a set of rules which define how to detect markup boundaries.
In reStructuredText these are defined in a very clear and strict way ([TODO - link]), 
while in Markdown the definition is somewhat more fuzzy.

Laika's parser combinators come with convenient helpers to check conditions on preceding and following characters
without consuming them. Let's fix our parser implementation:

```scala
val ticketParser: PrefixedParser[Span] = {
  val letterOrDigit: Char => Boolean = { c => 
    Character.isDigit(c) || Character.isLetter(c)
  }
  val delim = delimiter("#").prevNot(letterOrDigit)
  val ticketNum = someOf(CharGroup.digits)
  val postCond = nextNot(letterOrDigit)
  
  (delim ~> ticketNum <~ postCond).map(TicketResolver(_))
}
```

Instead of just using a literal parser for the `"#"` we use the `delimiter` parser which comes with convenient
shortcuts to check conditions before and after the delimiter.

The end of our span has no explicit delimiter, so we use a standalone `nextNot` condition.

Checking that neither the preceding nor the following character is a letter or digit increases the likeliness
we only match on constructs the user actually meant to be ticket references.


### Recursive Parsing

In our example we used the `SpanParser.standalone` method for registration.
In cases where your parser needs access to the parser of the host language for recursive parsing
we need to use the `SpanParser.recursive` entry point instead:

```scala
SpanParser.recursive { recParsers =>
  ('*' ~> recParsers.recursiveSpans(delimitedBy("*"))).map(Emphasized(_))
} 
```

This parser parses markup between two asterisk.
It also detects and parses any span kinds provided by either the host markup language or other installed extensions
between these two delimiters.
The result from the parser obtained with the call to `recursiveSpans` is always of type `Seq[Span]`.

Only this entry point gives you access to a parser that is fully configured with all extensions the user has specified.
This parser needs to be injected as you would otherwise hard-code a concrete, fixed set of inline parsers.


### Precedence

Finally the `SpanParser` registration builder allows to specify the precedence:

```scala
SpanParser.recursive { implicit recParsers =>
  ??? // parser impl here
}.withLowPrecedence
```

The default is that extension parsers have higher precedence than host language parsers, 
which means that they will be tried first and only when they fail the host language parsers will be invoked.

When setting low precedence like in the example above, this is reversed.

Note that precedence only refers to the ordering between the parsers of your extension and those of the host language.
For the precedence between your extensions, in case you install multiple, is determined by the order you specify them in.

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


### Performance Optimizations

The parser you are passing to the `standalone` or `recursive` methods must be of type `PrefixedParser[Span]`,
which is a subtype of the base trait `Parser[T]`. 

This is a trait implemented by all parsers with a stable prefix that can be optimized, like literal string parsers
or many other base parsers like the `someOf` or `delimiter` parsers. 
In most cases this is nothing you need to worry about, as most of the building blocks you use already implement
this trait. 
In the rare occasions where this is not the case, you might need to wrap your inline parser in a custom implementation
of the `PrefixedParser` trait.

This restriction is necessary as inline parsers cause checks to be performed on each character.
This is therefore the key hot spot for performance optimizations. 
For details on this optimization see [TODO - link]. 


Block Parsers
-------------

The API for creating block parsers is very similar to that for span parsers.
There is a variant for a parser that does not recursively parse nested blocks:

```scala
BlockParser.standalone {
  // your parser impl
}    
```

Or the parser is allowed to contain nested blocks from the host language
or any installed extensions, your can ask for the recursive parser to be provided:

```scala
BlockParser.recursive { recParsers =>
  // your parser impl
} 
```

One difference is that the block parser API does not require instances of `PrefixedParser[Block]`,
a plain `Parser[Block]` suffices. If your parser does implement the `PrefixedParser` trait,
it will still get optimized like span parsers, but for some typical markup blocks like underlined
headlines, providing a finite, reasonably small set of possible start characters is not feasible.

The performance hit for non-optimized block parsers is much smaller though than for span parsers,
as it is only invoked (more or less) after blank line has been read, not on each input character. 

Laika also offers a `BlockParsers` object with convenience methods for creating
a typical block parser. This is the signature of the first one:

```scala
def block (firstLinePrefix: Parser[Any], 
           linePrefix: Parser[Any], 
           nextBlockPrefix: Parser[Any]): Parser[List[String]]
```

It allows to parse a block based on three simple conditions, provided in form of
the three parser parameters: detecting the first line of a block, any subsequent
line and finally whether the block continues after a blank line has been seen.
Often a blank line marks the end of a block, but there are exceptions, like code
blocks or list items that span multiple paragraphs.

The following code is an example for a parser for quoted blocks, decorated
by a `>` character at the start of each line:

```scala
BlockParser.recursive { recParsers =>
  
  val textAfterDeco = TextParsers.ws       // any whitespace
  val decoratedLine = ">" ~ textAfterDeco  // '>' followed by whitespace
  val afterBlankLine = Parsers.failure("blank line ends block")
  
  val textBlock = BlockParsers.block(decoratedLine, decoratedLine, afterBlankLine)
  
  recParsers.recursiveBlocks(textBlock).map(QuotedBlock(_, Nil))
}
```
 
This implementation uses the `BlockParsers` helper. It passes just the same `decoratedLine`
parser for the first and subsequent lines.

Finally we pass the `textBlock` parser to the recursive parsers which lift our
`Parser[String]` to a `Parser[Seq[Block]]` and the result of that parser is 
passed to the `QuotedBlock` AST node.

Finally there is a second utility that can be used for indented blocks:

```scala
def indentedBlock (minIndent: Int = 1,
          linePredicate: => Parser[Any] = success(()),
          endsOnBlankLine: Boolean = false,
          firstLineIndented: Boolean = false,
          maxIndent: Int = Int.MaxValue): Parser[String]
```

Like the other utility it allows to specify a few predicates. This method
is not used for parsing Markdown's indented blocks, though, as Markdown has
a very special way of treating whitespace.

