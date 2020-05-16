
New Markup or Output Formats
============================

All the other chapters in the "Extending Laika" section of the manual deal with a customization option 
that at some point becomes part of an `ExtensionBundle`. 
As @:ref(Anatomy of the API) showed, it is one of the major API hooks for building a transformer:

[TODO - copy diagram from Anatomy of the API]

While an `ExtensionBundle` is about enhancing functionality for existing input and output formats,
this chapter finally is about actually adding new formats. 
Therefore it deals with the other two types in the diagram: `MarkupFormat` and `RenderFormat[FMT]`. 


Implementing a Markup Format
----------------------------

Laika currently supports two markup formats out of the box: Markdown and reStructuredText.
Having two formats from the beginning greatly helped in shaping a document model
that is not tied to the specifics of a particular text markup language.

Making it as straightforward as possible to add support for more formats like ASCIIDoc or Textile 
was one of Laika's initial design goals.


### Prerequisites

The content of this chapter builds on top of concepts introduced in other chapters,
therefore it's recommended to read those first.

First, all parsers build on top of @:ref(Laika's Parser Combinators).
Having its own implementation helps with keeping all functionality tightly integrated and adding
some optimizations useful for the specific use case of markup parsing right into the base parsers.

Second, since block and span parsers that together provide the markup implementation all produce AST nodes, 
it might help to get familiar with @:ref(The Document AST) first.

Finally, @:ref(Writing a Parser Extension) covers a lot of ground relevant for adding a new format, too.
It walks through the sample implementation of a span parser.

The main difference is merely that a block or span parser serving as an extension for existing markup languages 
will be registered with an `ExtensionBundle`, 
while the parsers for a new format need to be registered in a `MarkupFormat`.


### The MarkupFormat Trait

The contract a markup implementation has to adhere to is captured in the following trait:

```scala
trait MarkupFormat {

  def fileSuffixes: Set[String]

  def blockParsers: Seq[BlockParserBuilder]
  
  def spanParsers: Seq[SpanParserBuilder]

  def extensions: Seq[ExtensionBundle]
  
}
```

These are the four abstract method each parser has to implement.    
    
* The `fileSuffixes` method should simply return the set of supported file suffixes (without the '.'). 
  For Markdown this would be `Set("md", "markdown")` for example. 
  It is not recommended to support generic suffixes like `txt` as this could lead to conflicts with other parsers.
  
  Laika supports a setup where input directories may contain source files written in different markup languages.
  In this case the format detection is solely based on the file suffix.

* The `blockParsers` and `spanParsers` collections provide the definitions for the actual markup parsers. 
  These are the same types you've already seen when registering parsers as extensions.

  It may sound too good to be true, but the task of assembling the building blocks of a markup language
  in Laika is indeed merely declarative. 
  The parsers for individual elements like lists or links can almost always be implemented in isolation
  with little or no interdependencies.
  
  @:ref(Writing a Parser Extension) contains a walk-through of a span parser implementation.
  You can also browse the code for Laika's Markdown and reStructuredText support for more examples. 
  
* The `extensions` collection allows to add functionality beyond just markup parsing.
  It uses the same `ExtensionBundle` trait that exists for users to customize a transformation.
  
  Whenever a format that defines extensions is used, they are merged with the user-supplied extensions.
  This collection may very well remain empty for some formats.

Finally there are three concrete methods that may be overridden if required:

```scala
def description: String = toString

def escapedChar: Parser[String] = TextParsers.oneChar

def createBlockListParser (parser: Parser[Block]): Parser[Seq[Block]] = 
  (parser <~ opt(blankLines)).rep
```

* The `description` method allows to add details for tooling and logging.

* The `escapedChar` method controls the parsing of an escape sequence (the character after a backslash). 
  The default implementation accepts any character.

* Finally, `createBlockListParser` controls how a parser for a single block is turned into a parser for
  a sequence of blocks.
  The default implementation just skips blank lines between the blocks and repeats the same parser.
  The parser passed to this method already includes all the (merged) block parsers this trait has specified 
  as well as any extensions a user might have installed.


### Parser Precedence

The parser precedence is determined by the order you specify them in.
This means they will be "tried" on input in that exact order.
The second parser in the list will only be invoked when the first fails, and so on.

This is the logical execution model only, the exact runtime behaviour may differ due to performance optimizations,
but without breaking the guarantees of respecting the order you specify.

Normally the syntax between markup constructs is different enough that the precedence does not matter.
But in some cases extra care is needed.
If, for example, you provide parsers for spans between double asterisk `**` and between single asterisk `*`,
the former must be specified first, as otherwise the single asterisk parser would 100% shadow the double one
and consume all matching input itself, unless it contains a guard against it.
