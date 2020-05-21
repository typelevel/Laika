
Adding Syntax Highlighters
==========================

Laika has its own built-in syntax highlighters (based on its parser combinators). 

This manual itself is a showcase for this functionality. 
All code samples shown are highlighted by Laika's own syntax support. 

This chapter explains the general design goals as well as the basic building blocks for adding your own highlighters.


Design Goals
------------

* **Everything is pluggable and customizable.**

  This includes making it easy to add support for additional languages as well as ignoring the built-in
  highlighter altogether and use external tools like `highlight.js` instead.

* **The mechanism is not tied to HTML output.**

  The analyzed code is part of the document AST and can be processed and transformed like other
  AST nodes before rendering, making it a part of the common processing pipeline and not a bolt-on step after rendering,
  that would require the availability of JavaScript for the rendered output for example.
  
  This way the highlighted nodes can be rendered in any output format like EPUB, PDF or any 3rd-party format.

* **Focus on display comfort over validation.**

  It's primary use case is documentation or blogs, where the highlighter is supposed to improve the readability
  of the code.
  It is not designed to serve as a basis for code editors or include error highlighting and messages.
 
  Therefore it is also beyond the scope of this utility to detect larger constructs like entire method signatures.
  In some languages like Java this would come with a lot of ambiguity as a method declaration does not start 
  with a keyword like `def`.
  It would require to get much closer to having a full blown lexer and analyzer for the language than this
  utility is designed for.

  As a consequence its syntax definitions are more similar in scope and precision to those of lightweight tools
  like `highlight.js` than to the lower-level nature of sublime-text syntax definitions for example.
  If validation is required the recommended approach in Laika is to rely on other tools like MDoc for Scala
  validation and then only use Laika for highlighting the result.
  
* **Avoiding the regular expression mazes of other tools.**

  It offers a lot of higher level building blocks for assembling highlighters for common constructs like 
  string or number literals which are often quite similar between languages.
  The definitions then become more of a declarative list of supported constructs.
  
  But even in cases where a custom parser is required, it encourages the use of Laika's built-in parser combinator 
  framework, 
  which is a more composable and type-safe way to define a parser than a cryptic (and stringly) regular expression.
  
  None of the built-in parsers for text markup, HOCON, CSS or syntax highlighting use any regular expressions.


Prerequisites
-------------

The content of this chapter builds on top of concepts introduced in other chapters.

First, even though Laika's highlighting support defines a lot of higher level building blocks that
allow for a mere declarative assembling of the supported constructs for a language,
some additions of custom parsers might be necessary, depending on the complexity of the target language.
In that case it's recommended to use [Laika's Parser Combinators].

Second, highlighters produce a list of `CodeSpan` nodes which is one node type of Laika's document AST. 
Even though this is usually the only node type you deal with, 
it might help to get familiar with [The Document AST] first.


The SyntaxHighlighter Trait
---------------------------

These are the two abstract methods a new highlighter needs to implement:

```scala
def language: NonEmptyList[String]

def spanParsers: Seq[CodeSpanParser]
```

* The `language` property holds one or more names for the language that will be used to identify the language
  when syntax like fenced code blocks are used.
  
  Concretely this means that if you specify `python` and `py` as the names for example, 
  the implementation will be used for any fenced code block that starts with either @:lit(```py) or @:lit(```python).
  
  It's not tied to Markdown syntax though. 
  reStructuredText has its own way of specifying a code block with language identifier, 
  and the highlighter will be used for those, too.
  
* The `spanParsers` property holds the actual parsers.
  Like with markup parsers they can be defined independently and then simply get listed in this trait.

Let's start by looking at how you can assemble these span parsers by using some of the building blocks for 
common language constructs.

We later look at how to add other constructs based on custom parsers.


Using Existing Building Blocks
------------------------------

The building blocks shown in this section are meant to be used as shortcuts.
Whenever they do not provide the exact syntax option that you need, 
you can always fall back to [Defining Custom Parsers].

Our code examples in this section define highlighters for a Scala-ish language, 
but omit many details for the sake of brevity.
You can examine the source code of Laika's built-in highlighters for more complete examples.


### String Literals

There are shortcuts for defining single- and multi-line strings together with escape sequences:

```scala
val spanParsers: Seq[CodeSpanParser] = Seq(
  StringLiteral.multiLine("\"\"\""),
  StringLiteral.singleLine('"').embed(
    StringLiteral.Escape.unicode ++ StringLiteral.Escape.char
  )
)
```

The `multiline` and `singleLine` builders both require to specify the delimiter.
When the start and end delimiter are identical you can use the single-argument overload.

For a single line string we also define valid escape sequences which will be used
to detect nested spans. 
These are existing building blocks, too.

There are additional escape options like octal and hex, as well as building blocks to define substitutions,
e.g. Scala's `s"some $ref"` syntax.


### Character Literals

Character literals have fewer options than string literals, but are otherwise quite similar:

```scala
val spanParsers: Seq[CodeSpanParser] = Seq(
  CharLiteral.standard.embed(
    StringLiteral.Escape.unicode ++ StringLiteral.Escape.char
  )
)
``` 

The `standard` builder uses single quotes as delimiters, but you can specify a different one if required.
Like with our string literal example, we define two kinds of escapes that can occur in a character literal.
 

### Numeric Literals

There are shortcuts for the most common types of decimal, hex, octal and binary number literals:

```scala
val spanParsers: Seq[CodeSpanParser] = Seq(
  NumberLiteral.hex
    .withUnderscores
    .withSuffix(NumericSuffix.long),
  NumberLiteral.decimalInt
    .withUnderscores
    .withSuffix(NumericSuffix.long | NumericSuffix.float)
)
```

Here we are defining two types of number literals: hexadecimal and decimal integers.

In both cases we allow the use of underscores as separators (e.g. `1,000,000`).

We also define which kinds of type indicators we allow as a suffix and again use 
existing parsers for widely used syntax (`NumericSuffix.long` parses `L` or `l` for example).


### Identifiers

This is how the identifier category is defined for the Scala highlighter:

```scala
val identifier = Identifier.alphaNum
  .withIdStartChars('_','$')
  .withCategoryChooser(Identifier.upperCaseTypeName)
```

It expects any alphanumerical identifier that is also allowed to have `_` or `$` as a start character.

We then pass a function as a "category chooser" that assigns a category based on the parsed identifier.
It is a very simple function that serves as a relatively useful approximation:
it picks `CodeCategory.TypeName` when the identifier starts with an uppercase letter,
otherwise it assigns `CodeCategory.Identifier`.

This is obviously not always correct, but good enough for most use cases.
As discussed in the design goals, this highlighting utility does not aim to get close to defining
full lexers and parsers, and assigning the category correctly in all cases would require full context
of the parsed construct. 
Many highlighters you might be using with other tools are equally pragmatic.


### Keywords

Keywords can simply be listed as string literals:

```scala
val spanParsers: Seq[CodeSpanParser] = Seq(
  Keywords("abstract", "case", "catch", "class", "def")
)
```

It detects word boundaries so that it will not match on sub-strings.

By default the parser will assign `CodeCategory.Keyword` to any matching string.
If you want to use literal matches, but with a different code category, there is an alternative constructor:

```scala
Keywords(CodeCategory.BooleanLiteral)("true", "false")
```


### Comments

Shortcuts exist for defining single- and multi-line comments:

```scala
val spanParsers: Seq[CodeSpanParser] = Seq(
  Comment.singleLine("//"),
  Comment.multiLine("/*", "*/")
)
```

The code should be self-explanatory here, we simply define the delimiters for the comment syntax of our language.
The single-line variant terminates at the end of the line.


Defining Custom Parsers
-----------------------

The basic building blocks presented in the previous section help a lot with reducing boilerplate, 
but in most cases you end up with the need to have at least a handful of hand-written parsers, too.

For a little example, let's implement a highlighter for Scala's backtick identifiers (e.g. `` `tag-name` ``)
which is quite straightforward, but not included in the reusable builders since it's not a very common construct:

```scala
import laika.parse.builders._
import laika.parse.implicits._

val backtickId: CodeSpanParser = CodeSpanParser(CodeCategory.Identifier) {
    (oneOf('`') ~ anyNot('\n', '`') ~ oneOf('`')).source
  }
```

In the first line of the definition we specify the category that should be assigned to the result of the parser.

The next line is a standard parser definition using some of the building blocks of [Laika's Parser Combinators].
We simply expect a backtick as the start and end delimiter and allow anything in-between apart from newline or
backtick characters.

The call to `.source` is a convenient shortcut that discards the result of the three parsers and instead
provides the source of the entire input by those three parsers as the result. 
This is often preferable when all we would do with the three results would be concatenate them back to one string.

The code parser we now defined can then be registered in our `spanParsers` sequence like in all other examples.

The entry point above is just one option.
It's only appropriate if we provide a string parser and want the same category assigned to the entire string.

There is an alternative factory method, where we specify a `PrefixedParser[Seq[CodeSpan]]` 
where we have full control over how we slice the input into multiple categories.


Embedding other Languages
-------------------------

In some cases a language or format may have regions that require a highlighter for a different syntax.
A classic example is HTML which may have CSS syntax inside a `<style>` tag and JavaScript syntax inside
a `<script>` tag.

Laika comes with a parser builder that helps with the creation of such a parser.

Let's pick CSS in HTML as an example and show a simplified definition for a such a style tag:

```scala
val styleTagParser: PrefixedParser[Seq[CodeSpan]] = {
  val cat = CodeCategory.Tag.Name
  val bodyAndEndTag = EmbeddedCodeSpans.parser(delimitedBy("</style>"), CSSSyntax)
  (literal("<style>") ~> bodyAndEndTag).map { css =>
    CodeSpan("<style>", cat) +: css :+ CodeSpan("</style>", cat)
  }
``` 

The key here is the third line.
We define a region to parse with `delimitedBy("</style>")` which consumes text until it sees the specified
end delimiter. See [Delimiters] for details.
And we specify the syntax to switch to until we reach that delimiter, in this case the existing `CSSSyntax`

The result (the `css` param) is of type `Seq[CodeSpan]`, containing the spans detected by the CSS highlighter.
We then wrap the code spans for the start tag and end tag around it for the final result.

One of the simplifications to keep the example simple is that we assign the category `CodeCategory.Tag.Name`
to the entire tag, including the angle brackets.
Normally you would keep them separate under `CodeCategory.Tag.Punctuation`.


Parser Precedence
-----------------

The precedence rules are identical to those for declaring markup parsers.

The parser precedence is determined by the order you specify them in.
This means they will be "tried" on input in that exact order.
The second parser in the list will only be invoked on a particular input when the first fails, and so on.

This is the logical execution model only, the exact runtime behaviour may differ due to performance optimizations,
but without breaking the guarantees of respecting the order you specify.

In many cases the difference in the syntax between constructs is high enough that the precedence does not matter.
But in some cases extra care is needed.

If, for example, you provide a parser for a span delimited by one or two occurrences of the same character,
the two-character version must be specified first, as otherwise the parser with the single-character delimiter 
would 100% shadow the double one and consume all matching input itself, unless it contains a guard against it.

Similarly, parsers like those for identifiers which do not have a limited set of start delimiters 
usually need to occur near the bottom of the list of parsers you register.


Registering a Highlighter
-------------------------

First you have to assemble all the parsers in a `SyntaxHighlighter` implementation:

```scala
object FooHighlighter extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.one("foo")
  
  val spanParsers: Seq[CodeSpanParser] = Seq(
    // all your definitions here
  )

}
```

Finally, like all other types of extensions, the highlighter needs to be registered with an `ExtensionBundle`:

```scala
case object MyExtensions extends ExtensionBundle {
  
  override def parsers: ParserBundle = ParserBundle(
    syntaxHighlighters = Seq(
      FooHighlighter
    )
  )
}
```

You can bundle multiple highlighters in a single instance.

Finally you can register your extension together with any built-in extensions you may use:

```scala
TODO - plugin + library
```
