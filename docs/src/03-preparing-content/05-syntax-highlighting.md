
Syntax Highlighting
===================

Laika has its own built-in syntax highlighters (based on its parser combinators). 
Not relying on external tools for this task has several advantages:

* The highlighting is not tied to HTML output and works in the same way for EPUB or PDF output.

* The analyzed code is part of the document AST and can be processed and transformed like other
  AST nodes before rendering.
  
This manual itself is a showcase for this functionality. 
All code samples shown are highlighted by Laika's own syntax support. 
  
  
Configuration
-------------

Laika's syntax highlighting is not enabled by default, 
so that you can still choose other existing solutions if you prefer.

When using the sbt plugin it can be added to the `laikaExtensions` setting:

```scala
laikaExtensions ++= Seq(GitHubFlavor, SyntaxHighlighting)  
```

Note that you also have to add the `GitHubFlavor` extension so that you get the support for fenced code blocks.
When using reStructuredText input only the `SyntaxHighlighting` extension is needed.

When using the Library API highlighting can be activated like all other extension bundles:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor, SyntaxHighlighting)
  .build
```


Supported Languages
-------------------

Laika currently supports the following languages and formats:

* Scala
* Dotty
* Java
* Python
* JavaScript / JSX
* TypeScript / TSX
* Haskell
* HTML
* CSS
* XML
* YAML
* JSON
* HOCON
* SQL
* EBNF
* Alloy
* Dhall
* Laika Extensions

The last highlighter can be used when writing documentation about Laika itself: 
it includes highlighters for Laika's directives, substitution references and configuration headers.

More languages will be added over time (contributions are also welcome of course).


Styling
-------

The default Helium theme contains color sets for HTML, EPUB and PDF that can be overridden via the configuration API
as shown in [Theme Settings / Syntax Highlighting](../03-preparing-content/03-theme-settings.md#syntax-highlighting).

If you do not only want to specify the color scheme, but also how code categories are grouped into the color set,
you can alternatively add custom CSS.
The classes generated for code spans are:

* `comment`
* `keyword`
* `boolean-literal`
* `number-literal`
* `string-literal`
* `char-literal`
* `symbol-literal`
* `regex-literal`
* `literal-value`
* `escape-sequence`
* `substitution`
* `type-name`
* `attribute-name`
* `identifier`
* `tag-name`
* `tag-punctuation`
* `xml-dtd-tag-name`
* `xml-processing-instruction`
* `xml-cdata`
* `markup-fence`
* `markup-headline`
* `markup-emphasized`
* `markup-quote`
* `markup-link-text`
* `markup-link-target`


Integrating External Tools
--------------------------

If you want to use the available highlighters, but also need to use languages not currently supported out of the box,
you can integrate external highlighters just for those languages.

The only thing you need to ensure is that those 3rd-party tools do not try to mess with the code blocks
that have already been processed by Laika.

For `highlight.js` Laika already provides this kind of blocker, in that it adds the `nohighligting` class
to all code blocks it has already analyzed itself.

If you are using different tools you might need to customize the rendering of the `CodeBlock` AST element.
For details see [Overriding Renderers].


Adding Your Own Highlighters
----------------------------

Creating your own highlighter can potentially be quite easy, in cases where the syntax can be defined mostly
by combining Laika's building blocks for parsing string and number literals, identifiers and keywords.
For a fairly simple highlighter implementation as an example you can examine the [ScalaSyntax] source
as a starting point.

[ScalaSyntax]: https://github.com/planet42/Laika/blob/master/core/src/main/scala/laika/parse/code/languages/ScalaSyntax.scala

Once you have implemented and tested your highlighter you can add it to the built-in ones like this:

```scala
laikaExtensions ++= Seq(
  GitHubFlavor, 
  SyntaxHighlighting.withSyntax(MyHighlighter)
)  
```

See [Adding Syntax Highlighters] for details.
