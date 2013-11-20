
Laika
=====

Laika is a customizable and extensible toolkit for transforming lightweight markup languages to various types of output formats, written in Scala. 

It has been designed for two primary usage scenarios:

* Embedded use (e.g. web applications that allow user input in one of the supported markup syntaxes)

* As a standalone tool (e.g. as part of an sbt build that produces the project's documentation)


It is Open Source under the Apache 2.0 License.

If you wonder about the name of the project and have never heard about Laika, 
you can read about her [here][laika-wikipedia].


[laika-wikipedia]: http://en.wikipedia.org/wiki/Laika


Getting Started
---------------

The current version is published to Maven Central for Scala 2.10.x, 2.9.3 and 2.9.2.


Adding Laika to your project with sbt:

    libraryDependencies += "org.planet42" %% "laika" % "0.3.0"


Example for transforming from file to file:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"


For further information:

* Read more about standard usage in the chapter [Transformation Basics].

* Try out Laika with the [Web Tool].

* Browse the source on [GitHub].

* Browse the [API].

* Follow on [Twitter] for release announcements.

* Create [Issues] here on GitHub for bug reports or enhancement requests.

* Ask questions on [Stackoverflow], tagging with Laika and Scala.
 
There will also be a dedicated forum in case of growing interest in Laika.

This manual is written in Markdown and transformed by Laika. Its source
is included in the repository inside the `docs` folder.


[Web Tool]: http://www.planet42.org/laika/webtool
[GitHub]: https://github.com/planet42/Laika
[API]: api/
[Twitter]: https://twitter.com/_planet42
[Transformation Basics]: basics.html
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika


Features
--------

* Support for Markdown and reStructuredText as input

* Concise and type-safe API for all reStructuredText extensibility options (directives, text roles)

* Support for HTML and PrettyPrint (for debugging etc.) as output

* Customization Hooks for Renderers

* Document Tree Rewriting

* Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)


Road Map
--------

* Template-based site generation (0.4)

* sbt Plugin (0.5)

* New renderers for DocBook, PDF (0.6) and epub (0.7)


After these four releases priorities depend on user demand, therefore
no specific order has been set for these other ideas:

* Various Markdown extensions (GitHub Flavored Markdown, Multimarkdown, etc.)

* A few users asked for AsciiDoc support (although they don't have a real spec which makes it difficult)

* A Java API

* A command line interface

* Luna, a new markup syntax that aims to combine the simplicity and readability of Markdown
  with the power and extensibility of reStructuredText


Design Principles
-----------------

* Fully decouple the aspects of input/output, parsing, rendering and document tree rewriting, 
  making each of these steps pluggable.
  
* Provide a very convenient and simple high level API for common transformation tasks.

* Build a generic document tree model that does not reflect specifics of a particular 
  supported markup language like Markdown or reStructuredText.

* Allow for easy modification of the rendering for a particular node type only, without
  the need to sub-class or modify an existing renderer.
  
* Allow customization of rewrite rules for transforming the document tree before rendering
  (e.g for resolving cross-references).

* Provide concise and type-safe extension APIs for extensible markup definitions (e.g.
  for directives and text roles in reStructuredText)
  
* Create the built-in parsers with the Scala parser combinators, providing efficient and
  reusable base parsers that encapsulate requirements common to all lightweight markup languages,
  while keeping the basic contract for plugging in a new parser function as simple and generic as 
  `Input => Document`, so that other parser frameworks or tools can be used, too.
  
* Designed for robustness: Laika has more than 600 tests, it is protected against malicious
  or accidentally malformed input like endless recursion (a type of guard most other text markup 
  parsers do not include) and parsers like the URI parser are based on the actual relevant RFCs
  (and not just a rough approximation like in many other parsers). 


### Internal Architecture

![Internal Architecture](img/architecture.png)

This diagram shows the main building blocks of the toolkit:

* `Input` and `Output` are just little IO abstractions, so that the other parts of the system
  do not need to deal with the low-level details of where to read from and to write to. The toolkit
  supports files, strings and streams, writers and readers from `java.io`.
  
* `Parse` represents the actual parsing step, a pluggable function of type `Input => Document`.
  Supported out of the box are Markdown and reStructuredText. Other parsers can easily be added
  to the system and they do not need to be based on the SDK's parser combinators like the built-in
  parsers. The extensibility options of existing parsers depend on the capabilities of the 
  respective markup, e.g. reStructuredText has the concept of directives and text roles as
  extension hooks, therefore the extension APIs are parser-specific. 
  
* `Rewrite` is a customizable step for transforming the document tree model before rendering.
  There is always a default rewrite step involved, that looks for nodes in the model that need
  to be resolved, like link references, footnote references, etc. But custom rules can be added
  based on a partial function that deals with specific node types only.
  
* `Render` is the final render step. Currently supported out of the box are HTML and PrettyPrint,
  the latter visualizing the document tree for debugging purposes. Planned for future releases
  are support for PDF, DocBook and epub. Like with the rewrite step, the entire renderer can be replaced
  by a custom one, or an existing one can customized based on a partial function that deals with
  specific node types only.

  
Release History
---------------

* __0.3__ (Aug 3, 2013):

    * Support for most of the standard directives and text roles of the reStructuredText reference
      parser (admonitions, `figure`, `image`, `code`, `raw` and many more)
    * Now integrates the official Markdown test suite (any many fixes to make it pass)
    * Now integrates a test for transforming the full reStructuredText specification (which, of
      course, is written in reStructuredText) and many fixes to make it pass
    * Adds the renderer option `HTML.unformatted` for terse output without indentation or whitespace
      (often desirable when writing the rendered document to a database for example)
    * Adds a new [Web Tool] to try out Laika online
    * General cleanup of parser implementations and alignments between Markdown and reStructuredText
      parsers, making both of them much more robust

* __0.2__ (May 7, 2013):

    * Support for reStructuredText (full specification)
    * Concise and type-safe API for all reStructuredText extensibility options (directives, text roles)
    * New document tree nodes for tables, footnotes, citations, definition lists, internal links,
      comments, system messages, invalid elements
    * Render hints for document tree nodes in the form of the new Customizable trait

* __0.1__ (Jan 12, 2013): 

    * Support for Markdown as input
    * Support for HTML and PrettyPrint as output
    * Customization hooks for renderers
    * Document tree rewriting
    * Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)
    * Generic base traits for markup parser implementations


  
