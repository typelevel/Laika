
Laika
=====

Laika is a customizable and extensible toolkit for transforming lightweight text markup and template based site generation, supporting
Markdown and reStructuredText. For use with sbt or embedded in Scala applications, without the need for external tools.

It has been designed for two primary usage scenarios:

* Embedded use (e.g. web applications that allow user input in one of the supported markup syntaxes)

* As a standalone tool (e.g. as part of an sbt build that produces the project's documentation)


It is Open Source under the Apache 2.0 License.

If you wonder about the name of the project and have never heard about Laika, 
you can read about her [here][laika-wikipedia].


[laika-wikipedia]: http://en.wikipedia.org/wiki/Laika


Getting Started
---------------

The current version is published to Maven Central for Scala 2.10.x.

The sbt plugin is published to the sbt plugin repository for sbt 0.13.x.


**Using the sbt Plugin:**

Add the plugin to `project/plugins.sbt`:

    addSbtPlugin("org.planet42" % "laika-sbt" % "0.5.0")

Import its default settings in your project's `build.sbt`:

    LaikaPlugin.defaults
    
Add Markdown, reStructuredText or HTML template files to `src/docs` in your
project and run the `laika:site` task from within sbt to generate the site
in `target/docs/site`.    
    

**Using Laika Embedded:**

Adding the Laika dependency to your sbt build:

    libraryDependencies += "org.planet42" %% "laika-core" % "0.5.0"

Example for transforming from file to file:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"

Example for transforming an entire directory of markup files:

    Transform from ReStructuredText to 
      HTML fromDirectory "source" toDirectory "target"


For further information:

* Read more about standard usage in the chapters 
  [Using the sbt Plugin] and [Using Laika Embedded].

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
[API]: ../api/
[Twitter]: https://twitter.com/_planet42
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika


Features
--------

* Support for Markdown and reStructuredText as input

* Support for HTML and PrettyPrint (for debugging) as output

* Template-based Site Generation

* Support for tables of contents, convenient cross-linking between 
  documents and auto-numbering of documents and sections for all 
  supported markup formats
  
* sbt plugin, exposing all Laika features and customization hooks
  as sbt settings and tasks

* Custom Directives (tags) for templates and text markup, with type-safe
  and concise DSL for their declaration

* Customization hooks for renderers (based on simple partial 
  functions that change the output for specific node types)

* Document Tree Rewriting (hooks for modifying the document AST, consisting
  of case classes, between parsing and rendering, based on simple partial 
  functions)

* Simple contracts and APIs for adding new parsers and renderers

* A set of generic and fast parser combinators for common tasks in
  markup parsing that are not based on the commonly used (and often slow)
  regex parsers 

* Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)

* Parallel processing of parsers and renderers

* More than 900 tests


Road Map
--------

* New renderer for PDF (0.6) 

* New renderer for epub (0.7)

* Integrated support for syntax highlighting in source code (0.8)

* Various Markdown extensions: GitHub Markdown, MultiMarkdown, YAML headers, etc. (0.9)


After these four releases priorities depend on user demand, therefore
no decision has been made yet for these other ideas:

* AsciiDoc support

* Java API / Gradle plugin

* A command line interface


Release History
---------------

* __0.5__ (Jan ?, 2014):

    * New sbt plugin, exposing all Laika features and customization hooks as sbt tasks and settings
    * New option to merge multiple input directories into a tree structure with a single root,
      allowing to keep reusable styles or templates ("themes") separately
    * New option to use Markdown and reStructuredText markup in the same input tree, including
      cross-linking between the two formats
    * Move to a multi-project build and rename the main artifact from `laika` to `laika-core`
    * Upgrade to ScalaTest 2.0 and sbt 0.13
    * Drop support for Scala 2.9.x

* __0.4__ (Nov 22, 2013):

    * Template-based site generation
    * Support for tables of contents, convenient cross-linking between documents 
      and autonumbering of documents and sections for all supported markup formats
    * Custom Directives for templates and text markup
    * Document Fragments that can be rendered separately from the main document content
    * New API for batch processing for parse, render or full transform operations
    * Parallel processing of parsers and renderers 
      
* __0.3__ (Aug 3, 2013):

    * Support for most of the standard directives and text roles of the reStructuredText reference
      parser (admonitions, `figure`, `image`, `code`, `raw` and many more)
    * Now integrates the official Markdown test suite (and many fixes to make it pass)
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


  
