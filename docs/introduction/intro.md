
Laika
=====

Laika is a customizable and extensible toolkit for transforming lightweight text markup 
and template based site and e-book generation.

It has been designed for two primary usage scenarios:

* Embedded use (e.g. web applications that allow user input in one of the supported markup syntaxes)

* As a standalone tool (e.g. using Laika's sbt plugin to produce the project's documentation)

Laika does not require the installation of any external tools.

It is Open Source under the Apache 2.0 License.

If you wonder about the name of the project and have never heard about Laika, 
you can read about her [here][laika-wikipedia].


[laika-wikipedia]: http://en.wikipedia.org/wiki/Laika


Getting Started
---------------

The current version is published to Maven Central for Scala 2.13 and 2.12.

The sbt plugin is published to the sbt plugin repository for sbt 1.x.

The final release for Scala 2.11 had been 0.10.0, 
the final release for Scala 2.10 and sbt 0.13 was 0.7.0.


### Using the sbt Plugin

Add the plugin to `project/plugins.sbt`:

    addSbtPlugin("org.planet42" % "laika-sbt" % "0.12.0")

Enable the plugin in your project's `build.sbt`:

    enablePlugins(LaikaPlugin)

Add Markdown, reStructuredText or HTML template files to `src/docs` in your
project and run the `laikaSite` task from within sbt to generate the site
in `target/docs/site`.    


### Using the Library API

Adding the Laika dependency to your sbt build:

    libraryDependencies += "org.planet42" %% "laika-core" % "0.12.0"

Example for transforming Markdown:

    Transformer
      .from(Markdown)
      .to(HTML)
      .build
      .transform("hello *there*")

Example for transforming ReStructuredText:

    Transformer
      .from(ReStructuredText)
      .to(HTML)
      .build
      .transform("hello *there*")
    
For file/stream IO, parallel processing and/or EPUB support, based on cats-effect, 
add the laika-io module to your build:

    libraryDependencies += "org.planet42" %% "laika-io" % "0.12.0"  
    
Example for transforming an entire directory of markup files to a single EPUB file:

    implicit val processingContext: ContextShift[IO] = 
      IO.contextShift(ExecutionContext.global)
      
    val blockingContext: ContextShift[IO] = 
      IO.contextShift(ExecutionContext
        .fromExecutor(Executors.newCachedThreadPool()))
    
    val transformer = Transformer
      .from(Markdown)
      .to(EPUB)
      .using(GitHubFlavor)
      
    laika.io.Parallel(transformer)
      .build(processingContext, blockingContext)
      .fromDirectory("src")
      .toFile("hello.epub")
      .transform
      .unsafeRunSync()       

When using Laika's PDF support you need to add one more dependency to your build:

    libraryDependencies += "org.planet42" %% "laika-pdf" % "0.12.0"

The example for how to transform a directory of input files into a PDF file looks
the same as the EPUB example, apart from swapping `EPUB` for `PDF`    


### Other Resources

* Read more about standard usage in the chapters  
  [Using the sbt Plugin][../using-laika/sbt.md:Using the sbt Plugin]
  or [Using the Library API][../using-laika/embedded.md:Using the Library API].

* Try out Laika with the [Web Tool].

* Browse the source on [GitHub].

* Browse the [API].

* Follow on [Twitter] for release announcements.

* Create [Issues] here on GitHub for bug reports or enhancement requests.

* Ask questions on [Stackoverflow], tagging with Laika and Scala.
 
There will also be a dedicated forum in case of growing interest in Laika.

This manual is written in Markdown and transformed by Laika. Its source
is included in the repository inside the `docs` folder.


[Web Tool]: http://planet42.org/
[GitHub]: https://github.com/planet42/Laika
[API]: ../api/laika/api/
[Twitter]: https://twitter.com/_planet42
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika


Features
--------

* Support for Markdown (incl. GitHub Flavored Markdown) and reStructuredText as input

* Support for HTML, EPUB, PDF, XSL-FO and AST (for debugging) as output

* Template-based Site and Book Generation

* Support for tables of contents, convenient cross-linking between 
  documents and auto-numbering of documents and sections for all 
  supported markup formats
  
* Support for styling of PDF documents with CSS
  
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

* Support for file and stream IO and parallel processing based on cats-effect,
  with full control over ExecutionContexts for blocking IO and processing

* Purely functional library API

* More than 1,200 tests


Road Map
--------

* __0.13__: (tentative) Support for Scala.js

* __0.14__: Integrated support for syntax highlighting in source code

* __0.15__: Include a set of default themes for all output formats

* __1.0__: API polishing and removal of all deprecations


Release History
---------------

* __0.12.0__ (<WIP>, 2019):

    * Extraction of all file and stream IO, parallel processing and EPUB support to a new module laika-io
    * IO module based on cats-effect, with effect abstraction and full control over ExecutionContexts 
      for blocking IO and processing 
    * Refactoring of AST Rewrite API to be fully type-safe and avoid runtime reflection and exceptions.
    * Refactoring of the Render API to be referentially transparent

* __0.11.0__ (June 12, 2019):

    * New Renderer for producing EPUB containers
    * New `laikaEPUB` task in the sbt plugin
    * New `laikaIncludeEPUB` setting for the `laikaSite` task
    * Support for cover images for EPUB and PDF
    * Support for document metadata (author, language, date, etc.) for EPUB and PDF
    * Support for title pages per chapter
    * Backwards-compatible to 0.9.0 and 0.10.0 - if you update from earlier version, please see
      the release notes for 0.9.0 for migration

* __0.10.0__ (Dec 1, 2018):

    * Add support for GitHub Flavored Markdown:
        * Tables
        * Fenced Code Blocks
        * Auto-Links
        * Strikethrough
    * Preparing for Scala 2.13    
        * Adjust use of Collection API for breaking changes and deprecations in 2.13
        * Replace use of parallel collections with custom executor
    * Level of parallelism of transformations is now configurable

* __0.9.0__ (Sep 15, 2018):

    * New ExtensionBundle APIs allow to bundle extensions into a single object for easier reuse. Supported extension
      hooks include directives, markup parser extensions, rewrite rules, custom renderers, document type matchers, 
      alternative parsers for stylesheets, templates or configuration headers and default templates per output format. 
    * Reduced number of settings and tasks in the sbt plugin, by using the new ExtensionBundle API for sbt settings.
    * Improved package structure to reduce number of required imports. 

* __0.8.0__ (June 4, 2018):

    * Doubles parsing speed for both Markdown and reStructuredText
    * Much lower number of parser instance creations on repeated runs
    * Performance goals had been achieved through replacing the former
      Scala SDK parser combinators with a custom, optimized combinator design:
        * Fewer dependent types and base parsers in objects instead of traits, making it easier to freely compose parsers
        * Create parser error messages lazily, as most of them will never be accessed
        * Avoid cost of by-name args in all cases except | combinator
    * Add support for size and align options for the image directive in reStructuredText
    * Fixes for all bugs known and reported to this point 
    * Address all deprecation warnings for Scala 2.12    
    
* __0.7.5__ (Dec 30, 2017):

    * Support for sbt 1.0
    * Laika's sbt plugin is now an AutoPlugin
    * Prefixed all task and setting keys to adhere to recommended naming pattern
      (e.g. laikaGenerate) to avoid name conflicts for autoImports
    * Adjustments for API changes in sbt 1.0
    * Bug fixes in the library
    * Drop support for sbt 0.13 and Scala 2.10

* __0.7.0__ (April 17, 2017):

    * Support for Scala 2.12 (with continued support for 2.11 and 2.10)
    * Redesign and cleanup of the Document API: use case classes wherever possible,
      extract features into pluggable traits and introduce a new `Cursor` type for tree rewriting
    * Allow to customize the `FopFactory` for the PDF renderer (in API and sbt plugin)
    * Fix an issue in the `laika:site` task in the sbt plugin that executed several sub-tasks
      twice which could lead to IllegalStateExceptions caused by the resulting race condition
    * Fixes for the reStructuredText parser (for option lists and IP addresses)

* __0.6.0__ (May 23, 2016):

    * Support for rendering PDF documents
    * Support for rendering XSL-FO output
    * New CSS parser supporting a large subset of standard CSS
    * Support styling of PDF documents with CSS
    * Support for different templates per output format
    * New sbt tasks: `html`, `pdf`, `xslfo`, `prettyPrint` for rendering
      a single output format
    * New sbt task `generate` for rendering one or more output formats
      (e.g. `laika:generate html pdf`)
    * Integrate PDF output into existing sbt task `laika:site` via
      new setting `includePDF`
    * New directives `pageBreak`, `style` and `format`
    * Changes to the `Render` and `Transform` API to allow for the
      merging of an entire directory of input files into a single output
      file (as required by PDF rendering)

* __0.5.1__ (Oct 10, 2015):

    * Cross-compile for Scala 2.11 and 2.10
    * Publish the sbt plugin to the new plugin repository on Bintray
    * Upgrade to ScalaTest 2.2.4
    
* __0.5.0__ (Jan 9, 2014):

    * New sbt plugin, exposing all Laika features and customization hooks as sbt tasks and settings
    * New option to merge multiple input directories into a tree structure with a single root,
      allowing to keep reusable styles or templates ("themes") separately
    * New option to use Markdown and reStructuredText markup in the same input tree, including
      cross-linking between the two formats
    * Move to a multi-project build and rename the main artifact from `laika` to `laika-core`
    * Upgrade to ScalaTest 2.0 and sbt 0.13
    * Drop support for Scala 2.9.x

* __0.4.0__ (Nov 22, 2013):

    * Template-based site generation
    * Support for tables of contents, convenient cross-linking between documents 
      and autonumbering of documents and sections for all supported markup formats
    * Custom Directives for templates and text markup
    * Document Fragments that can be rendered separately from the main document content
    * New API for batch processing for parse, render or full transform operations
    * Parallel processing of parsers and renderers 
      
* __0.3.0__ (Aug 3, 2013):

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

* __0.2.0__ (May 7, 2013):

    * Support for reStructuredText (full specification)
    * Concise and type-safe API for all reStructuredText extensibility options (directives, text roles)
    * New document tree nodes for tables, footnotes, citations, definition lists, internal links,
      comments, system messages, invalid elements
    * Render hints for document tree nodes in the form of the new Customizable trait

* __0.1.0__ (Jan 12, 2013): 

    * Support for Markdown as input
    * Support for HTML and AST as output
    * Customization hooks for renderers
    * Document tree rewriting
    * Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)
    * Generic base traits for markup parser implementations


  
