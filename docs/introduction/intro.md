
Laika
=====

Laika is a customizable and extensible toolkit for transforming lightweight text markup and template based site generation.

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

The current version is published to Maven Central for Scala 2.12 and 2.11.

The sbt plugin is published to the sbt plugin repository for sbt 1.x.

If you still need to work with sbt 0.13 and/or Scala 2.10 you need to stay on Laika 0.7.0
which had been the final release supporting those versions.


### Using the sbt Plugin

Add the plugin to `project/plugins.sbt`:

    addSbtPlugin("org.planet42" % "laika-sbt" % "0.9.0")

Enable the plugin in your project's `build.sbt`:

    enablePlugins(LaikaPlugin)
    
Add Markdown, reStructuredText or HTML template files to `src/docs` in your
project and run the `laikaSite` task from within sbt to generate the site
in `target/docs/site`.    
    

### Using Laika Embedded

Adding the Laika dependency to your sbt build:

    libraryDependencies += "org.planet42" %% "laika-core" % "0.9.0"

Example for transforming from file to file:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"

Example for transforming an entire directory of markup files:

    Transform from ReStructuredText to 
      HTML fromDirectory "source" toDirectory "target"

Example for transforming an entire directory of markup files to a single PDF file:

    Transform from Markdown to PDF fromDirectory "src" toFile "hello.pdf"
    
When using Laika's PDF support you need to add one more dependency to your build:

    libraryDependencies += "org.planet42" %% "laika-pdf" % "0.9.0"


### Other Resources

* Read more about standard usage in the chapters  
  [Using the sbt Plugin][../using-laika/sbt.md:Using the sbt Plugin]
  or [Using Laika Embedded][../using-laika/embedded.md:Using Laika Embedded].

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

* Support for HTML, PDF, XSL-FO and AST (for debugging) as output

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

* Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)

* Parallel processing of parsers and renderers

* More than 1,200 tests


Road Map
--------

* __0.10__: Support for GitHub-flavored Markdown

* __0.11__: Support for output in the epub format

* __0.12__: Integrated support for syntax highlighting in source code

* __1.0__: API polishing


Release History
---------------
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


  
