
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

The current version is published to Maven Central for Scala 2.13, 2.12 and Scala.js 1.0.

The sbt plugin is published to the sbt plugin repository for sbt 1.x.

The final release for Scala 2.11 had been 0.10.0, 
the final release for Scala 2.10 and sbt 0.13 was 0.7.0.


### Using the sbt Plugin

Add the plugin to `project/plugins.sbt`:

```scala
addSbtPlugin("org.planet42" % "laika-sbt" % "0.14.0")
```

Enable the plugin in your project's `build.sbt`:

```scala
enablePlugins(LaikaPlugin)
```

Add Markdown, reStructuredText or HTML template files to `src/docs` in your
project and run the `laikaSite` task from within sbt to generate the site
in `target/docs/site`.    


### Using the Library API

If you are updating from a version before 0.12.0, it's recommended to read
the [Migration Guide](http://planet42.github.io/Laika/migration-guide-0.12.html) 
first, as there were significant changes in the Library API.

Adding the Laika dependency to your sbt build:

```scala
libraryDependencies += "org.planet42" %% "laika-core" % "0.14.0"
```

If you want to use Laika with Scala.js, use the standard triple-%:
```scala
libraryDependencies += "org.planet42" %%% "laika-core" % "0.14.0"
```

Note that only `laika-core` has been published for Scala.js, the `laika-io`
and `laika-pdf` modules are only available for the JVM.

Example for transforming Markdown to HTML:

```scala
import laika.api._
import laika.format._

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .build
  
val res: Either[ParserError, String] = transformer
  .transform("hello *there*")
```


For file/stream IO, parallel processing and/or EPUB support, based on cats-effect, 
add the laika-io module to your build:

```scala
libraryDependencies += "org.planet42" %% "laika-io" % "0.14.0"  
```

Example for transforming an entire directory of markup files to a single EPUB file:

```scala
import laika.api._
import laika.format._
import laika.io.implicits._

implicit val cs: ContextShift[IO] = 
  IO.contextShift(ExecutionContext.global)
  
val blocker = Blocker.liftExecutionContext(
  ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
)

val transformer = Transformer
  .from(Markdown)
  .to(EPUB)
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
  
val res: IO[Unit] = transformer
  .fromDirectory("src")
  .toFile("hello.epub")
  .transform
```

When using Laika's PDF support you need to add one more dependency to your build:

```scala
libraryDependencies += "org.planet42" %% "laika-pdf" % "0.14.0"
```

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
  
* Integrated parsers for syntax highlighting that work for all output
  formats, not just HTML.
  
* Support for styling of PDF documents with CSS
  
* sbt plugin, exposing all Laika features and customization hooks
  as sbt settings and tasks
  
* Support for Scala.js for all of Laika's features except File/Stream IO, EPUB and PDF
  
* Purely Functional Library API, respecting referential transparency,
  no exceptions, no mutable state

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

* More than 1,500 tests
