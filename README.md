![Laika](http://planet42.github.io/Laika/img/laika-top.png)

Customizable and extensible toolkit for transforming lightweight text markup and template based site and e-book generation.

Supporting Markdown and reStructuredText as input and HTML, EPUB and PDF as output, 
either through its integrated sbt plugin or embedded in Scala applications, 
without the need to install external tools.

Open Source under the Apache 2.0 License.


Getting Started
---------------

The main artifact is published to Maven Central for Scala 2.13 and 2.12.

The sbt plugin is published to the sbt plugin repository for sbt 1.x.

The final release for Scala 2.11 had been 0.10.0, 
the final release for Scala 2.10 and sbt 0.13 was 0.7.0.


### Using the sbt Plugin

Add the plugin to `project/plugins.sbt`:

```scala
addSbtPlugin("org.planet42" % "laika-sbt" % "0.12.0")
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
libraryDependencies += "org.planet42" %% "laika-core" % "0.12.0"
```

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
libraryDependencies += "org.planet42" %% "laika-io" % "0.12.0"
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
libraryDependencies += "org.planet42" %% "laika-pdf" % "0.12.0"
```

The example for how to transform a directory of input files into a PDF file looks
the same as the EPUB example, apart from swapping `EPUB` for `PDF` and change
the file extension of the target file.

### Other Resources

For further information:

* Read the [Manual].

* Try out Laika with the [Web Tool].

* Browse the [API].

* Follow on [Twitter] for release announcements.

* Create [Issues] here on GitHub for bug reports or enhancement requests.

* Ask questions on [Stackoverflow], tagging with Laika and Scala.
 

[Manual]: http://planet42.github.com/Laika/index.html
[Web Tool]: http://planet42.org/
[API]: http://planet42.github.com/Laika/api/laika/api/
[Twitter]: https://twitter.com/_planet42
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika
