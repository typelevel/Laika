
Library API
===========

Laika's API is fully referentially transparent, does not throw exceptions and
avoids any kind of runtime reflection. Its core module is also available for Scala.js.
The library only has a very limited number of dependencies and does not require the installation of any external tools.

This chapter only covers the topics which are specific to using Laika without its sbt plugin, 
while most other parts of the manual apply to both use cases, library and plugin.


Dependencies
------------

Which of the modules you need to add to the build depends on the functionality you intend to use.

If you want to stick to pure transformations from string to string and don't need file or stream IO or
any of the binary output formats like EPUB or PDF, you are fine with just using the `laika-core` module:

```scala
libraryDependencies += "org.planet42" %% "laika-core" % "0.14.0" 
```

This module is also 100% supported for Scala.js, so you can alternatively use the triple `%%%` syntax
if you want to cross-build for Scala.js and the JVM:

```scala
libraryDependencies += "org.planet42" %%% "laika-core" % "0.14.0" 
```

If you want to add support for file and stream IO and/or output in the EPUB format, 
you need to depend on the `laika-io` module instead:

```scala
libraryDependencies += "org.planet42" %% "laika-io" % "0.14.0" 
```

This depends on `laika-core` in turn, so you always only need to add one module as a dependency and will get
the rest via transitive dependencies. No module apart from `laika-core` is available for Scala.js, so you
are in JVM land here.

Finally PDF support comes with its own module as it adds a whole range of additional dependencies:

```scala
libraryDependencies += "org.planet42" %% "laika-pdf" % "0.14.0" 
```

Again, this builds on top of the other modules, so adding just this one dependency is sufficient.


Anatomy of the API
------------------

The library API for creating parsers, renderers or transformers, while aiming to be intuitive and simple, 
also exposes one of the main design goals of Laika: that everything is meant to be pluggable.

The following example for creating a pure transformer shows the main building blocks:

[TODO diagram]

* All inputs need to support the `MarkupFormat` contract.
  Supported by the core library are `Markdown` and `ReStructuredText`.
  
* Output formats implement the `RenderFormat[FMT]` trait.
  The type parameter represents the formatter API that renderer overrides for specific AST nodes can talk to,
  which is different depending on the format.
  The library supports the formats `HTML`, `EPUB`, `PDF`, `XSLFO` and `AST` out of the box.
  The latter is a debug output that renders the document AST in a formatted structure.
  
* Finally, the optional `using` hook in the API accepts anything that implements `ExtensionBundle`.
  This API has hooks for all phases of the transformation process, parsing, AST transformation and rendering
  and is Laika's main extension point.
  Shown above are `GitHubFlavor` and `SyntaxHighlighting`, two bundles provided by the library that can be enabled on demand. 

If you require support for file/stream IO, templating or binary output formats, 
the `laika-io` module expands on the core API to add this functionality:

[TODO diagram]

The elements of the API which are identical to the pure transformer are darkened.
This API introduces a dependency on `cats-effect` which is used to model the effectful computations.
You can use it with any effect that supports the `cats-effect` type classes, like cats-IO, Monix or Zio.

* The blocker passed in the example gives you control over which `ExecutionContext` blocking IO is performed in.

* The call to `parallel[IO]` builds a transformer that lets you specify entire directories as input and allows to instruct
  the library which effect type to use (cats-IO in this example). There is a second variant, `sequential`, 
  that builds a transformer that accepts individual files, streams or strings as input and output.
  
Using a `Transformer` is the most convenient option when you want to go directly from the raw input format (text markup)
to the final output format. There are also `Parser` and `Renderer` instances that only do one half of the transformation.

All types, `Transformer`, `Parser` and `Renderer` come in three flavors each. 
A simple, pure API for dealing with strings as input and output, 
a sequential, effectful API for processing individual files and streams 
and finally a parallel, effectful API for processing entire directories with templates and configuration files 
in addition to the usual text markup input.


Transforming Strings
--------------------

Transforming in-memory input is the simplest form of transformation, and works for Markdown and reStructuredText 
as input, and for HTML as output. EPUB and PDF both require additional modules and are described later in this chapter.

For most cases where you don't use any of the customization hooks, you should be fine with just these imports:

```scala
import laika.api._
import laika.format._
```

As the next step you can then create transformer instances, which in most cases you'll want to reuse with different
inputs to reduce the memory footprint and initialization overhead. 
The instances are immutable and thread-safe.

Example for creating a transformer for Markdown, including the GitHub-Flavor extensions:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .build
```

Example for creating a transformer for reStructuredText:

```scala
val transformer = Transformer
  .from(ReStructuredText)
  .to(HTML)
  .build
```

In addition to specifying markup and output formats and extensions, there are many other customization options in the builder
step immediately before calling `build`. They are identical for the pure and IO parser and are summarized in the [Configuration] section.

Finally you can use the transformer with text markup as input:

```scala
val result = transformer.transform("hello *there*")
```

The result is of type `Either[ParserError, String]`.


Setup for Effectful Transformations
-----------------------------------

In case you want to go beyond pure string transformations, 
you need to switch your dependency to the `laika-io` module as shown in the [Dependencies] section.

This module depends on cats-effect, and models all side effects in an abstract effect,
so that you can use it with cats IO, Monix or Zio.

With the dependency in place you also need to add a third import to those you used for pure transformations:

```scala
import laika.api._
import laika.format._
import laika.io.implicits._

```

The remainder of the setup depends on whether you are [Using cats.IO, Monix or Zio]
or whether you build [Applications without Effect Library].


### Using cats.IO, Monix or Zio

When your application is already using one of the effect libraries compatible with the cats-effect type class hierarchy,
integrating Laika will be seamless. 

The exact mechanics of your Laika transformer setup will depend on which of the aforementioned libraries you are using 
and whether you keep all of you code in an abstract `F[_]` instead of coding against a concrete effect type.

The following example assumes the use case of an application written around abstract effect types and using `IOApp`
from cats.IO for initialization:

```scala
def createTransformer[F: Async: ContextShift](blocker: Blocker): SequentialTransformer[F] =
  Transformer
    .from(Markdown)
    .to(HTML)
    .using(GitHubFlavor)
    .io(blocker)
    .sequential[F]
    .build
``` 

The setup method above can then be used inside `IOApp` initialization logic:

```scala
object MyApp extends IOApp {

  def run(args: List[String]) = {
    Blocker[IO].use { blocker =>
      val transformer = createTransformer(blocker)
      // other setup code
    }.as(ExitCode.Success)
  }
}
```

This way Laika gives full control over the `ExecutionContext` in which the blocking IO and CPU-bound operations are performed.

Setup for other libraries would be similar, Monix for example comes with a `TaskApp` which is similar to `IOApp`.


### Applications without Effect Library

When using other stacks like Akka HTTP or the Play framework, you need to bridge the gap between the worlds of
cats-effect and the surrounding toolkit, often centering around the Future API (which, in contrast to cats-effect,
is not referentially transparent).

First, to create a transformer with the setup method created in the previous example, you need to create instances
of `ContextShift` and `Blocker` yourself:

```scala
implicit val cs: ContextShift[IO] = 
  IO.contextShift(ExecutionContext.global)
  
val blocker = Blocker.liftExecutionContext(
  ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
)

val transformer = createTransformer(blocker)
```

The resulting instance can then be used to describe a transformation: 

```scala
val result: IO[String] = transformer
  .fromFile("hello.md")
  .toFile("hello.html")
  .transform
```

The resulting `IO` instance is only a description of a program, as it starts evaluation lazily, 
in contrast to the Scala SDK's `Future` which starts processing eagerly.

For actually running the effect and producing a result,
you have several options depending on the stack you are using.
One common scenario is a toolkit like Akka HTTP or Play where you execute a route that is expected to return a `Future`.
This can achieved by a simple translation:

```scala
val futureResult: Future[String] = result.unsafeToFuture()
```

Other options not involving `Future` are either blocking, synchronous execution:

```scala
val syncResult: String = result.unsafeRunSync()
```

Or asynchronous execution based on a callback:

```scala
result.unsafeRunAsync {
  case Left(throwable) => handleError(throwable)
  case Right(result)   => handleResult(result)
}
```

Do not get too hung up on the scary sound of all these `unsync...` methods. The one kind of safety you are loosing
when using them is referential transparency. But if you are using a `Future`-based API for example, 
your program is not referentially transparent anyway.


File and Stream IO
------------------

With transformer instances created by the setup shown in the previous section in place, 
you can now transform the content of files:

```scala
val res: IO[String] = transformer
  .fromFile("hello.md")
  .toFile("hello.html")
  .transform
```

Or use streams as input and output:

```scala
val input: IO[InputStream] = ???
val output: IO[InputStream] = ???

val res: IO[Unit] = transformer
  .fromStream(input)
  .toStream(output, autoClose = false)
  .transform 
```

The API is similar to that for File IO, but the creation of the streams are treated as an effect, 
so you have to pass an `F[InputStream]` or `F[OutputStream]`:

The `autoClose` flag is `true` by default, 
which means the stream will be closed after all input has been read or all output has been written.

The previous two example both showed matching input and output types, but of course they can be freely combined.
The last example shows an in-memory string as input and a file as output:

```scala
val res: IO[String] = transformer
  .fromString("hello *there*")
  .toFile("hello.html")
  .transform
```


Entire Directories as Input
---------------------------

The parallel, effectful transformer is the most powerful variant and also the one that is the basis for the sbt plugin.
It expands the functionality beyond just processing markup files to also parsing templates and configuration files
as well as copying static files over to the target directory.

The setup is almost identical to the transformer for individual files or streams:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
```

The only difference is switching from `sequential[IO]` to `parallel[IO]`. 
This switch does not only cause a change in the internal execution model, 
but also actually changes the API of the transformer. 
The sequential API offers the `fromFile/toFile` or `fromStream/toStream` methods (amongst other options), 
whereas the parallel API comes with the `fromDirectory/toDirectory` combination, 
alongside other ways to transform multiple inputs in parallel.

```scala
val res: IO[RenderedTreeRoot[IO]] = transformer
  .fromDirectory("src")
  .toDirectory("target")
  .transform
```

The `target` directory is expected to exist, while any required subdirectories will be automatically created during rendering. 
There will be one HTML file for each input file in the same directory layout.

If you replace the `HTML` renderer in the example above with one of the binary formats (`EPUB` or `PDF`),
the API will be slightly different, as the `toDirectory` option is replaced by `toFile`. 
This is because these formats always produce a single binary result, 
merging all content from the input directories into a single, linearized e-book:

```scala
val res: IO[Unit] = transformer
  .fromDirectory("src")
  .toFile("output.epub")
  .transform
```


### Merging Multiple Directories

All previous examples read content from the single input directory. But you can also merge the contents of multiple directories:

```scala
val res: IO[RenderedTreeRoot[IO]] = transformer
  .fromDirectories("markup", "theme")
  .toDirectory("target")
  .transform
```

This adds some additional flexibility, as it allows, for example, to keep reusable styles and templates separately.
This flexibility is possible as Laika is not tied to the file system, but instead provides a [Virtual Tree Abstraction].


### Preparing Content

Laika does not have any special directories and content can be nested in sub-directories down to arbitrary levels.
For more details on how to organize content, see [Directory Structure].

Transformers and Parsers distinguish between the following file types:

* **Markup Files**: Files with the extensions `.md`, `.markdown` or `.rst` (depending on the specified input format) 
  will be parsed and rendered to the target in the same directory structure and with the same file names apart from the suffix,
  which will be replaced depending on the output format (e.g. `.html`).
* **Configuration Files**: Each directory can contain an optional `directory.conf` file for specifying
  things like navigation order or chapter title. See [[TODO]] for details.
* **Template Files**: You can provide a default template per directory with the name `default.template.<suffix>`
  with the suffix matching the output format (e.g. `.html`). 
  They will also be applied to sub-directories, unless overridden. 
  You can also add additional templates with the name pattern `*.template.<suffix>`, 
  which will only be applied when a markup document explicitly refers to them in its configuration header.
  (The upcoming 0.16 release will introduce Themes which will provide default templates out of the box. 
  Until then you need to provide at least one default template in the root directory unless you are fine with the
  very basic fallback template.)
* **Static Files**: All other files, like CSS, JavaScript, images, etc., will be copied over to the 
  target in the same directory structure and with identical file names.


Separate Parsing and Rendering
------------------------------

So far all examples in this chapter showed the use of a transformer instance, 
which takes you all the way from a raw input file to the target format, e.g. from Markdown to HTML.

But in some cases you may just want to execute one half of the process:

* If the input is generated in-memory. In this case there is no need to generate text markup, you can directly
  produce the document AST and pass it to a `Renderer`.
  
* If you want to process the document AST produced by a `Parser` by other means than rendering one of the supported
  output formats.

* If you want to render the same input to multiple output formats. 
  In this case building a `Transformer` for all output formats is inefficient as each of them would parse the same input again. 
  Instead you can create a single `Parser` for the input and in addition one `Renderer` for each output format you want to generate.

In fact if you examine the implementations of the various pure and effectful transformer types, you'll notice that
it is mostly trivial: delegating to the underlying `Parser` and `Renderer` and just piping the result from one to the other.

The following code example demonstrates the third scenario listed above: Rendering the same input to multiple output formats.

First we create a parser that reads from a directory:

```scala
val parser = MarkupParser
  .of(Markdown)
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
```

Next we create the renderers for the three output formats:

```scala
val htmlRenderer = Renderer.of(HTML).io(blocker).parallel[IO].build
val epubRenderer = Renderer.of(EPUB).io(blocker).parallel[IO].build
val pdfRenderer  = Renderer.of(PDF).io(blocker).parallel[IO].build
```

Finally we define the actual transformation by wiring the parser and the three renderers:

```scala
val transformOp: IO[Unit] = 
  parser.fromDirectory("source").parse.flatMap { tree =>
    val htmlOp = htmlRenderer.from(tree).toDirectory("target").render
    val epubOp = epubRenderer.from(tree).toFile("out.epub").render
    val pdfOp = pdfRenderer.from(tree).toFile("out.pdf").render
    (htmlOp, epubOp, pdfOp).parMapN { (_, _, _) => () }
  }
```

We are using cats' `parMapN` here to run the three renderers in parallel.

The `tree` instance passed to all renderers is of type `DocumentTreeRoot`. 
If necessary you can use its API to inspect or transform the tree before rendering.
See [The Document AST] for details.

The sample code in this scenario showed the parallel-effectful variant of the `Parser` and `Renderer` types,
but just as the `Transformer` they exist in the other two flavors as well: sequential-effectful and pure.


Configuration
-------------

All the examples in this chapter only scratched the surface of Laika's API, 
focusing on the basics like specifying input and output. 
But since Laika is designed to be fully customizable and serve as a toolkit for creating toolkits, there is whole
range of options available for customizing and extending its built-in functionality.

Most oof these configuration options are not specific to the library API use case and apply to the sbt plugin as well,
apart from differences in the syntax/mechanics which with they are applied, which are reflected in the corresponding code examples.
For this reason this section only gives a very brief overview while linking to the relevant sections in the other chapters.

- [Strict mode]: Disables all non-standard extensions Laika adds to the supported markup formats, like directives.

- [Raw Content]: Enables the inclusion of raw sections of the target format in markup files, 
  e.g. snippets of verbatim HTML embedded in Markdown files. By default this is disabled.

- [Character Encoding]: Sets the character encoding of input and output files, the default is UTF-8.

- [Error Handling & Debugging]: Specify log levels or switch to "visual debugging", where recoverable errors are
  rendered in the page context where they occur instead of causing the transformation to fail.

- **Laika Extensions**: Use the library's customization options to override renderers for specific AST nodes ([Overriding Renderers]),
  transform the document AST before rendering ([AST Rewriting]), install custom directives ([Implementing Directives])
  or use some of the lower level hooks in ([The ExtensionBundle API]).

- [TODO] Other Configuration Options**: 

- [Inspecting Laika's Configuration]: Run the `describe` method on the IO-based transformers, parsers and renderers 
  to get a formatted summary of the active configuration, installed extension bundles and lists of input and output files.
