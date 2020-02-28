
Using the Library API
=====================

Apart from using Laika via its sbt plugin, you can alternatively use its library API
in your own applications (or libraries). Laika is very lightweight, it does not
require the installation of any external tools and the core artifact
has no dependencies.

The main and most commonly used operation is a full transformation
from input text written in a lightweight markup language like Markdown to 
output formats like HTML.
 
Alternatively the parse and render operations can be executed
separately, which is described in the chapter
[Separate Parsing and Rendering].

The following sections describe the options for full transformations
from text markup to output format.



Transforming Strings
--------------------

Transforming in-memory input is the simplest form of transformation, and works for Markdown and reStructuredText 
as input, and for HTML as output. EPUB and PDF both require additional modules and are described later in this chapter.


### Setup

First add the dependency to your build:

```scala
libraryDependencies += "org.planet42" %% "laika-core" % "0.14.0" 
```

For most cases where you don't use any of the customization hooks, you should be fine with just these imports:

```scala
import laika.api._
import laika.format._
```


### Running a Transformation

Converting a string from Markdown to HTML:

```scala
Transformer
  .from(Markdown)
  .to(HTML)
  .build
  .transform("hello *there*")
```

From reStructuredText to HTML:

```scala
Transformer
  .from(ReStructuredText)
  .to(HTML)
  .build
  .transform("hello *there*")
```


### Reusing Transformer Instances

You would normally keep the transformer instance for reuse with different
inputs and outputs to reduce the memory footprint and initialization overhead:

```scala
val transformer = Transformer.from(Markdown).to(HTML).build

val res1 = transformer.transform("example *1*")
val res2 = transformer.transform("example *2*")
```


### Adding Extensions

The most likely bundle you would use in everyday scenarios is probably GitHub-Flavored Markdown,
which is not installed by default:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .build
```

The `GitHubFlavor` object is an instance of `ExtensionBundle` an API that application
or library authors can use themselves for bundling their own customizations. It has hooks
for all phases of the transformation process, parsing, AST transformation and rendering.


### Debugging with AST Output

If you are investigating an unexpected result, it might help to get
an insight into how Laika has interpreted the input and display the entire
AST structure. It truncates longer strings, so it should normally be convenient
to browse the entire tree structure:

```scala
val input = "some *text* example"

Transformer
  .from(Markdown)
  .to(AST)
  .build
  .transform(input)
```

The output for the small example above will have the following format:

```laika-ast
RootElement - Blocks: 1
. Paragraph - Spans: 3
. . Text - 'some '
. . Emphasized - Spans: 1
. . . Text - 'text'
. . Text - ' example'
```


File/Stream IO and Binary Formats
---------------------------------

In case you want to transform from files or directories, or use one of the binary formats
like EPUB or PDF, you need to add another dependency to your build:

```scala
libraryDependencies += "org.planet42" %% "laika-io" % "0.14.0" 
```

This module depends on cats-effect, and models all side effects in an abstract effect,
so that you can use it with cats IO, Monix or Zio.

The type class constraints of the Library API are `Async`, `ContextShift` 
and for some use cases a `Parallel` instance.


### Providing ContextShifts for Blocking IO

Laika gives full control over the `ExecutionContext` in which the blocking IO operations
are performed. All the examples below will assume a setup like this:

```scala
implicit val cs: ContextShift[IO] = 
  IO.contextShift(ExecutionContext.global)
  
val blocker = Blocker.liftExecutionContext(
  ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
)
```


### Transforming a Single File

You can use the sequential builder to transform single files or streams:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .io(blocker)
  .sequential[IO]
  .build

val res: IO[Unit] = transformer
  .fromFile("hello.md")
  .toFile("hello.html")
  .transform
```

Note that the first line builds a transformer in the same way as the examples
for in-memory transformations. The IO module only wraps additional features
around a transformer.

The result of the above transformation is an `IO[Unit]`. If you build applications
using a stack from the cats-effect ecosystem, like fs2 or http4s, you are good to go.

If you run in a different stack, like Akka HTTP, you can convert the `IO` to a `Future`:

```scala
val res: Future[Unit] = transformer
  .fromFile("hello.md")
  .toFile("hello.html")
  .transform
  .unsafeToFuture()
```


### Using Streams

The API is similar to that for File IO, but the creation of the streams are treated
as an effect, so you have to pass an `F[InputStream]` or `F[OutputStream]`:

```scala
val input: IO[InputStream] = ???
val output: IO[InputStream] = ???

val res: IO[Unit] = transformer
  .fromStream(input)
  .toStream(output, autoClose = false)
  .transform 
```

The `autoClose` flag is `true` by default, which means the stream will be closed
after all input has been read or all output has been written.

 
### Transforming an Entire Directory

You can use the parallel builder to transform an entire directory of files:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
  
val res: IO[Unit] = transformer
  .fromDirectory("src")
  .toDirectory("target")
  .transform
```

The parser will pick up any document with a recognized suffix (`.md` or `.markdown`
for Markdown, `.rst` for reStructuredText).

The target may be a sub-directory of the source, the library will recognize this setup
and not process the output files as input.


### Merging Multiple Input Directories

Laika allows to merge multiple directories into a tree structure with 
a single root. This allows to keep reusable styles and templates separately,
or any other kind of flexible setup. 

This flexibility is possible as all directory input gets translated to a virtual
tree independent of the file system. In theory you could also mix file input
and in-memory input in the same transformation, although this would probably
be more of an edge case. 

When the merged directories contain sub-folders with the same name, those
will be merged recursively. Only files with the same name in the same folder
are treated as errors.

Use the `fromDirectories` method to specify the directories to merge:

```scala
val res: IO[Unit] = transformer
  .fromDirectories("markup", "theme")
  .toDirectory("target")
  .transform
```

### Templates

The directories that contain the markup documents can optionally contain
templates that get applied to the parsed documents. A very basic template
for HTML output may look like this:

```laika-html
<html>
  <head>
    <title>${document.title}</title>
  </head>
  <body>
    <div class="content">
      ${document.content}
    </div>
  </body>
</html>
```
    
The two placeholders enclosed in double curly braces will be replaced with the
title and content of the parsed markup document. Everything else will be copied
to the final document unchanged.

Every directory can contain a file with the name `default.template.<suffix>` that
automatically gets applied to all documents in that directory and its subdirectories,
unless overridden in a subdirectory. There are also ways to manually declare a
template for a specific markup document only. 

The suffix must match the output format of the transformation, e.g. `.html` for HTML,
`.fo` for PDF (as it renders via XSL-FO) and `.epub.xhtml` for EPUB.

For more details on the template engine, see the chapter [Templates].

For features like titles, sections, cross-linking, auto-numbering 
and tables of content, see the chapter [Document Structure].


### Rendering EPUB and PDF

These binary formats also rely on the laika-io module, and they always produce a single
output file, even if the input is an entire directory:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(EPUB)
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
  
val res: IO[Unit] = transformer
  .fromDirectories("markup", "theme")
  .toFile("output.epub")
  .transform
```

Note that the API recognizes the kind of transformer passed in and adjusts automatically 
in which methods it offers. 
Where it offered the `toDirectory` method for HTML output for example, 
it now only offers `toStream` or `toFile` in case of EPUB output.


### Parallel Execution

The actual transformation is a three phase process, the first (parsing) and
third (rendering) can run in parallel. For the second phase this is not possible,
as this is the rewrite step for the document tree model where things like cross references or
table of contents get processed that need access to more than just the current
document. But the rewrite phase is also the least expensive phase so that you
should still benefit from parallel execution.


### Character Encoding

Laika uses the same platform-dependent defaults for file encodings as the
IO classes in the Scala SDK. The most convenient way to specify an encoding
is via an implicit:

```scala
implicit val codec:Codec = Codec.UTF8
```

This codec will then be used by the `fromDirectory` and `toDirectory` methods shown
in the examples above.


### Error Reporting

Text markup parsers are usually very resilient. For any input they cannot
make sense of they fall back to rendering it back as raw text. Therefore
transformations rarely fail, but the output may not exactly be what you 
expected.

For some errors like unresolved link references or illegal directive
attributes, Laika inserts system message nodes into the tree. By default
these are ignored by all renderers, but you can explicitly enable
the rendering of message nodes for a specific message level.

In the HTML renderer messages are rendered as a span with the class
`system-message` and a second class for the level (`warning`, `error`, etc.),
so you can add CSS to easily identify these nodes in the page. This can be
useful for your own debugging purposes, but also for applications that allow
users to edit text with markup, giving them visual feedback for their mistakes.

The following example renders all message with the level `Warning` or higher:

```scala
val transformer = Transform
  .from(Markdown)
  .to(HTML)
  .withMessageLevel(Warning)
  .build
```
