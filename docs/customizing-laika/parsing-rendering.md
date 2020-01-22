
Separate Parsing and Rendering
==============================

The chapter [Using the Library API] introduced the API
for performing a full transformation from text markup to
output format. In many cases this is all you need.

Alternatively the parse and render operations can be executed
separately, which is useful for special cases when you need to 
process the document tree before rendering or when parsing 
and rendering should not happen at the same time or even for
creating the tree model programmatically instead of reading
from text input.


The Parser API
--------------

The Parser API is quite similar to the Transform API, but leaves
out the rendering step.


### Parsing a Single Document

Reading from a String:

```scala
val input = "some *text* example"    
val document = Parser
  .of(Markdown)
  .build
  .parse(input)
```

The `document` instance gives you the full document model. See
the [AST Scaladoc][ast-scaladoc] for details.

The `content` property of the `document` instance holds
the parsed content of the document in the form a tree 
of case classes.

You can then specify rewrite
rules to transform some nodes of the model (see [Document Tree Rewriting] for 
details) and use the Render API (explained below) to render
it to various output formats.


### Parsing an Entire Directory

Similar to the full transformation step, you need to specify the `ContextShift`
and `Blocker` to use for blocking IO and parallel processing:

```scala
implicit val cs: ContextShift[IO] = 
  IO.contextShift(ExecutionContext.global)
  
val blocker = Blocker.liftExecutionContext(
  ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
)
```

These will then be used to build a parallel parser instance:

```scala
val parser = Parser
  .of(Markdown)
  .io(blocker)
  .parallel[IO]
  .build
```

Finally, you can obtain a tree instance from the parser:

```scala
val tree: IO[DocumentTreeRoot] = parser
  .fromDirectory("source")
  .parse
```

The tree instance is of type `DocumentTreeRoot` which gives you access
to all documents, templates and subdirectories contained in the
parsed directory.



### Character Encoding

Laika uses the same platform-dependent defaults for file encodings as the
IO classes in the Scala SDK. The most convenient way to specify an encoding
is via an implicit:

```scala
implicit val codec:Codec = Codec.UTF8
```

This codec will then be used by `fromDirectory` and other methods 
shown in the examples above.


[ast-scaladoc]: ../api/laika/ast/
    

The Renderer API
----------------

The Renderer API is similar to the Transformer API, but expects
that you already have a document tree model, either obtained
by a previous parsing step or built programmatically, and now
want to render it to a specific output format.


### Reusing the parser configuration

In contrast to the Transformer, the Parser and Renderer keep these
two operations completely separate. However, this also means that the
renderer does not know about any custom renderers the parser might 
provide as extensions for rendering custom AST nodes the default renderer
does not know about.

For this reason it is always best to copy the parser configuration over
to the Render API:

```scala
val parser = Parser.of(Markdown).strict.build

val renderer = Renderer.of(HTML).withConfig(parser.config).build
```

For the sake of brevity we omit this detail from the following examples.


### Rendering a Single Document

Rendering as a string:

```scala
// obtained from a parse step or created programmatically
val doc: Document = ... 

val res: String = renderer.render(doc)
```


### Rendering an Entire Directory as HTML

Like for the Parser and Transformer APIs, you need to specify the `ContextShift`
and `Blocker` to use for blocking IO and parallel processing:

```scala
implicit val cs: ContextShift[IO] = 
  IO.contextShift(ExecutionContext.global)

val blocker = Blocker.liftExecutionContext(
  ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
)
```

You can then render the result to a files in a target directory:

```scala
// obtained from a parse step or created programmatically
val tree: DocumentTreeRoot = ???

val renderer = Renderer
  .of(HTML)
  .io(blocker)
  .parallel[IO]
  .build
    
val op: IO[Unit] = renderer
  .from(tree)
  .toDirectory("target")
  .render
```

The `target` directory is expected to exist, while any required
subdirectories will be automatically created during rendering. There
will be one HTML file for each input file in the same directory layout.


### Rendering an Entire Directory as PDF

```scala
// obtained from a parse step or created programmatically
val tree: DocumentTreeRoot = ???

val op: IO[Unit] = renderer
  .from(tree)
  .toFile("out.pdf")
  .render    
```

Here the input files get rendered into a single PDF document, with
the directory structure getting translated into the document structure
(optionally including bookmarks and table of contents).


### Character Encoding

Laika uses the same platform-dependent defaults for file encodings as the
IO classes in the Scala SDK. The most convenient way to specify an encoding
is via an implicit:

```scala
implicit val codec:Codec = Codec.UTF8
```

This codec will then be used by `toDirectory` and other methods 
shown in the examples above.
