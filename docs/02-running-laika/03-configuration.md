
Configuration
=============


from old embedded page:


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


===============================================================================================================

from old plugin page:


Settings for Customization Hooks
--------------------------------

Laika allows to easily add custom logic to the processing of documents. When all you need to
adjust is the processing of one or more particular nodes types, a simple partial function allows
to do that while still benefiting from the defaults for all other node types.

When working with the document tree (the internal AST representing the document structure),
it is most comfortable to add the following import to the build:

```scala
import laika.ast._
```

### Custom Renderers

A custom renderer allows to override the generated output for one or more specific node
types, while falling back to the default renderers for all other node types.

The `laikaHtmlRenderer` shortcut allows to add a custom HTML renderer to the `laikaExtensions` setting
and expects a function of type `PartialFunction[(HTMLFormatter, Element), String]`. 

`HTMLFormatter` provides the API for rendering tags, adds the current indentation level after
line breaks and knows how to render child elements. 

`Element` is the base trait for all nodes in a document tree. 
For all elements where this partial function is not defined, Laika will use the default renderers for all node types. 

The following simple example shows how you can add a style to the renderer for
the `<em>` tag:

```scala
laikaExtensions += laikaHtmlRenderer {
  case (fmt, Emphasized(content, opt)) => 
    fmt.element("em", opt, content, "class" -> "big") 
}
```

For more details see the chapter [Customizing Renderers][../customizing-laika/customize-rendering.md:Customizing Renderers].
  
Similarly the `laikaFoRenderer` shortcut can be used to add a custom `XSL-FO` renderer 
of type `PartialFunction[(FOFormatter, Element), String]`. `XSL-FO` is an interim format for PDF output,
so this option would also allow to change the appearance of PDF documents.

For EPUB the `laikaEpubRenderer` can be used to add a custom XHMTL renderer of type
`PartialFunction[(HTMLFormatter, Element), String]`. 


### Customizing the HTML Renderer (library use case)

Finally you can adjust the rendered output for one or more node types
of the document tree programmatically with a simple partial function:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .rendering {
    case (fmt, Emphasized(content, opt)) => 
      fmt.element("em", opt, content, "class" -> "big")  
  }
  .build
```

Note that in some cases the simpler way to achieve the same result may be
styling with CSS.

See [Customizing Renderers][../customizing-laika/customize-rendering.md:Customizing Renderers] for more details.


### Custom Rewrite Rules

When customizing renderers you have to repeat the step for each output format like HTML, EPUB or PDF. 
A rewrite rule lets you express a transformation
of a node inside the document tree before rendering, so it would have an effect on all output formats.

A rewrite rule is a function of type `PartialFunction[T, RewriteAction[T]]`.
If the function is not defined for a particular element or the result is `Retain` the old element is kept in the tree.
If it returns `Replace(Element)` this element replaces the old one.
If the function returns `Remove` the old element is removed from the tree.
The type parameter `T` is either `Block`, `Span` or `TemplateSpan`, depending on the kind of AST element you
want to rewrite.

The following (somewhat contrived, but simple) example shows how to turn each `Emphasized` node
into a `Strong` node:

```scala
laikaExtensions += laikaSpanRewriteRule { 
  case Emphasized(content, opts) => Replace(Strong(content, opts))
}
```

For more details see the chapter [Document Tree Rewriting].


### Custom Document Type Matcher

A document type matcher categorizes an input document based on its path. 
It is a function that takes a `Path` instance (from the Laika API, not `sbt.Path`)
and returns the matching `DocumentType` with valid values being one of the following
objects:

```scala
Markup, Template, Dynamic, Static, Config, Ignored
```

These values correspond to the descriptions provided in [Document Types].

The function is of type `Path => DocumentType` and can be added to the `laikaExtensions` setting:

```scala
laikaExtensions += laikaDocTypeMatcher {
  case path: Path => path.name match {
    case "hello.md" => Markup
    case "hello.js" => Static
  }
}
```


Settings for Directives
-----------------------

Directives are Laika's extension hook for both, templates and text markup.
They allow to add new tag-like constructs without touching the existing parsers.

Laika supports two flavors of directives, one compatible with the reStructuredText
specification and the other with a Laika-specific syntax. 

The reStructuredText
variant is supported for full compatibility with the reStructuredText specification
and reuse of existing reStructuredText files making use of some of the standard
directives. Laika supports almost all of the directives defined in the
specification. Add an instance of type `RstExtensionRegistry` to the `laikaExtensions` 
setting for adding custom implementations to the built-in ones.
For more details on this directive type see [Extending reStructuredText].

The Laika variant is more flexible, as it can be used in template files as well
as Markdown and reStructuredText markup. It is also somewhat simpler in syntax
and configuration while offering the same functionality. It comes with a small
set of predefined directives in this syntax. Add an instance of type `DirectiveRegistry` 
to the `laikaExtensions` setting for adding custom implementations to the built-in ones.
For more details on this directive type see [Directives][../extending-laika/directive.md:Directives].

The examples in the two chapters linked above show how to implement a directive
and register it either for use in sbt or in embedded mode.



==================================================================================================================

from old output page:


Formatted AST
-------------

A renderer that visualizes the document tree structure, essentially a formatted
`toString` for a tree of case classes, mainly useful for testing and debugging
purposes.

You can use this renderer with the Transformer API:

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

Alternatively you can use the Render API to render an existing document:

```scala
val input = "some *text* example"

val doc = Parser.of(Markdown).build.parse(input)

Renderer.of(AST).build.render(doc)
```

The above will yield the same result as the previous example.

Finally, if you are using the sbt plugin you can use the `laikaAST` task.


### HTML Renderer Properties

The `unformatted` property tells the renderer to omit any formatting (line breaks or indentation) 
around tags. Useful when storing the output in a database for example:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .unformatted 
  .build
```

The `withMessageLevel` property instructs the renderer to include system messages in the
generated HTML. Messages may get inserted into the document tree for problems during
parsing or reference resolution, e.g. an internal link to a destination that does not
exist. By default these messages are not included in the output. They are mostly useful
for testing and debugging, or for providing feedback to application users producing 
markup input:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .withMessageLevel(Warning)
  .build
```
