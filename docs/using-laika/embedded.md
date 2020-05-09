

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
