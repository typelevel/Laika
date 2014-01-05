
Separate Parsing and Rendering
==============================

The chapter [Using Laika Embedded] introduced the API
for performing a full transformation from text markup to
output format. In many cases this is all you need.

Alternatively the parse and render operations can be executed
separately, which is useful for special cases when you need to 
process the document tree before rendering or when parsing 
and rendering should not happen at the same time or even for
creating the tree model programmatically instead of reading
from text input.


The Parse API
-------------

The Parse API is quite similar to the Transform API, but leaves
out the rendering step.


### Parsing a Single Document

Reading from a file:

    val document = Parse as Markdown fromFile "hello.md"
    
Reading from a String:

    val input = "some *text* example"    
    val document = Parse as Markdown fromString input
    
The `document` instance gives you the full document model. See
the [Documents Scaladoc][doc-scaladoc] for details.

The `content` property of the `document` instance holds
the parsed content of the document in the form a tree 
of case classes. You can read the
[Elements Scaladoc][elements-scaladoc] for an overview of all node types.

You can then specify rewrite
rules to transform some nodes of the model (see [Document Tree Rewriting] for 
details) and use the Render API (explained below) to render
it to various output formats.


### Parsing an Entire Directory

    val tree = Parse as Markdown fromDirectory "source"
    
The tree instance is of type `DocumentTree` which gives you access
to all documents, templates and subdirectories contained in the
parsed directory.


### Parallel Execution

Like with a full transformation, parsing can be performed in parallel:

    Parse as Markdown fromTree Directory("source").inParallel

When specifying options like parallel execution, you cannot use
the `fromDirectory` shortcut like in the previous example, but
instead use the `Directory` entry point to the full configuration
API. Apart from parallel execution, it allows to specify a custom
template engine, a custom document type matcher, and more.


### Reusing Parsers

Like with the Transform API, all objects are reusable and immutable:

    val parse = Parse as ReStructuredText
    
    val doc1 = parse fromFile "input1.rst"
    val doc2 = parse fromFile "input2.rst"


### Character Encoding

Laika uses the same platform-dependent defaults for file encodings as the
IO classes in the Scala SDK. The most convenient way to specify an encoding
is via an implicit:

    implicit val codec:Codec = Codec.UTF8

This codec will then be used by `fromDirectory` and other methods 
shown in the examples above.


[doc-scaladoc]: ../api/#laika.tree.Documents$
[elements-scaladoc]: ../api/#laika.tree.Elements$
    

The Render API
--------------

The Render API is similar to the Transform API, but expects
that you already have a document tree model, either obtained
by a previous parsing step or built programmatically, and now
want to render it to a specific output format.


### Rendering a Single Document

Rendering to a file:

    // obtained from a parse step or created programmatically
    val doc: Document = ... 
    
    Render as HTML from doc toFile "hello.html"
    
Or to obtain the HTML as a string:

    val html = Render as HTML from doc toString
    

### Rendering an Entire Directory

    // obtained from a parse step or created programmatically
    val tree: DocumentTree = ...
    
    Render as HTML from tree toDirectory "path/to/target"
 
The `target` directory is expected to exist, while any required
subdirectories will be automatically created during rendering.


### Parallel Execution

Like with a full transformation, rendering can be performed in parallel:

    val tree: DocumentTree = ...
    
    Render as HTML from tree toTree Directory("source").inParallel


### Reusing Renderers

Like with the Transform API, all objects are reusable and immutable:

    val document = ...
    
    val render = Render as HTML from document
    
    render toFile "output1.html"
    render toFile "output2.html"
    

### Character Encoding

Laika uses the same platform-dependent defaults for file encodings as the
IO classes in the Scala SDK. The most convenient way to specify an encoding
is via an implicit:

    implicit val codec:Codec = Codec.UTF8

This codec will then be used by `toDirectory` and other methods 
shown in the examples above.

  