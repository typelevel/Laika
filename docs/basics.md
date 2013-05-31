
Transformation Basics
=====================


There are three types of operations you can perform with Laika:

* A full transformation from input text written in a lightweight
  markup language like Markdown to output formats like HTML.
  
* A parse operation from input text written in a lightweight
  markup language like Markdown to an in-memory document tree
  model.
  
* A render operation from an in-memory document tree model
  to output formats like HTML.
  
  
If there is no need to decouple the parse and render step,
it is most convenient to use the transformation API.

The other operations are useful when you need to 
process the document tree between parse and render operations
or when they do not happen at the same time or even for
creating the tree model programmatically instead of reading
from text input.

The following sections describe these three operation types.


The Transform API
-----------------

Converting from Markdown to HTML using files as input and output:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"


Converting using Strings as input and output

    val input = "some *text* example"
    
    val result = Transform from Markdown to HTML fromString input toString


Other options are using `java.io.Reader/Writer` or streams. Of course they
can be combined in any way, like going from file as input to String as output.

You can also keep the created transformer instance to reuse it with different
inputs and outputs:

    val transform = Transform from ReStructuredText to HTML
    
    transform fromFile "hello.rst" toFile "hello.html"
    
    val result = transform fromString "some *text* example" toString

    
All objects created by the API are reusable and immutable. For example,
using the same input for two different types of output could be coded like this:

    val doc = Transform from ReStructuredText to HTML fromFile "hello.rst"
    
    doc toFile "hello.html"
    
    val res = doc toString

    
Laika also supports a PrettyPrint renderer which can be useful for debugging.
It visualizes the document tree model while shortening longer text spans
into one line:

    val input = "some *text* example"
    
    Transform from Markdown to PrettyPrint fromString input toString
    
    res0: java.lang.String = Document - Blocks: 1
    . Paragraph - Spans: 3
    . . Text - 'some '
    . . Emphasized - Spans: 1
    . . . Text - 'text'
    . . Text - ' example'


The Parse API
-------------

If you need to perform the parsing step separately, the API is similar to
the Transform API:

    val document = Parse as Markdown fromFile "hello.md"
    
This gives you the full document tree model. You can read the
[Scaladoc][tree-scaladoc] for an overview of all node types.

You can then specify rewrite
rules to transform some nodes of the model and use the Render API to render
it to various output formats, both described in sections below.


Like with the Transform API, all objects are reusable and immutable:

    val parse = Parse as ReStructuredText
    
    val doc1 = parse fromFile "input1.rst"
    val doc2 = parse fromFile "input2.rst"


[tree-scaladoc]: api/#laika.tree.Elements$
    

The Render API
--------------

If you need to perform the rendering step separately, the API is similar to
the Transform API:

    // obtained from a parse step or created programmatically
    val doc: Document = ... 
    
    Render as HTML from doc toFile "hello.html"
    
Or to obtain the HTML as a string:

    val html = Render as HTML from doc toString
    

Like with the Transform API, all objects are reusable and immutable:

    val document = ...
    
    val render = Render as HTML from document
    
    render toFile "output1.html"
    render toFile "output2.html"
    


  