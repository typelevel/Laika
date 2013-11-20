
Separate Parsing and Rendering
==============================

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
    


  