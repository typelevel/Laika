
Supported Output Formats
========================

The current release supports HTML, PDF, XSL-FO and PrettyPrint.
Rendering happens from a generic document tree model shared between all parsers,
so that no renderer implementation has to understand specifics about a concrete
markup syntax like Markdown or reStructuredText.

Customization of the output is possible on two levels, first most formats (except
for PrettyPrint) can be styled with CSS. Secondly the rendering of specific nodes
can be overridden with a simple partial function as described in [Customizing Renderers].

Finally you can develop an entirely new renderer for a format not supported by Laika
out of the box. See chapter [Implementing a Renderer] for details.


HTML
----


PDF
---


XSL-FO
------


PrettyPrint
-----------

A renderer that visualizes the document tree structure, essentially a formatted
`toString` for a tree of case classes, mainly useful for testing and debugging
purposes.

You can use this renderer with the Transform API:

    val input = "some *text* example"
    
    Transform from Markdown to PrettyPrint fromString input toString
    
    res0: java.lang.String = Document - Blocks: 1
    . Paragraph - Spans: 3
    . . Text - 'some '
    . . Emphasized - Spans: 1
    . . . Text - 'text'
    . . Text - ' example'

Alternatively you can use the Render API to render an existing document:
    
    val input = "some *text* example"
    
    val doc = Parse as Markdown fromString input
    
    Render as PrettyPrint from doc toString

The above will yield the same result as the previous example.

Finally, if you are using the sbt plugin you can use the `laika:prettyPrint` task.
