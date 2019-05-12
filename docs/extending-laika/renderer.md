
Implementing a Renderer
=======================

This document describes the best practices for adding an entirely new renderer to the toolkit.
It is only useful if you either plan to implement a renderer for an output format not
supported by Laika, want to replace one of the existing renderers, or are just
curious about the inner workings of the library. None of the information here is required
for standard usage of Laika.



API Contract
------------

A renderer has to implement the following trait:

    trait RenderFormat[FMT] {
      
      def fileSuffix: String
      
      def defaultTheme: Theme
      
      def defaultRenderer: (FMT, Element) => String
      
      def formatterFactory: RenderContext[FMT] => FMT
  
    }

The `fileSuffix` method returns the suffix to append when writing files in this format
(without the ".").

The `defaultTheme` specifies the theme to use when it is not overridden by the user.
It allows to specify a default template and/or static files to include in the output.
For details see the section about [Themes].

The `defaultRenderer` represents the actual renderer. It takes both, a formatter instance
and the element to render and returns a String in the target format.    
    
`formatterFactory` is the formatter instance for the target format. A new instance of this
formatter gets created for each render operation. `FMT` is a parameterized type representing 
the Formatter API specific to the output format. For the built-in renderers, this is `FOFormatter`
for the `XSLFO` renderer and `HTMLFormatter` for the `HTML` renderer.



The Render Function
-------------------

This `defaultRenderer` function should usually adhere to these rules:

* When given an element that is a container type that contains child elements (like `Paragraph`), it should never
  render the children itself, but instead delegate to the Formatter API, so that user-defined
  render functions can kick in for individual element types.
  
* It should expect unknown element types. Since parsers can also be extended, the document tree
  can contain nodes which are not part of the default node types provided by Laika. `Element` is *not*
  a sealed trait. Usually the parser
  should then also install renderers that know how to handle these nodes, but if it does not your
  renderer should not blow up. Often there is a sensible default, e.g. if you see an unknown
  element that mixes in `SpanContainer` and `Block` it is obviously similar to a regular
  Paragraph and may be rendered as such. In the worst case the renderer may choose to ignore
  such an element, but it should never blow up.
  
For character output your renderer may use the `TextFormatter` or `HTMLFormatter` APIs, which are
explained [here][Formatter API]. Alternatively it may create its own API, but you should keep in mind
then, that this API will also get used by users customizing specific nodes, so it should be
convenient and straightforward to use.

Finally, we'll show a minimal excerpt of the HTML render function we omitted above, just to
give you an impression that it is often quite simple to implement:

    def renderElement (fmt: HTMLFormatter, elem: Element): String = {
    
      elem match {
        case Paragraph(content,opt) => fmt.element("p", opt, content)
        
        case Emphasized(content,opt) => fmt.element("em", opt, content)
        
        /* [other cases ...] */
        
        /* [fallbacks for unknown elements] */
      }   
    }

As you see, the function never deals with children (the `content` attribute of many node
types) directly. Instead it passes them to the Formatter API which delegates to the composed
render function. This way customized renderers can kick in on every step of the recursion.

The various functions of the Formatter API are explained in the chapter
on the [Formatter API].


[Formatter API]: customize-rendering.html#the-formatter-apis
  