
Implementing a Renderer
=======================

This document describes the best practices for adding an entirely new renderer to the toolkit.
It is only useful if you either plan to implement a renderer for an output format not
supported by Laika, want to replace one of the existing renderers, or are just
curious about the inner workings of the library. None of the information here is required
for standard usage of Laika.



Factory Contract
----------------

The contract a renderer has to adhere to is not as simple as for parsers, but the implementation
is often quite straightforward nevertheless. A renderer has to mix in the following trait:

    trait RendererFactory[W] {
      
      def fileSuffix: String
      
      def defaultTheme: Theme
      
      def newRenderer (out: Output, root: Element, delegate: Element => Unit,
          styles: StyleDeclarationSet, config: RenderConfig): (W, Element => Unit)
  
    }

The `fileSuffix` method returns the suffix to append when writing files in this format
(without the ".").

The `defaultTheme` specifies the theme to use when it is not overridden by the user.
It allows to specify a default template and/or static files to include in the output.
For details see the section about [Themes].

The `newRenderer` method creates the actual renderer. In contrast to the parser factory
it creates a new function for each render operation.    
    
`Output` is a little IO abstraction provided by Laika so that you do not have to
deal with the details of whether the renderer writes to a string builder or file or other 
types of streams.

`Element => Unit` is the actual render function. The `delegate` function that gets passed to your
renderer is the *composed* render function. Since default renderers can be overridden by users
of your renderer as described in the chapter [Customizing Renderers], you need to use this function
as the delegate when your default render function needs to render the children of
an element. 

The render function you return in the tuple is the *default* render function
to use for all elements where no custom renderer has been defined. 

Finally, `W` is a parameterized type representing the Writer API that render functions
should use for writing the actual output. For the built-in renderers, this is `TextWriter`
for the `PrettyPrint` renderer and `HTMLWriter` for the `HTML` renderer.



The Render Function
-------------------

Finally you need to provide the actual default render function which we omitted in the example
above. This render function should usually adhere to these rules:

* When given an element that is a container type that contains child elements (like `Paragraph`), it should never
  render the children itself, but instead delegate to the Writer API, so that user-defined
  render functions can kick in for individual element types.
  
* It should expect unknown element types. Since parsers can also be extended, the document tree
  can contain nodes which are not part of the default node types provided by Laika. Usually the parser
  should then also install renderers that know how to handle these nodes, but if it does not your
  renderer should not blow up. Often there is a sensible default, e.g. if you see an unknown
  element that mixes in `SpanContainer` and `Block` it is obviously similar to a regular
  Paragraph and may be rendered as such. In the worst case the renderer may choose to ignore
  such an element, but it should never blow up.
  
For character output your renderer may use the `TextWriter` or `HTMLWriter` APIs, which are
explained [here][Writer API]. Alternatively it may create its own API, but you should keep in mind
then, that this API will also get used by users customizing specific nodes, so it should be
convenient and straightforward to use.

Finally, we'll show a little (simplified) excerpt of the HTML render function we omitted above, just to
give you an impression that it is often quite simple to implement:

    private def renderElement (out: HTMLWriter)(elem: Element): Unit = {
    
      elem match {
        case Paragraph(content,opt)  => 
          out <<@ ("p",opt)  << content << "</p>"  
        
        case Emphasized(content,opt) => 
          out <<@ ("em",opt) << content << "</em>" 
        
        /* [other cases ...] */
        
        /* [fallbacks for unknown elements] */
      }   
    }
    
As you see, the function never deals with children (the `content` attribute of many node
types) directly. Instead it passes them to the Writer API which delegates to the composed
render function.

The various functions of the Writer API (like `<<` or `<<@`) are explained in the chapter
on the [Writer API]. They are far less cryptic than at first sight and should be easy
to memorize once you know what they are supposed to do. The short symbols make render
functions easier to read. It's the only API where Laika uses symbols as method names.


[Writer API]: customize.html#writer
  
  