
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
  
      def newRenderer (out: Output, delegate: Element => Unit): 
                                                      (W, Element => Unit)
      
    }
    
The `fileSuffix` method returns the suffix to append when writing files in this format
(without the ".").

The `newRenderer` method creates the actual renderer. In contrast to the parser factory
it creates a new function for each render operation.    
    
`Output` is a little IO abstraction provided by Laika so that you do not have to
deal with the details of whether the renderer writes to a string builder or file or other 
types of streams.

`Element => Unit` is the actual render function. The `delegate` function that gets passed to your
renderer is the *composed* render function. Since default renderers can be overridden by users
of your renderer
as described in the chapter [Customizing Renderers], you need to use this function
as the delegate when your default render function needs to render the children of
an element. 

The render function you return in the tuple is the *default* render function
to use for all elements where no custom renderer has been defined. 

Finally, `W` is a parameterized type representing the Writer API that render functions
should use for writing the actual output. For the built-in renderers, this is `TextWriter`
for the `PrettyPrint` renderer and `HTMLWriter` for the `HTML` renderer.



Providing an API
----------------

When you build a new renderer you should provide the following features for your users:

* An easy way to use your renderer with the Transform API

* An easy way to use it with the Render API

* A fluent API for specifying options (in case your renderer is configurable)

The first two come for free when you create an object that extends the setup function
explained in the previous section.
The built-in `PrettyPrint` or `HTML` object are an example. Since they do extend that function,
you can easily use them in expressions like this:

    val transform = Transform from Markdown to HTML
    
When you want to specify options this should be possible inline:

    val transform = Transform from Markdown to (MyFormat withOption "foo")

You can achieve this by providing a trait that offers all the available configuration
hooks and returns `this` for each of these methods for easy chaining. Additionally
you create a companion object that represents the default configuration.

This is how skeletons for the trait and object for the HTML renderer look as an example (Scaladoc
comments, imports and actual render logic removed for brevity):

    class HTML private (messageLevel: Option[MessageLevel]) 
                                      extends RendererFactory[HTMLWriter] {
    
      val fileSuffix = "html"
      
      def withMessageLevel (level: MessageLevel) = new HTML(Some(level))
      
      def newRenderer (output: Output, render: Element => Unit) = {
        val out = new HTMLWriter(output.asFunction, render)  
        (out, renderElement(out))
      }
      
      private def renderElement (out: HTMLWriter)(elem: Element): Unit = {
        /* actual render logic omitted */
      } 
    }
    
    object HTML extends HTML(None)

It calls `asFunction` on the `Output` instance which is the most convenient way
if all you need for writing is a simple `String => Unit` function, no matter
where the text is actually written to. Alternatively you can use
`Output.asWriter` to get access to the full `java.io.Writer` API.



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
  
  
  
  
  
  



