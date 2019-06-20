
Customizing Renderers
=====================

In some cases you might want to override the output of a renderer for a few types
of document tree nodes only, while keeping the default for the rest. Both the
Transform and Render APIs offer a hook to easily do that without modifying
or extending the existing renderer. 

This is the signature of a custom renderer hook:

    PartialFunction[(Formatter, Element), String]
    
`Formatter` is a generic type representing the formatting API which is different for each 
output format. For HTML it is `HTMLFormatterr`, for XSL-FO it is `FOFormatter`. 
This way renderers can offer the most convenient API for a specific output format.
It provides a helper API for rendering tags, adds the current indentation level after
line breaks and knows how to render child elements. 

`Element` is the base type of the document AST and represent the AST node to render, `String`
is the render result in the expected target format.
  
  
  
Defining a Render Function
--------------------------

This section explains how a render function is implemented and the subsequent sections
show the three different ways to register such a function.

In the following example only the HTML output for emphasized text will be modified,
adding a specific style class:

    val renderer: PartialFunction[(HTMLFormatter, Element), String] = {
      case (fmt, Emphasized(content, opt)) => 
        fmt.element("em", opt, content, "class" -> "big") 
    }

For all node types where the partial function is not defined, the default renderer
will be used.

Multiple custom renderers can be specified for the same transformation, they will be 
tried in the order you added them, falling back to the default in case none is defined 
for a specific node.

The `content` value above is of type `Seq[Span]`. A renderer should only ever render
a single node and delegate to the formatter for rendering children. Only the formatter
has a list of all installed render extensions as well as the base renderer and will
delegate to those function where the partial function is defined for the child element. 



Registering a Render Function
-----------------------------

The mechanism is slightly different, depending on whether you are using the sbt
plugin or Laika as a library in an application. In the latter case you have two
choices, one for performing a full transformation, the other for a separate
render operation. All three options are described below.


### Using the sbt Plugin

In `build.sbt`:

    import laika.ast._
    
    laikaSiteRenderers += laikaSiteRenderer {
      case (fmt, Emphasized(content, opt)) => 
        fmt.element("em", opt, content, "class" -> "big")
    }

    
### Using the Transform API

    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .rendering {
        case (fmt, Emphasized(content, opt)) => 
          fmt.element("em", opt, content, "class" -> "big")
      }.build


### Using the Render API

    val doc: Document = ...
    
    val renderer = Renderer
      .of(HTML)
      .rendering { 
        case (fmt, Emphasized(content, opt)) => 
          fmt.element("em", opt, content, "class" -> "big")
      }.build



The Formatter APIs
------------------

Each formatter provides functionality that is specific to the target format.
On top of that all formatters manage the indentation level after line breaks and 
know how to delegate to the relevant renderers for child elements.


### TextFormatter

This is the base API supported by both the `XSL-FO` and `HTML` renderer,
both of them adding several methods with rendering logic specific to that format.

* `newLine` renders a newline character followed by whitespace for the current level of indentation

* `child` render a single child element.

* `childPerLine` renders a list of child elements, each on a separate line, 
  with the current level of indentation

* `indentedChildren` renders a list of child elements, each on a separate line, 
  indented one level to the right from the current indentation level



### HTMLFormatter

This formatter supports all methods of the `TextFormatter` API shown above, and adds
the following methods:

* `element` renders a tag where the specified list of child elements will be used to render the content of the tag

* `indentedElement` is similar to `element`, but renders the child elements one per line and indented to the right
  
* `textElement` renders an element with text content with special characters replaced by entities

* `emptyElement` renders an empty tag



### Themes

A theme is a collection of customized renderers as shown in the previous sections,
plus optionally default templates and/or static files to be copied to the output
directory.

This is the signature of the `Theme` case class:

    case class Theme (customRenderer: PartialFunction[(Formatter, Element), String],
                      defaultTemplate: Option[TemplateRoot],
                      defaultStyles: StyleDeclarationSet)

* The `customRenderer` is a renderer function that overrides the built-in renderers
  for one or more nodes, see the sections above for details on how to write such a function.
  
* The `defaultTemplate` is the AST for the template to embed the render result in, 
  overriding the default template of the library. You can create the AST programmatically
  or use Laika's templating syntax and parse it from a file or the resource directory if
  you bundle your theme in a jar:
  
      DefaultTemplateParser.parse(Input.fromClasspath(
        "/templates/default.template.html", Root / "default.template.html"
      )) 

* The `defaultStyles` allow you to customize the default CSS for a format. This
  is currently only processed for PDF, as it is the only format where Laika processes
  the CSS and applies it to the render result. If you want to add CSS for HTML or EPUB, 
  add them as static files instead, as Laika will just copy them over without processing them.


A theme can be installed as part of an extension bundle:

    object MyExtensions extends ExtensionBundle {
    
      override val themes = Seq(HTML.Theme(...), PDF.Theme(...))
        
    }
    
    val transformer = Transformer.from(Markdown).to(HTML).using(MyExtensions)

A theme is specific to a particular output format, separate instances need to be 
provided if you want to install themes for several formats like HTML, PDF or EPUB.
