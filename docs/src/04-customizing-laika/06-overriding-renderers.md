
Overriding Renderers
====================

In some cases you might want to override the output of a renderer for a few types of AST nodes only,
while keeping the default for the rest. 
Both the sbt plugin and the library API offer a hook to easily do that 
without modifying or extending the existing renderer. 

In general this processing hook is intended for cases where the customization you intend to do is coupled
to a specific output format. 
For any more generic processing logic, it is recommended to consider [AST Rewriting] instead,
as that hook allows to replace or remove individual nodes in the AST before rendering.
This way the same logic can be used for all supported output formats.


Function Signature
------------------

This is the signature of a custom renderer hook:

```scala
PartialFunction[(Formatter, Element), String]
```

`Formatter` is a generic type representing the formatting API which is different for each output format. 
For HTML it is `HTMLFormatterr`, for XSL-FO it is `FOFormatter`. 
It provides a helper APIs for rendering tags, adds the current indentation level after line breaks 
and knows how to render child elements. 

`Element` is the base type of the document AST and represents the AST node to render, 
`String` is the render result in the expected target format.

See [The Document AST] for more details about the various node types


Defining a Render Function
--------------------------

This section explains how a render function is implemented and the subsequent sections
show the three different ways to register such a function.

In the following example only the HTML output for emphasized text will be modified,
adding a specific style class:

```scala mdoc:silent
import laika.ast._
import laika.render.HTMLFormatter

val renderer: PartialFunction[(HTMLFormatter, Element), String] = {
  case (fmt, Emphasized(content, opt)) => 
    fmt.element("em", opt, content, "class" -> "big") 
}
```

For all node types where the partial function is not defined, the default renderer will be used.

Multiple custom renderers can be specified for the same transformation, 
they will be tried in the order you added them, 
falling back to the default in case none is defined for a specific node.

The `content` value above is of type `Seq[Span]`. 
A renderer should only ever render a single node and delegate to the formatter for rendering children. 
Only the formatter has a list of all installed render extensions as well as the base renderer
and will delegate to those functions where the partial function is defined for the child element.


Registering a Render Function
-----------------------------

Since it is one of the most likely extension points used in user-code, there is a direct shortcut for passing
it to the Laika configuration as shown below.

In case you want to combine it with other extensions, a render override can also be defined as part of an
`ExtensionBundle` (see [The ExtensionBundle API] for details).


@:select(config)

@:choice(sbt)

```scala mdoc:invisible
import laika.sbt.LaikaPlugin.autoImport._
```

```scala mdoc:compile-only
import laika.ast._

laikaExtensions += laikaHtmlRenderer {
  case (fmt, Emphasized(content, opt)) => 
    fmt.element("em", opt, content, "class" -> "big")
}
```

@:choice(library)

**Using the Transformer API**

```scala mdoc:silent
import laika.api._
import laika.ast._
import laika.format._

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .rendering {
    case (fmt, Emphasized(content, opt)) => 
      fmt.element("em", opt, content, "class" -> "big")
  }.build
```

**Using the Renderer API**

```scala mdoc:compile-only
def doc: Document = ???

val renderer = Renderer
  .of(HTML)
  .rendering { 
    case (fmt, Emphasized(content, opt)) => 
      fmt.element("em", opt, content, "class" -> "big")
  }.build
```

@:@


The Formatter APIs
------------------

Each formatter provides functionality that is specific to the target format.
On top of that all formatters manage the indentation level after line breaks and 
know how to delegate to the relevant renderers for child elements.


### TextFormatter

This is the base API supported by both the `XSL-FO` and `HTML` renderer,
both of them adding several methods with rendering logic specific to that format.

* `newLine` renders a newline character followed by whitespace for the current level of indentation.

* `child` render a single child element.

* `childPerLine` renders a list of child elements, each on a separate line, with the current level of indentation.

* `indentedChildren` renders a list of child elements, each on a separate line, 
  indented one level to the right from the current indentation level.


### HTMLFormatter

This formatter supports all methods of the `TextFormatter` API shown above, and adds the following methods:

* `element` renders a tag where the specified list of child elements will be used to render the content of the tag

* `indentedElement` is similar to `element`, but renders the child elements one per line and indented to the right
  
* `textElement` renders an element with text content with special characters replaced by entities

* `emptyElement` renders an empty tag
