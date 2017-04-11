
Directives
==========

Directives are Laika's extension hook for both, templates and text markup.
They allow to add new tag-like constructs without touching the existing parsers.

Each directive expects a configurable combination of attributes or body elements,
where some of them may be required, while others may be optional. Laika takes care
of parsing and validating these directive parts based on your declarations
and then passes these values to your directive function (in a type-safe way)
that produces a node element to add to the document tree result.



Directive Types
---------------

Due to subtle differences in the node types they produce, there is a
distinction between three directive types:

* Template directives, which produce `TemplateSpan` elements (or alternatively simple strings
  if you do not mind that your directive can only be used with one output format)
* Directives for block elements in markup, which produce `Block` elements
* Directives for inline elements in markup, which produce `Span` elements

If you want to create a directive that can be used in all three locations
you can usually easily reuse parts of your logic. If your directive version
for inline elements generates a simple `ExternalLink` node for example,
you can simply wrap that same node in a `Paragraph` node for a block-level 
directive.



Directive Syntax
----------------

A very minimal example is the `toc` directive for inserting a table of contents.
Since all its attributes are optional, it can simply be used like this:

    @:toc.

A directive always starts with `@:` followed by the name of the 
directive. When there are no further attributes or body elements
you can close it with a `.`
    
A more complete example is the use of the `for` directive:

    @:for "document.sections": {
      <li><a href="#{{id}}">{{title.content}}</a></li>
    } 

Here `for` is the name of the directive, `"document.sections"` is an unnamed
attribute (where in this case the value is interpreted as a variable reference),
and finally, enclosed in curly braces the body of the directive. The opening
declaration with directive name and attributes ends with a `:` and not with `.` 
if it is followed by body elements.



Example Implementation
----------------------

You can browse the source of the built-in directives here: [directive-api]
But the logic required for the `toc` or `for` directives is probably much 
more complicated than what you would usually do for a little custom
helper tag that you need to unclutter your templates.

Therefore we'll show and explain a very simple example here that
should be sufficient to get you started.

Our `ticket` directive constructs an external link to our
bugtracker. Since the base URL is always the same, we want to
only write:

    @:ticket 456.
    
to get the output:

    <a href="http://tickets.cloud42.com/main-project/456">#456</a>
    
Or optionally specify a different project than the main one:

    @:ticket 456 project=pineapple.
    
to get the output:

    <a href="http://tickets.cloud42.com/pineapple/456">#456</a>

The following sections explain how to implement the tag
and then how to add it either to the Markdown or reStructuredText
parser or to the template parser.

[directive-api]: https://github.com/planet42/Laika/blob/master/core/src/main/scala/laika/directive/StandardDirectives.scala


### Directive Implementation

    val directive = Spans.create("ticket") {
      (attribute(Default) ~ attribute("project").optional) { 
        (ticketNo, project) => 
          val base = "http://tickets.cloud42.com/"
          val url = base + project.getOrElse("main-project") + "/" + ticketNo
          val linkText = Seq(Text("#"+ticketNo))
          ExternalLink(linkText, url, options = Styles("ticket"))
      }
    }
    
Let's examine the code:

With `Spans.create("ticket")` we specify the name of the directive (`ticket`)
which is also the name we are going to use in markup files (`@:ticket`).

The `Spans` object let's us create a directive for an inline element
as that has a different node type as the final result of the directive 
and the API is typesafe. For block level markup directives you'd use
the `Blocks` object, for template directives the `Templates` object.
The features and APIs are identical, but there are subtle differences
in return types.

With `attribute(Default)` we specify that we expect a default (unnamed)
attribute which is required. When a required attribute is missing
our directive function will never be invoked. Instead Laika inserts
a node of type `InvalidSpan` into the document tree (which we may
then choose to render or not).

With `attribute("project").optional` we specify that we expect an
attribute with the name `project` which is optional. In case it is 
missing our directive function will still be invoked.

The `~` method chains our declarations together. This operator is fairly
common in Scala lands, you've very likely seen it in other APIs already.

`(ticketNo, project)` are the parameters that the parser will pass
to our function after parsing and validating the directive. They
have a type corresponding to our configuration. `ticketNo` is of
type `String` (you can also specify converters to other types, but
that is not needed here). `project` is of type `Option[String]` since
we declared it as optional.

The rest is just plain Scala code. The result is of type `ExternalLink`
which is one type of Laika's rich tree model. It mixes in the `Span`
trait and thus satisfies the requirement of an inline directive.


### Directive Registration

Finally all we need to do is register our directive before parsing.
All the examples below refer to the `directive` variable we declared
in the example in the previous section.


**sbt Plugin**:

    import LaikaKeys._
    
    // ... your standard build stuff
    
    LaikaPlugin.defaults
    
    spanDirectives in Laika += directive
    
This registers the directive for both Markdown and reStructuredText
parsers. 

The directive implementation itself usually requires some
amount of logic, so is usually best kept separately in a `.scala`
file in the `project` directory and then referenced from your
`build.sbt`. Reusable directives are best packaged as a library
and then added as a dependency of your build.


**Markdown**:

    Transform from Markdown.withSpanDirectives(directive) to 
      HTML fromFile "hello.md" toFile "hello.html"   

The `directive` variable refers to the instance we created above.
We can now use our `ticket` directive in Markdown files!


**reStructuredText**:

    Transform from ReStructuredText.withLaikaSpanDirectives(directive) to 
      HTML fromFile "hello.md" toFile "hello.html"  

The only difference here is that the method name is `withLaikaSpanDirectives`
as reStructuredText comes with its own concept of directives which is only
supported in the reStructuredText parser.


**Templates**:

    Transform from Markdown to HTML fromDirectory 
      "source" withTemplateDirectives directive toDirectory "target" 

When you also want to use the directive in templates you need to create
a very similar declaration like the one above, just starting with `Templates.create`
instead and wrapping the `ExternalLink` node in a `TemplateElement`. You can extract
the rest of the logic into a shared function.

Therefore the type of the variable `directive` is `Templates.Directive`, and not
`Spans.Directive` like in the previous examples.



API Reference
-------------

The example implementation only showed a few options for declaring the
expected parts of your directive. Even though there is full scaladoc
for the API, the combinators and converters are a bit scattered over
different objects, so we assemble them here for easier reference.


### Default Attributes

A default attribute is an attribute without a name. 
It has to be the first attribute before any named attributes.
It allows for more concise syntax in all the cases where usually only one
attribute is used and its meaning is obvious.

Markup example:

    @:name arg.

Combinator:

    attribute(Default)
    
    
### Named Attributes

A named attribute can appear after the default attribute or right
after the directory name in case there is no default attribute.
The order of attributes does not matter, it does not need to match
the order you declared them in (that's why they have names after all).

Markup example:

    @:name myAttr=value.

Combinator:

    attribute("myAttr")


### Default Bodies

A default body is a body element without a name. 
It has to be the first body before any named body elements.

Markup example:

    @:name: { 
      this is the content of the default body 
    }

Combinator:

    body(Default)
    
    
### Named Bodies

A named body element usually appears after a default body (but it does not have
to). In the example below the second body has the name `empty`.

Markup example:

    @:for "documents.sections": { 
      <li>{{title.content}}</li>
    } ~empty: {
      <p>no sections</p>
    }

Combinator:

    body("empty")
    

### Optional Elements

All the element types shown above, default and named attributes as well
as default and named bodies can be marked as optional.

Combinator:

    body("empty").optional
    
The parameter type of your directive function changes accordingly,
from `T` to `Option[T]` where `T` is either the type returned by
your converter (see below) or the default type (which is different
for body and attribute elements (again see next section).


### Converters

For any attribute or body element a converter can be specified.
If it is left out the default types are as follows:

* `String` for attributes in all directive types
* `Seq[Block]` for body elements in block directives
* `Seq[Span]` for body elements in inline directives
* `Seq[TemplateSpan]` for body elements in template directives

This means that for body elements they are already parsed by default,
as this is the most likely requirement for a directive.

The object `Converters` in the `Directives` object contains several
pre-built converters, like `positiveInt`:

    attribute("depth", Converters.positiveInt).optional

It will validate the attribute value and only invoke your directive
function if it succeeds. The parameter type in your function changes
to `Option[Int]` accordingly.

If you want to implement your own converter, it simply has to
be a function of type `(Parser, String) => Result[T]` for which
a type alias `Converter[T]` exists.


### Access to the Parser

For block elements the default is to pre-parse the content for you,
so there is rarely a need to parse something yourself. But if you need to,
it wouldn't be a good idea to instantiate your own parser. Because it
would not have access to any of the directives or other configuration
options active for this operation (unless you manually duplicate it which
is brittle). Therefore you can request a parser for your function in 
addition to the other values:

    attribute(Default) ~ parser { (attrValue, parser) =>
      val parsedSpans = parser("["+attrValue+"]")
      SpanSequence(parsedSpans)
    }
    
In this contrived example the attribute value is modified before being passed
to the parser and then wrapped inside a sequence.


### Access to the Document Context

Finally you can also request access to the document context. This gives
you access to the structure, the title, sections and parent and root
trees, in short, the full API for the AST built by the parser.

It is, for example, required for a directive like the `toc` directive,
because for building a table of contents you have to look beyond your
particular directive node.

    attribute(Default) ~ context { (attrValue, context) =>
      val spans = Text("The title is: ") +: context.document.title
      SpanSequence(spans)
    }


### Differences between Directive Types

[Directive Types] already gave a quick overview over the available types.
The sections [Directive Implementation] and [Directive Registration] showed
a simple example for a span directive. Since there are three different directive
types and different ways to register them depending on whether you use the 
sbt plugin or Laika embedded, this section gives a final overview over the
API differences.


### Span Directives

Use: in inline elements in text markup files

Implementation:

    import laika.directive.Directives.Spans
    import laika.util.Builders._
    import laika.tree.Elements._
    import Spans.Combinators._
    import Spans.Converters._
    
    val directive = Spans.create("name") {
      // implementation producing a `Span` element
    }    
    
Registration:

    // for Markdown and reStructuredText with sbt plugin:
    spanDirectives in Laika += directive // in build.sbt

    // for Markdown with Transform API or Parse API:
    Transform from Markdown.withSpanDirectives(directive) to ...
    Parse as Markdown.withSpanDirectives(directive) from ...
    
    // for reStructuredText with Transform API or Parse API:
    Transform from ReStructuredText.withLaikaSpanDirectives(directive) to ...
    Parse as ReStructuredText.withLaikaSpanDirectives(directive) from ...


### Block Directives

Use: in block elements in text markup files

Implementation:

    import laika.directive.Directives.Blocks
    import laika.util.Builders._
    import laika.tree.Elements._
    import Blocks.Combinators._
    import Blocks.Converters._
    
    val directive = Blocks.create("name") {
      // implementation producing a `Block` element
    }    
    
Registration:

    // for Markdown and reStructuredText with sbt plugin:
    blockDirectives in Laika += directive // in build.sbt

    // for Markdown with Transform API or Parse API:
    Transform from Markdown.withBlockDirectives(directive) to ...
    Parse as Markdown.withBlockDirectives(directive) from ...
    
    // for reStructuredText with Transform API or Parse API:
    Transform from ReStructuredText.withLaikaBlockDirectives(directive) to ...
    Parse as ReStructuredText.withLaikaBlockDirectives(directive) from ...


### Template Directives

Use: in template files

Implementation:

    import laika.directive.Directives.Templates
    import laika.util.Builders._
    import laika.tree.Templates._
    import Templates.Combinators._
    import Templates.Converters._
    
    val directive = Templates.create("name") {
      // implementation producing a `TemplateSpan` element
    }    
    
Registration:

    // for templates with sbt plugin:
    templateDirectives in Laika += directive // in build.sbt

    // for Markdown in Transform API:
    Transform from Markdown to HTML fromDirectory 
      "source" withTemplateDirectives directive toDirectory "target" 

    // for reStructuredText in Transform API:
    Transform from ReStructuredText to PDF fromDirectory 
      "source" withTemplateDirectives directive toFile "out.pdf" 
        

