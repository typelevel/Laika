
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

    @:toc

A directive always starts with `@:` followed by the name of the directive.
    
A more complete example is the use of the `for` directive:

    @:for { some.list.of.products }
    <li><a href="#${_.id}">${_.description}</a></li>
    @:@ 

Here `for` is the name of the directive, `some.list.of.products` is an unnamed
attribute (where in this case the value is interpreted as a variable reference,
a path the user has defined in the configuration),
and finally the body of the directive followed by the `@:@` fence. 


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

    @:ticket {456}
    
to get the output:

    <a href="http://tickets.cloud42.com/main-project/456">#456</a>
    
Or optionally specify a different project than the main one:

    @:ticket {456, project=pineapple}
    
to get the output:

    <a href="http://tickets.cloud42.com/pineapple/456">#456</a>

The following sections explain how to implement the tag
and then how to add it either to the Markdown or reStructuredText
parser or to the template parser.

[directive-api]: https://github.com/planet42/Laika/blob/master/core/src/main/scala/laika/directive/StandardDirectives.scala


### Directive Implementation

    val directive = Spans.create("ticket") {
      val ticketAttr = defaultAttribute.as[String]
      val projectAttr = attribute("project").as[String].optional
      (ticketAttr, projectAttr).mapN { (ticketNo, project) => 
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

With `defaultAttribute.as[String]` we specify that we expect a default (unnamed)
attribute which is required. When a required attribute is missing
our directive function will never be invoked. Instead Laika inserts
a node of type `InvalidSpan` into the document tree (which we may
then choose to render or not).

With `attribute("project").as[String].optional` we specify that we expect an
attribute with the name `project` which is optional. In case it is 
missing our directive function will still be invoked.

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

    laikaSpanDirectives += directive
    
This registers the directive for both Markdown and reStructuredText
parsers. 

The directive implementation itself usually requires some
amount of logic, so is usually best kept separately in a `.scala`
file in the `project` directory and then referenced from your
`build.sbt`. Reusable directives are best packaged as a library
and then added as a dependency of your build.


**Library API**:

    object MyDirectives extends DirectiveRegistry {
      val spanDirectives = Seq(directive)
      val blockDirectives = Seq()
      val templateDirectives = Seq()
    }

    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .using(MyDirectives)
      .build

The `directive` variable refers to the instance we created above.
We can now use our `ticket` directive in Markdown files!


**Templates**:

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

    @:name { arg }

Combinator:

    defaultAttribute

    
### Named Attributes

A named attribute can appear after the default attribute or right
after the directory name in case there is no default attribute.
The order of attributes does not matter, it does not need to match
the order you declared them in (that's why they have names after all).

The syntax for attributes is HOCON and it supports the full spec except
for file includes.

Markup example:

    @:name { myAttr=value }

Combinator:

    attribute("myAttr")


### All Attributes

If you want the full flexibility of accepting any kind of attribute without
knowing the expected names upfront, you can use the `allAttributes` combinator.

    val directive = Spans.create("custom") {
      (allAttributes, body).mapN { (attributes, bodyContent) => 
        val path = attributes.getOpt[Path]("filePath")
        val index = attributes.getOpt[Int]("index")
        ...
      }
    }

This combinator gives you an instance of `Config` as a result, Laika's configuration API, 
and you can manually inspect the values it contains. This naturally leaves the burden
of validation and error handling (e.g. for required values or type conversions)
with the implementor of the directive. It should therefore only be used when
this level of flexibility is really required.


### Body

The body is an element containing text markup that can follow the attribute section. 

Markup example:

    @:name
    
    this is the content of the body 
    
    @:@

Example with a custom fence:

    @:name { foo=bar } ^^^
    
    this is the content of the body 
    
    ^^^
    
A custom fence is only supported for block directives and can be used for disambiguation
in case of nesting a directive inside another. It has to consist of exactly 3 characters
on the same line right after the directive declaration. 

For span and template directives this
is not necessary as the parser can figure the nesting hierarchy on its own.

The type of the result this combinator will produce depends on the type of the directive:
In a template directive it is `Seq[TemplateSpan]`, in a block directive it is `Seq[Block]`,
and finally, in a span directive it is `Seq[Span]`. 

Combinator:

    parsedBody
    
There is an alternative combinator if you need the raw, unparsed body as a String:

    rawBody    


### Separated Body

A separated body element is divided into multiple sub-sections by a special kind of directive,
called a separator directive. It is a fairly advanced feature and probably not something you'd
need often. 

For an example from the built-in standard directives, let's have a look at the `@:if` directive:

    @:if { showSidebar }
    <div class="sidebar">...</div>
    
    @:elseIf { showInfobox }
    <div class="infobox">...</div>
    
    @:else
    <p>This document does not have any sections</p>
    
    @:@
    
The root `@if` directive is the parent directive in this case, and both the `@:elseIf` and `@:else`
directives are separator directives that partition the body. Separator directives are different
than normal directives in that they do not need to produce an AST element (e.g. `Block` or `Span`)
as they will be passed to the parent directive for processing which then will produce the target AST element.

If you want to see a full example of such a directive, have a look at the implementation of the `@:if` directive
in the `StandardDirectives` source.

In this section we'll just show a very small, contrived example. Declaring a separator directive looks just
like declaring a normal directive, only that you call `separator` instead of `create`:

    case class Child (content: Seq[Span])
    
    val sepDir = Spans.separator("child", min = 1) { body map Foo }   

Here you specify the name of the directive `@:child`, as well as that it has to be present in 
the body at least once. Then you use the regular combinators to declare the expected directive
parts, in this case only a body that you map to the `Child` type.

Now you can use this directive in the parent:

    val directive = Spans.create("parent") { 
      separatedBody(Seq(sepDir)) map { multipart =>
        val seps = multipart.children.flatMap { sep => 
          Text("Child: ") +: sep.content 
        }
        SpanSequence(multipart.mainBody ++ seps)
      }
    }

You can use the `separatedBody` combinator where you pass all expected child directives (in this
case only one) and then map the resulting `Multipart` instance to an AST element. The `Multipart`
gives you access to the main body as well as all processed separator directives in `multipart.children`.

This entire directive can then be used like this:

    @:parent
    This is the main body
    
    @:child
    This is the body of the separator
    
    @:@
    

### Optional Elements

Default and named attributes can be marked as optional.

Combinator:

    attribute("width").as[Int].optional

The parameter type of your directive function changes accordingly,
from `T` to `Option[T]` where `T` is either the type returned by
your converter (see below) or the default type.


### Inherited Elements

By default directives only accept attributes defined right in the
attribute section of the directive to avoid name clashes with attributes
in other scopes. If you want to explicitly enable inheritance, so that
a user can define default values for attributes either in the configuration
header of the markup or template files or in the `directory.conf` file,
you can set the `inherited` flag:

Combinator:

    attribute("width").as[Int].inherited

With this flag set, the `width` attribute can be inherited from other scopes
if it is not defined in the directive itself.


### Decoders

You can specify a decoder for all attributes with the `as[T]`
method:

    attribute("depth").as[Int].optional

Without a decoder the result type would be `ConfigValue`
which is a data structure similar to those of popular JSON libraries.
But that type is rarely the most convenient type, which is why
Laika defines some basic decoders out of the box, for `String`,
`Int`, `Double`, `Boolean` and `Path`.

You can define your own by implementing `ConfigDecoder[T]`.


### Access to the Parser

For block elements the default is to pre-parse the content for you,
so there is rarely a need to parse something yourself. But if you need to,
it wouldn't be a good idea to instantiate your own parser. Because it
would not have access to any of the directives or other configuration
options active for this operation (unless you manually duplicate it which
is brittle). Therefore you can request a parser for your function in 
addition to the other values:

    (defaultAttribute, parser).mapN { (attrValue, parser) =>
      val parsedSpans = parser("["+attrValue+"]")
      SpanSequence(parsedSpans)
    }

In this contrived example the attribute value is modified before being passed
to the parser and then wrapped inside a sequence.


### Access to the Document Cursor

Finally you can also request access to the document context. This gives
you access to the structure, the title, sections and parent and root
trees, in short, the full API for the AST built by the parser.

It is, for example, required for a directive like the `toc` directive,
because for building a table of contents you have to look beyond your
particular directive node.

    (defaultAttribute, cursor).mapN { (attrValue, cursor) =>
      val spans = Text("The title is: ") +: cursor.target.title
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

    import cats.implicits._
    import laika.ast._
    import laika.directive.Spans
    import Spans.dsl._
    
    val directive = Spans.create("name") {
      // implementation producing a `Span` element
    }    

Registration:

    // for Markdown and reStructuredText with sbt plugin:
    laikaSpanDirectives += directive // in build.sbt

    // for Markdown and reStructuredText with Transformer or Parser API:
    object MyDirectives extends DirectiveRegistry {
      val spanDirectives = Seq(directive)
      val blockDirectives = Seq()
      val templateDirectives = Seq()
    }
    
    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .using(MyDirectives)
      .build

    val parser = Parser
      .of(Markdown)
      .using(MyDirectives)
      .build    


### Block Directives

Use: in block elements in text markup files

Implementation:

    import cats.implicits._
    import laika.ast._
    import laika.directive.Blocks
    import Blocks.dsl._
    
    val directive = Blocks.create("name") {
      // implementation producing a `Block` element
    }

Registration:

    // for Markdown and reStructuredText with sbt plugin:
    laikaBlockDirectives += directive // in build.sbt

    // for Markdown and reStructuredText with Transformer or Parser API:
    object MyDirectives extends DirectiveRegistry {
      val spanDirectives = Seq()
      val blockDirectives = Seq(directive)
      val templateDirectives = Seq()
    }
    
    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .using(MyDirectives)
      .build

    val parser = Parser
      .of(Markdown)
      .using(MyDirectives)
      .build    


### Template Directives

Use: in template files

Implementation:

    import cats.implicits._
    import laika.ast._
    import laika.directive.Templates
    Templates Blocks.dsl._
    
    val directive = Templates.create("name") {
      // implementation producing a `TemplateSpan` element
    }    

Registration:

    // for templates with sbt plugin:
    laikaTemplateDirectives += directive // in build.sbt

    // for Markdown and reStructuredText with Transformer or Parser API:
    object MyDirectives extends DirectiveRegistry {
      val spanDirectives = Seq()
      val blockDirectives = Seq()
      val templateDirectives = Seq(directive)
    }
    
    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .using(MyDirectives)
      .build

    val parser = Parser
      .of(Markdown)
      .using(MyDirectives)
      .build    
