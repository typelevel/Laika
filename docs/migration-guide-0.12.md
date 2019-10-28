
Migration Guide for Version 0.12
================================

The 0.12 release contains the most significant number of breaking changes
of any release since the 0.1 version in 2012.

**Using Laika's sbt plugin**
 
* If you do not use any of the customization hooks you can safely skip this guide.
* If you define custom rewrite rules, render functions or directives, 
  please see the corresponding sections below.

**Using Laika's Library API**

* It's probably safest you read all the sections of this guide as even the 
  high level APIs had changed.
* You can of course simply skip features you do not use.

This guide is written for users of the 0.9, 0.10 and 0.11 releases which had been compatible
with each other. 

If you migrate from an even earlier version, please start with the release
notes for 0.9 first https://github.com/planet42/Laika/releases/tag/0.9.0 


Motivation
----------

The following design goals led to the partial rewrite:

* Make the library a good citizen in a pure FP setup
* Stop throwing any exceptions in public APIs
* Stop hiding the side-effecting nature of some operations in blocking, synchronous APIs
* Build on top of cats-effect so that users get the familiar Bring-Your-Own Effect setup
* Avoid any kind of runtime reflection
* Make the feature set which had grown organically over the years more consistent and convenient
  to use
* Do many changes in one big, breaking release so that the road towards
  a final 1.0 release will be less bumpy from now on


New Separate laika-io Module
----------------------------

A subset of the existing `laika-core` module has been extracted into its own module,
`laika-io`. You need to add this new dependency if you use:

* File/Stream IO
* EPUB output
* Parallel transformations

The existing PDF support already had its own, separate artifact `laika-pdf` which
is also still available.

If you mostly do in-memory transformations from Markdown or reStructuredText to HTML
you can continue using the `laika-core` module.

The reasons for the split were:

* Keep features that depend on cats-effect separate so that the core module
  does not need this dependency
* Narrow the feature set of the core module to a point where it can realistically
  be fully supported for Scala.js in a later release (most likely 0.13)


Referential Transparency
------------------------

Laika is now supposed to be a good citizen in a pure FP setup when using its
library API. The following changes had been implemented for this purpose:

* No method in the public API throws any Exceptions anymore.
* The result of pure operations is provided by instances of `Either`.
* The result of side-effecting operations is provided by a return type of `F[A]`
  where `F[_]` is the standard Bring-Your-Own-Effect pattern, so that
  Laika can be used with cats-IO, Monix or Zio.
* The dependency on the Typesafe Config library and its impure Java API has
  been removed in favor of a new lightweight and pure HOCON parser provided
  by Laika, supporting the full spec except for file includes (for now).


Parser, Renderer and Transformer APIs
-------------------------------------

The changes were necessary for the following reasons:

* The reduced core module does no longer offer any IO-related functionality. 
* The return types of many methods had to change as Laika does no longer throw any Exceptions.
* The new IO module has its own builder APIs for constructing instances that work with
  the cats-effect type classes.

It's probably easiest to just show a few before/after examples below:

**Transforming from Markdown to HTML in-memory**

Before
```scala
val input = "some *text* example"

val transformer = Transform
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  
val res: String = transformer
  .fromString(input)
  .toString  
```

After
```scala
val input = "some *text* example"

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .build
  
val res: Either[ParserError, String] = transformer
  .transform(input)
```

**Transforming a directory of files from Markdown to HTML**

Before
```scala
val transformer = Transform
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  
val res: Unit = transformer
  .fromDirectory("src")
  .toDirectory("target")  
```

After (ensure you added the new `laika-io` dependency):
```scala
import laika.io.implicits._

implicit val cs: ContextShift[IO] = 
  IO.contextShift(ExecutionContext.global)
      
val blocker = Blocker.liftExecutionContext(
  ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
)
    
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
  
val res: IO[Unit] = transformer
  .fromDirectory("src")
  .toDirectory("target")
  .transform
```

Note that while the new code sample looks more verbose, it now gives you full
control over where your effects are run.


Customizing Renderers
---------------------

The renderer API had changed from a side-effecting API to a pure API.
Renderers now take AST elements and produce a string and will be invoked
recursively as before. This model allows for easier testing and pure code
at a minimal performance hit over the old design (< 10%).

Before
```scala
val transformer = Transform
  .from(Markdown)
  .to(HTML)
  .rendering { out => 
    { case Emphasized(content, _) => 
      out << """<em class="big">""" << content << "</em>" } 
  }
```

After
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .rendering {
    case (fmt, Emphasized(content, opt)) => 
      fmt.element("em", opt, content, "class" -> "big")
    }.build
```

The engine passes the following instances to the partial function:

* The formatter (`fmt`) which has a different API for each supported output format,
  in this case it will be an `HTMLFormatter`
* The current element of the rendered document AST which you can pattern match on  


Rewrite Rules
-------------

Here most of the changes are in the implementation which had been rewritten to avoid
any kind of runtime reflection. 

On the API surface, there are only two changes:
 
* The return type is more explicit (e.g. `Replace(newElement)` instead of `Some(newElement)`)
* Rules for rewriting spans and blocks get registered separately for increased
  type-safety, as it is invalid to replace a span with a block element.

```scala
val transformer = Transform
  .from(Markdown)
  .to(HTML)
  .usingRule {
    case Emphasized(content, opts) => Some(Strong(content, opts))
  }
```

After
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .usingSpanRule {
    case Emphasized(content, opts) => Replace(Strong(content, opts))
  }
  .build
```

Templating
----------

Variable substitutions now use HOCON syntax in line with the general deeper integration
with HOCON throughout Laika's feature set. 

The old syntax is still supported, 
but will be removed at some point before the 1.0 release.

The old reference style `{{some.ref}}` now becomes either `${some.ref}` for a required
reference or `${?some.ref}` for an optional one.


Directives
----------

Directives in 0.12 come with changes in both, the DSL for creating custom directives
and the supported syntax in markup and template files.

If you are not implementing your own directives, but only use the built-in ones
provided by Laika, you can skip the section on the DSL.

### Directive Syntax

* The separators for the attribute and body sections have changed
* HOCON syntax is now used for attributes
* The old syntax is still supported, but will be removed at some point before the 1.0 release

Before
```
@:if "layout.showSidebar":
  <div class="sidebar">...</div>

~else:
  <p>Something else</p>
```

After
```
@:if { layout.showSidebar }
<div class="sidebar">...</div>

@:else
<p>Something else</p>

@:@
```

### Directive DSL

The building blocks for creating your own directives have also changed significantly:

* `attribute(Default)` is now `defaultAttribute`
* `body` is now either `parsedBody` or `rawBody`
* Type conversions happen with the new `as` method: `attribute("title").as[String]`,
  based on the `ConfigDecoder` type class that is also used for the new Config API
* Named body parts have been replaced by the more flexible Separator Directives, 
  see [Separated Body] for details
* The built-in helpers for mapping directive parts with different arities has
  been replaced by cats `mapN`
  
Before
```scala
import laika.ast._
import Spans.dsl._

case class Note (title: String, 
                 content: Seq[Span], 
                 options: Options = NoOpt) extends Span 
                                           with SpanContainer[Note]
 
val spanDirective = Spans.create("note") {
  (attribute(Default) ~ body(Default)) (Note(_,_))
}  
```

After
```scala
import cats.implicits._
import laika.ast._
import Spans.dsl._

case class Note (title: String, 
                 content: Seq[Span], 
                 options: Options = NoOpt) extends Span 
                                           with SpanContainer[Note]
 
val spanDirective = Spans.create("note") {
  (defaultAttribute.as[String], parsedBody).mapN(Note(_,_))
}  
```

Note the additional import of cats implicits in the new version.

Config API
----------

Laika's support for HOCON configuration, originating either from configuration headers
in markup or template documents or separate configuration files had previously been
based on the Typesafe Config library and its Java API. Instances of `Config` were
available in the `Document` and `DocumentTree` types. 

This type is now Laika's own Config API, based on its own lightweight HOCON parser.
See the API docs for [laika.config.Config] for details.

[laika.config.Config]: http://planet42.github.com/Laika/api/laika/config/Config.html


Document Tree Model
-------------------

The model had been enhanced to better cater for Laika's support for e-book generation.

* The result of a tree parsing operation is now a new type called `DocumentTreeRoot`
* It has a `coverDocument` property and contains the recursive tree structure of the parsed content.
* Each `DocumentTree` in the structure now has an explicit `titleDocument: Option[Document]` property
  for a more explicit content organization in e-books.
* Properties that could previously hold references to streams and other impure data had been
  removed from the pure content model (e.g. `DocumentTree.staticDocuments`).  
  