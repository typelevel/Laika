
Migration Guide
===============

Laika has not reached the 1.0 status yet and as a consequence, minor API changes can still be expected on the road
towards 1.0.

Most of the content in this guide is not relevant if you either use the integrated sbt plugin or the library API
without any lower-level customizations.

The only major change of the top-level library API in 8 years had been the 0.12.0 release, which has very detailed
migration instructions below.


Versions older than 0.18.0
--------------------------

Version 0.18 is based on cats-effect 3, and the API changes in that library had to surface in API changes in Laika
in some places.
If you either use the sbt plugin or just the pure `laika-core` module, you are not affected.

But users of the library API in either `laika-io` or `laika-pdf` need to make an easy, minor change:

Before:

```scala
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
```

After:

```scala mdoc:compile-only
import cats.effect.IO
import laika.api._
import laika.format._
import laika.io.implicits._
import laika.markdown.github.GitHubFlavor

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .parallel[IO]
  .build
```

The manual creation of `ContextShift` and `Blocker` instances is no longer needed,
and consequently the `.io(blocker)` builder step is gone, too.

If required, control over the thread pool configuration is still available centrally, e.g. in `IOApp`.


Versions older than 0.16.0
--------------------------

The 0.16 release introduced theme support and as a consequence all parsers, renderers and transformers are now
provided as a cat-effect `Resource`.

If you are using the sbt plugin, you are not affected by this API change.
When using the library API the necessary code changes are quite trivial. Change:

```scala
// build your transformer like before
val transformer = ???

transformer
  .fromDirectory("docs")
  .toDirectory("target")
  .transform
```

to

```scala mdoc:compile-only
import cats.effect.{ IO, Resource }
import laika.io.api.TreeTransformer

// build your transformer like before
def transformer: Resource[IO, TreeTransformer[IO]] = ???

transformer.use {
  _.fromDirectory("docs")
   .toDirectory("target")
   .transform
}
```

Ensure that you create the transformer only once at application start to avoid unnecessary overhead and memory usage 
for repeated use.

Finally, the legacy directive syntax which had been deprecated in the 0.12 release, has now been removed. 

Versions older than 0.15.0
--------------------------

If you plan to migrate to 0.15 it is recommended to address all deprecation warnings the compiler admits
as this is the last release where they'll be supported.

This version introduces namespaces for the built-in substitution variables to avoid name clashes with
user-defined variables.
If you use them in your markup files or templates you need to adjust them for this release:

* `document.*` becomes `cursor.currentDocument.*`

* All configuration variables are now prefixed with `laika`, e.g. `laika.title`, `laika.navigationOrder`.

* `cursor.*` and `laika.*` are the only reserved namespaces, any other prefix can be freely chosen.

If you upgrade from a version older than 0.12.0 it is also recommended to switch to the new directive syntax
described below as this is a functional area that unfortunately does not emit any compiler warnings.


Versions older than 0.14.0
--------------------------

* If you use the document AST: the `Path` API had been split into `Path` for absolute paths only and `RelativePath`
  for pointing from one document to another.
  
* If you developed parser extensions there are new entry points for defining a span or block parser.
  You can address all deprecation warnings you might get for your parser definitions by following the new
  guide for [Writing Parser Extensions].


Versions older than 0.12.0
--------------------------

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

This section is written for users of the 0.9, 0.10 and 0.11 releases which had been compatible with each other. 

If you migrate from an even earlier version, please start with the 
[release notes for 0.9](https://github.com/planet42/Laika/releases/tag/0.9.0) first.


**Motivation**

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


**New Separate laika-io Module**

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


**Referential Transparency**

Laika is now supposed to be a good citizen in a pure FP setup when using its
library API. The following changes had been implemented for this purpose:

* No method in the public API throws any Exceptions anymore.
* The result of pure operations is provided by instances of `Either`.
* The result of side-effecting operations is provided by a return type of `F[A]`
  where `F[_]` is the standard Bring-Your-Own-Effect pattern, so that any effect implementing
  the cats-effect typeclasses (`Sync` or `Async`, depending on use case) can be used.
* The dependency on the Typesafe Config library and its impure Java API has
  been removed in favor of a new lightweight and pure HOCON parser provided
  by Laika, supporting the full spec.


**Parser, Renderer and Transformer APIs**

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

```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.parse.markup.DocumentParser.TransformationError

val input = "some *text* example"

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .build
  
val res: Either[TransformationError, String] = transformer
  .transform(input)
```


**Transforming a directory of files from Markdown to HTML**

Before

```scala
val transformer = Transform
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .build
  
val res: Unit = transformer
  .fromDirectory("src")
  .toDirectory("target")  
```

After (ensure you added the new `laika-io` dependency):

```scala mdoc:compile-only
import cats.effect.IO
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.io.implicits._

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .parallel[IO]
  .build
  
val res: IO[Unit] = transformer.use {
  _.fromDirectory("src")
   .toDirectory("target")
   .transform
   .void
}
```

Note that while the new code sample looks more verbose, it now gives you full
control over where your effects are run.


**Customizing Renderers**

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

```scala mdoc:compile-only
import laika.ast.Emphasized
import laika.api._
import laika.format._

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


**Rewrite Rules**

Here most of the changes are in the implementation which had been rewritten to avoid
any kind of runtime reflection. 

On the API surface, there are only two changes:
 
* The return type is more explicit (e.g. `Replace(newElement)` instead of `Some(newElement)`)
* Rules for rewriting spans and blocks get registered separately for increased
  type-safety, as it is invalid to replace a span with a block element.

@:pageBreak

Before

```scala
val transformer = Transform
  .from(Markdown)
  .to(HTML)
  .usingRule {
    case Emphasized(content, opts) => Some(Strong(content, opts))
  }
```

After

```scala mdoc:compile-only
import laika.ast._
import laika.api._
import laika.format._

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .usingSpanRule {
    case Emphasized(content, opts) => Replace(Strong(content, opts))
  }
  .build
```

**Templating**

Variable substitutions now use HOCON syntax in line with the general deeper integration
with HOCON throughout Laika's feature set. 

The old syntax is still supported, 
but will be removed at some point before the 1.0 release.

The old reference style `{{some.ref}}` now becomes either `${some.ref}` for a required
reference or `${?some.ref}` for an optional one.


**Directives**

Directives in 0.12 come with changes in both, the DSL for creating custom directives
and the supported syntax in markup and template files.

If you are not implementing your own directives, but only use the built-in ones
provided by Laika, you can skip the section on the DSL.

**Directive Syntax**

* The separators for the attribute and body sections have changed
* HOCON syntax is now used for named attributes between curly braces or a plain string for an 
  unnamed attribute in parenthesis
* The old syntax is still supported, but will be removed at some point before the 1.0 release

@:pageBreak

Before

```laika-html
@:if "layout.showSidebar":
  <div class="sidebar">...</div>

~else:
  <p>Something else</p>
```

After

```laika-html
@:if(layout.showSidebar)
<div class="sidebar">...</div>

@:else
<p>Something else</p>

@:@
```

**Directive DSL**

The building blocks for creating your own directives have also changed significantly:

* `attribute(Default)` is now `attribute(0)`
* `body` is now either `parsedBody` or `rawBody`
* Type conversions happen with the new `as` method: `attribute("title").as[String]`,
  based on the `ConfigDecoder` type class that is also used for the new Config API
* Named body parts have been replaced by the more flexible [Separator Directives]. 
* The built-in helper for mapping directive parts with different arity has
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

@:pageBreak

After

```scala mdoc:compile-only
import cats.implicits._
import laika.ast._
import laika.directive.Spans
import Spans.dsl._

case class Note (title: String, 
                 content: Seq[Span], 
                 options: Options = NoOpt) extends Span 
                                           with SpanContainer {
  type Self = Note  
  def withOptions(options: Options): Self = copy(options = options)  
  def withContent(content: Seq[Span]): Self = copy(content = content)                                  
}
 
val spanDirective = Spans.create("note") {
  (attribute(0).as[String], parsedBody).mapN(Note(_,_))
}  
```

Note the additional import of cats implicits in the new version.

**Config API**

Laika's support for HOCON configuration, originating either from configuration headers
in markup or template documents or separate configuration files had previously been
based on the Typesafe Config library and its Java API. Instances of `Config` were
available in the `Document` and `DocumentTree` types. 

This type is now Laika's own Config API, based on its own lightweight HOCON parser.
See the API docs for [laika.config.Config] for details.

[laika.config.Config]: http://planet42.github.com/Laika/api/laika/config/Config.html


**Document Tree Model**

The model had been enhanced to better cater for Laika's support for e-book generation.

* The result of a tree parsing operation is now a new type called `DocumentTreeRoot`
* It has a `coverDocument` property and contains the recursive tree structure of the parsed content.
* Each `DocumentTree` in the structure now has an explicit `titleDocument: Option[Document]` property
  for more explicit content organization in e-books.
* Properties that previously held references to streams and other impure data had been
  removed from the pure content model (e.g. `DocumentTree.staticDocuments`).  
  