
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
  .using(Markdown.GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
```

After:

```scala mdoc:compile-only
import cats.effect.IO
import laika.api._
import laika.format._
import laika.io.syntax._

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(Markdown.GitHubFlavor)
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

Versions prior to 0.12 had significantly different APIs as they did not build around the cats-effect APIs.
In the unlikely case you still have code for one of those ancient versions, please see the migration
guides for earlier versions.