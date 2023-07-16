
![Laika](docs/src/img/site/laika-dog@1.5x.png)

Site and E-book Generator and Customizable Text Markup Transformer for sbt, Scala and Scala.js

![Build Status](https://github.com/planet42/Laika/workflows/CI/badge.svg)
![Maven Central Release](https://img.shields.io/maven-central/v/org.planet42/laika-core_2.12.svg)


Latest Release
--------------

* The latest stable release is version **0.19.3**.
* The latest milestone release is version **1.0.0-M2**.

The library is published to Maven Central for Scala 3, 2.13, 2.12 and supports cats-effect 3.x and Scala.js 1.x.  
The sbt plugin is published for sbt 1.x.

Open Source under the Apache 2.0 License.


Adding Laika to your Build
--------------------------

Stable releases for 0.19 are still published under the `org.planet42` organization, 
while the 1.0 milestones are now published under `org.typelevel`:

```scala
// for version 0.19.3:
addSbtPlugin("org.planet42" % "laika-sbt" % "0.19.3")

// for version 1.0.0-M2:
addSbtPlugin("org.typelevel" % "laika-sbt" % "1.0.0-M2")
```

In case you still need support for older versions of Scala, sbt or cats-effect, please use:
* 0.17.1 for cats-effect 2.x
* 0.10.0 for Scala 2.11
* 0.7.0 for Scala 2.10 and sbt 0.13.x



Features
--------

* **No External Tools**

  Easy setup without any external tools or languages and only minimal library dependencies.
  
* **Flexible Runtime**

  Laika can be used as an sbt plugin, as a Scala library for the JVM or in the browser via Scala.js.
  
* **Purely Functional**

  Fully referentially transparent, no exceptions or runtime reflection and integration 
  with cats-effect for polymorphic effect handling.
  
* **Rich Feature Set**

  Markdown and reStructuredText as input, HTML, EPUB and PDF as output, integrated preview server and syntax highlighting, 
  link validation, auto-generated navigation, versioned documentation, and much more.
  
* **Lightweight Theme**

  The default Helium theme includes only a minimal amount of handcrafted CSS and JS, no Bootstrap, no frameworks.

* **Highly Extensible**

  Process the document AST, adjust rendering for individual AST nodes 
  or extend text markup languages with custom directives.
  

Further Information
-------------------

* Read the [Manual].

* Try out Laika with the [Demo App].

* Browse the [API].

* Create [Issues] for bug reports or enhancement requests.

* Ask questions or share ideas in [Discussions].
 

[Manual]: https://typelevel.org/Laika/
[Demo App]: http://planet42.org/
[API]: https://typelevel.org/Laika/latest/api/laika/api/
[Issues]: https://github.com/typelevel/Laika/issues
[Discussions]: https://github.com/typelevel/Laika/discussions
