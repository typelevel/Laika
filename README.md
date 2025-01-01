
![Laika](docs/src/img/site/laika-dog@1.5x.png)

Site and E-book Generator and Customizable Text Markup Transformer for sbt, Scala and Scala.js

![Build Status](https://github.com/typelevel/Laika/actions/workflows/ci.yml/badge.svg)
![Maven Central Release](https://img.shields.io/maven-central/v/org.typelevel/laika-core_2.12.svg)


The library is published to Maven Central for Scala 3, 2.13, 2.12 and supports cats-effect 3.x and Scala.js 1.13+.  
The sbt plugin is published for sbt 1.x.

Open Source under the Apache 2.0 License.


Version 1.x
-----------

1.x artefacts are now published under the `org.typelevel` organization as opposed to 0.x releases which were published under `org.planet42`.


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

* Browse the [API].

* Create [Issues] for bug reports or enhancement requests.

* Ask questions or share ideas in [Discussions]  
  or in the `#laika` channel of the [Typelevel Discord].


[Manual]: https://typelevel.org/Laika/
[API]: https://javadoc.io/doc/org.typelevel/laika-docs_2.12/latest/laika/index.html
[Issues]: https://github.com/typelevel/Laika/issues
[Discussions]: https://github.com/typelevel/Laika/discussions
[Typelevel Discord]: https://discord.gg/XF3CXcMzqD