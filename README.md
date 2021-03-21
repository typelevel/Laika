
![Laika](docs/src/img/laika-dog@1.5x.png)

Toolkit for transforming lightweight text markup and template based site and e-book generation.

![Build Status](https://github.com/planet42/Laika/workflows/CI/badge.svg)
![Maven Central Release](https://img.shields.io/maven-central/v/org.planet42/laika-core_2.12.svg)


Latest Release
--------------

The latest release is version **0.17.1**.  

The library is published to Maven Central for Scala 2.13, 2.12 and Scala.js 1.x and the sbt plugin for sbt 1.x.

Support for Scala 3 and cats-effect 3 is available on master, but has not been released yet.

The last release supporting Scala 2.11 had been 0.10.0, 
the last release for Scala 2.10 and sbt 0.13 was 0.7.0.

Open Source under the Apache 2.0 License.


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

  Markdown and reStructuredText as input, HTML, EPUB and PDF as output, integrated syntax highlighting, 
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

* Follow on [Twitter] for release announcements.

* Create [Issues] here on GitHub for bug reports or enhancement requests.

* Ask questions on [Stackoverflow], tagging with Laika and Scala.
 

[Manual]: https://planet42.github.com/Laika/index.html
[Demo App]: http://planet42.org/
[API]: https://planet42.github.com/Laika/api/laika/api/
[Twitter]: https://twitter.com/_planet42
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika
