Laika
=====

Library for transforming lightweight text markup into various types of output formats, written in Scala.

Open Source under the Apache 2.0 License.


Getting Started
---------------

The current version is published to Maven Central for Scala 2.10.x, 2.9.3 and 2.9.2.


Adding Laika to your project with sbt:

    libraryDependencies += "org.planet42" %% "laika" % "0.2.0"


Example for transforming from file to file:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"


For further information:

* Read the extensive documentation in the [Manual].

* Browse the API in the [Scaladoc].

* Follow on [Twitter] for release announcements.
 

[Manual]: http://planet42.github.com/Laika/index.html
[Scaladoc]: http://planet42.github.com/Laika/api/
[Twitter]: https://twitter.com/_planet42

