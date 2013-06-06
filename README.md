![Laika](http://planet42.github.io/Laika/img/laika-top.png)

Customizable and extensible toolkit for transforming lightweight text markup into various types of output formats, written in Scala.

Open Source under the Apache 2.0 License.


Getting Started
---------------

The current version is published to Maven Central for Scala 2.10.x, 2.9.3 and 2.9.2.


Adding Laika to your project with sbt:

    libraryDependencies += "org.planet42" %% "laika" % "0.2.0"


Example for transforming from file to file:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"


For further information:

* Read the [Manual].

* Try out Laika with the [Web Tool].

* Browse the [API].

* Follow on [Twitter] for release announcements.

* Create [Issues] here on GitHub for bug reports or enhancement requests.

* Ask questions on [Stackoverflow], tagging with Laika and Scala.
 

[Manual]: http://planet42.github.com/Laika/index.html
[Web Tool]: http://www.planet42.org/laika/webtool
[API]: http://planet42.github.com/Laika/api/
[Twitter]: https://twitter.com/_planet42
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika
