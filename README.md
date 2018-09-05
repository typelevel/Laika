![Laika](http://planet42.github.io/Laika/img/laika-top.png)

Customizable and extensible toolkit for transforming lightweight text markup and template based site generation.

Supporting Markdown and reStructuredText as input and HTML, PDF and XSL-FO as output, 
either through its integrated sbt plugin or embedded in Scala applications, 
without the need to install external tools.

Open Source under the Apache 2.0 License.


Getting Started
---------------

The main artifact is published to Maven Central for Scala 2.12 and 2.11.

The sbt plugin is published to the sbt plugin repository for sbt 1.x.

If you still need to work with sbt 0.13 and/or Scala 2.10 you need to stay on Laika 0.7.0
which had been the final release supporting those versions.


### Using the sbt Plugin

Add the plugin to `project/plugins.sbt`:

    addSbtPlugin("org.planet42" % "laika-sbt" % "0.8.0")

Enable the plugin in your project's `build.sbt`:

    enablePlugins(LaikaPlugin)

Add Markdown, reStructuredText or HTML template files to `src/docs` in your
project and run the `laikaSite` task from within sbt to generate the site
in `target/docs/site`.    


### Using Laika Embedded

Adding the Laika dependency to your sbt build:

    libraryDependencies += "org.planet42" %% "laika-core" % "0.8.0"

Example for transforming from file to file:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"

Example for transforming an entire directory of markup files:

    Transform from ReStructuredText to HTML fromDirectory "source" toDirectory "target"

Example for transforming an entire directory of markup files to a single PDF file:

    Transform from Markdown to PDF fromDirectory "source" toFile "hello.pdf"

When using Laika's PDF support you need to add one more dependency to your build:

    libraryDependencies += "org.planet42" %% "laika-pdf" % "0.8.0"
        

### Other Resources

For further information:

* Read the [Manual].

* Try out Laika with the [Web Tool].

* Browse the [API].

* Follow on [Twitter] for release announcements.

* Create [Issues] here on GitHub for bug reports or enhancement requests.

* Ask questions on [Stackoverflow], tagging with Laika and Scala.
 

[Manual]: http://planet42.github.com/Laika/index.html
[Web Tool]: http://planet42.org/
[API]: http://planet42.github.com/Laika/api/
[Twitter]: https://twitter.com/_planet42
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika
