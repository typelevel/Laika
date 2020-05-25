
Laika
=====

Laika is a customizable and extensible toolkit for transforming lightweight text markup 
and template based site and e-book generation.

If you wonder about the name of the project and have never heard about Laika, 
you can read about her [here][laika-wikipedia].

[laika-wikipedia]: http://en.wikipedia.org/wiki/Laika


Latest Release
---------------

The latest release is version **0.14.0**.  

It is published to Maven Central for Scala 2.13, 2.12 and Scala.js 1.0.

The sbt plugin is published to the sbt plugin repository for sbt 1.x.

The last release supporting Scala 2.11 had been 0.10.0, 
the last release for Scala 2.10 and sbt 0.13 was 0.7.0.

Open Source under the Apache 2.0 License.


Features
--------

* Supported Platforms 
  
    * Integrated plugin for sbt 1.x, to run Laika as part of your build or CI pipeline.
    
    * Purely functional library API for Scala 2.13 and 2.12 and Scala.js 1.0, 
      with full referential transparency, no exceptions and no runtime reflection.
    
    * The library API abstracts all effectful computations behind a polymorphic effect type based on 
      `cats-effect` typeclasses, so that any compatible library can be used with Laika (`cats.IO`, `Monix`, `Zio`).
     
    * Full control over ExecutionContexts for blocking IO and CPU-bound processing.
    
    * All functionality is available without the need to install any external tools.
    
    * Scaling up to producing complex e-books and sites from dozens of inputs, 
      to scaling down to transform a snippet of Markdown with three lines of code.

* Supported Formats
  
    * Markdown including GitHub Flavor and reStructuredText as input formats
    
    * HTML, EPUB and PDF as output formats
    
* Content Organization

    * Convenient navigation with validated internal links, global link definitions, cross-referencing by headline text,
      auto-generated navigation trees or breadcrumbs with optional auto-numbering.
      
    * Use integrated parsers for syntax highlighting that work for all output formats. 
      Supported out of the box are Scala, Dotty, Java, Python, JavaScript (JSX), TypeScript (TSX), 
      HTML, CSS, XML, JSON, HOCON, SQL, EBNF
      
    * Freely organize and merge content from multiple input directories or generated in-memory 
      with the library's virtual path abstraction.
    
* Customizations & Extensibility

    * Process and transform the Document AST between parsing and rendering.
      
    * Adjust the rendered output for individual AST node types or control the final output of documents with custom templates. 
    
    * Extend the syntax of text markup languages, either with custom directives or by writing a parser extension.


Getting Started
---------------

* Check the detailed list of [Features].

* Read the introduction for the [sbt Plugin] or the [Library API].

* Try out Laika with the [Demo App].

* Browse the source on [GitHub].

* Browse the [API Documentation].

* Follow on [Twitter] for release announcements.

* Create [Issues] here on GitHub for bug reports or enhancement requests.

* Ask questions on [Stackoverflow], tagging with Laika and Scala.
 
This manual is written in Markdown and transformed by Laika. 
Its source is included in the repository inside the `docs` folder.


[Demo App]: http://planet42.org/
[GitHub]: https://github.com/planet42/Laika
[API Documentation]: ../api/laika/api/
[Twitter]: https://twitter.com/_planet42
[Issues]: https://github.com/planet42/Laika/issues
[Stackoverflow]: http://stackoverflow.com/questions/ask?tags=scala%2claika
