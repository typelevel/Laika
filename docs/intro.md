
Laika
=====

Laika is a library for transforming lightweight markup languages to various types of output formats, written in Scala. 

It has been designed for two primary usage scenarios:

* Embedded use (e.g. web applications that allow user input in one of the supported markup syntaxes)

* As a standalone tool (e.g. as part of an sbt build that produces the project's documentation)


It is Open Source under the Apache 2.0 License.

If you wonder about the name of the project and have never heard about Laika, 
you can read about her [here][laika-wikipedia].


[laika-wikipedia]: http://en.wikipedia.org/wiki/Laika


Getting Started
---------------

The current version is published to Maven Central for Scala 2.10.x, 2.9.3 and 2.9.2.


Adding Laika to your project with sbt:

    libraryDependencies += "org.planet42" %% "laika" % "0.2.0"


Example for transforming from file to file:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"


For further information:

* Read more about standard usage in the chapter [Transformation Basics].

* Browse, clone or fork the source on [GitHub].

* Browse the API in the [Scaladoc].

* Follow on [Twitter] for release announcements.
 

This manual is written in Markdown and transformed by Laika. Its source
is included in the repository inside the `docs` folder.


[GitHub]: https://github.com/planet42/Laika
[Scaladoc]: api/
[Twitter]: https://twitter.com/_planet42
[Transformation Basics]: basics.html


Features
--------

* Support for Markdown and reStructuredText as input

* Concise and type-safe API for all reStructuredText extensibility options (directives, text roles)

* Support for HTML and PrettyPrint (for debugging etc.) as output

* Customization Hooks for Renderers

* Document Tree Rewriting

* Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)


Road Map
--------

* Books/Sites, multi-document processing with TOC generation (0.4)

* sbt Plugin and/or command line interface (0.5)

* New renderers for PDF (0.6) and epub (0.7)

* Luna, a new markup syntax that aims to combine the simplicity and readability of Markdown
  with the power and extensibility of reStructuredText (0.8)


Design Principles
-----------------

* Fully decouple the aspects of input/output, parsing, rendering and document tree rewriting, 
  making each of these steps pluggable.
  
* Provide a very convenient and simple high level API for common transformation tasks.

* Build a generic document tree model that does not reflect specifics of a particular 
  supported markup language like Markdown or reStructuredText.

* Allow for easy modification of the rendering for a particular node type only, without
  the need to sub-class or modify an existing renderer.
  
* Create the built-in parsers with the Scala parser combinators, providing efficient and
  reusable base parsers that encapsulate requirements common to all lightweight markup languages,
  while keeping the basic contract for plugging in a new parser function as simple and generic as 
  `Input => Document`, so that other parser frameworks or tools can be used, too.
  
  
Release History
---------------

* __0.2__ (May 7, 2013):

    * Support for reStructuredText (full specification)
    * Concise and type-safe API for all reStructuredText extensibility options (directives, text roles)
    * New document tree nodes for tables, footnotes, citations, definition lists, internal links,
      comments, system messages, invalid elements
    * Render hints for document tree nodes in the form of the new Customizable trait

* __0.1__ (Jan 12, 2013): 

    * Support for Markdown as input
    * Support for HTML and PrettyPrint as output
    * Customization hooks for renderers
    * Document tree rewriting
    * Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)
    * Generic base traits for markup parser implementations


  
