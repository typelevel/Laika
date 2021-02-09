
Features
========

Laika is a customizable and extensible toolkit for transforming lightweight text markup 
and template based site and e-book generation.

It can be used in a range of usage scenarios:

* As an sbt plugin as part of your build and CI pipeline, e.g. for generating Scala project documentation

* As part of your build with other build tools such as Mill through its library API.

* In server-side Scala applications, e.g. for transforming user input in markup syntax or as part of document 
  processing pipelines
  
* In browser-based applications via its library API which is published for Scala.js

* Scaling up to producing complex e-books and sites from dozens of inputs, to scaling down to transform
  a snippet of Markdown with three lines of code.
  
  
Supported Platforms
-------------------

Laika can be used as:

* A plugin for sbt version 1.x.

* A library on the JVM with Scala 2.12 or 2.13.

* A library in Scala.js 1.0 applications with all its features except File/Stream IO, EPUB and PDF output

For getting started see the documentation of the [sbt Plugin] or [Library API].


Supported Formats
-----------------

* Markdown including GitHub Flavor

* reStructuredText including its standard directives

* HOCON for configuration and directive attributes, based on Laika's own parser

* HTML site generation based on templates

* [E-Books (EPUB & PDF)] with auto-generated navigation

* Lightweight default theme with configurable styling for site, EPUB and PDF output
  with extensive [Theme Settings].


Content Organization
--------------------

* [Validated Internal Links] in "native" text markup syntax, based on relative paths or [Linking by Section Headline]. 

* Avoid repetition and centralize all external URLs used in links with [Global Link Definitions].

* Use Laika's [Standard Directives] for automatically [Generating Navigation Trees] or [Breadcrumbs],
  optionally with [Auto-Numbering].

* Use integrated parsers for [Syntax Highlighting] that work for all output formats. 
  Supported out of the box are Scala, Dotty, Java, Python, JavaScript (JSX), TypeScript (TSX), Haskell,
  HTML, CSS, XML, YAML, JSON, HOCON, SQL, EBNF, Alloy, Dhall
  
* Freely organize and merge content from multiple input directories or generated in-memory 
  with the library's [Virtual Tree Abstraction].
  
* Produce [Versioned Documentation] based on simple configuration steps and an integrated version switcher
  dropdown in the default Helium theme.


Library API
-----------
  
* Use a purely functional API, with full referential transparency, no exceptions and no runtime reflection.

* Abstracts all effectful computations behind a polymorphic effect type based on `cats-effect` typeclasses, 
  so that any compatible library can be used with Laika (`cats.IO`, `Monix`, `Zio`).
 
* Full control over ExecutionContexts for blocking IO and CPU-bound processing.


Customizations
--------------

* Create your own look & feel by [Creating Themes] or adjust the built-in theme by [Creating Templates].

* Process and transform [The Document AST] between parsing and rendering by [AST Rewriting].
  
* Adjust the rendered output for individual AST node types by [Overriding Renderers].


Extensibility
-------------

* Extend the syntax of text markup languages, either by [Implementing Directives] or by [Writing Parser Extensions].

* Support additional languages for Laika's built-in highlighter by [Adding Syntax Highlighters].

* Use simple contracts and APIs for adding [New Markup or Output Formats].

* Write all parsers based on [Laika's Parser Combinators] which are optimized for multi-pass markup parsing.
