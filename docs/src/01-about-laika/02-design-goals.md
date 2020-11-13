
Design Goals
============

Laika is designed around a few guiding principles that are outlined below.

They may be of interest for finding out how Laika tries to differentiate itself from comparable tools used in the
Scala ecosystem. 
It may also be relevant for potential contributors as a very high-level introduction to the library's 
underlying design choices.


Not Tied to External Tools
--------------------------

Laika is released as a standalone library.
It does not rely on any external tool or language installed on your machine, apart from the JVM or the browser,
depending on whether you use it with Scala.js or not.

The Scala ecosystem still heavily relies on installed tools for project documentation.
While it is attractive to benefit from the wide adoption of projects like Jekyll or Hugo and all its available
themes and extensions, the fact that a contributor has to perform installations on her machine just to verify
a small documentation change presents an extra hurdle that would be nice to avoid.

Scala project documentation is not the only use case Laika supports though.
Its independence from tools also makes it easier to embed it in server-side or browser applications.


Not Tied to the JVM
-------------------

The `laika-core` module, about 80% - 90% of the library's functionality is published for both, the JVM and Scala.js.
This way Laika can be used in the browser, e.g. for processing user input right on the client.

The implementation is also free of any runtime reflection, a prerequisite for Scala.js support and has
minimal dependencies. 
`laika-core` only depends on `cats-core` while the `laika-io` module adds a dependency on `cats-effect`.

The only module definitely tied to the JVM is the `laika-pdf` module which depends on a fairly heavy Java library
(Apache FOP). 


Not Tied to sbt
---------------

While Laika does include an sbt plugin, it is a separate artifact and only a very thin layer
of sbt settings and tasks on top of the library API.
In fact, Laika started as a library and only added the sbt plugin in version 0.5 in 2014.

Due to this design, less than 2% of the code lines in the Laika project depend on the sbt APIs.
Laika can easily be used with other build tools such as Mill or as a library in application code.

This is in contrast to many sbt plugins which "bury" the provided functionality under the sbt APIs,
making it unavailable for any other use case.


Not Tied to Markdown
--------------------

Even though Markdown has become fairly ubiquitous, Laika avoided tying its internal design entirely to this one
markup format, in contrast to many similar tools and libraries.
It is decoupled from Markdown in several ways:

* In addition to its Markdown parser it comes with support for a second, 
  advanced text markup format right out of the box: reStructuredText.
  Which brings advanced table formats and footnotes to the table.
  It is probably the only complete and actively maintained implementation of reStructuredText on the JVM.
  
* The Markdown parser produces a generic AST model as a result, instead of directly producing an output format.
  This AST does not reflect any specific semantics of Markdown and other text markup formats produce the exact
  same kind of model. The AST can get analyzed and transformed before being fed to any of the supported
  renderers. (See [The Document AST] and [AST Rewriting] for details)
  
* The Markdown and reStructuredText parser sit behind a well-defined (and fairly simple) abstraction, 
  the `MarkupFormat` trait. 
  This allows 3rd-parties (or future Laika releases) to add support for additional markup formats while
  still benefiting from the subsequent processing steps the library provides, like validating links or
  producing tables of contents. (See [New Markup or Output Formats] for details)
  
* Finally there is another hook for extending the syntax of existing text markup parsers. 
  A syntax extension can be written in such a way that it works with any markup language as long as the
  syntax does not conflict with its "native" syntax. (See [Writing Parser Extensions] for details)
 

Not Tied to HTML
----------------

Similar to Laika's independence from a particular input format, it also avoids the coupling to HTML output
found in many other tools.

It supports HTML, EPUB and PDF out of the box, together with an `AST` renderer that provides a formatted
representation of the document AST for debugging purposes.

All renderers will be fed with an AST model which is usually obtained by parsing text markup,
but can alternatively be generated in memory (or a combination of the two). (See [The Document AST] for details)

Like the parsers the renderers are all based on a well-defined abstraction, the `RenderFormat` trait.
This allows 3rd-parties (or future Laika releases) to add support for additional output formats.
(See [New Markup or Output Formats] for details) 


Not Tied to the File System
--------------------------- 

While the most common forms of specifying the input and output are file-based with the default
settings of the sbt plugin specifying a single directory as input, this is by no means the only option to compose 
the sources.
 
Laika internally uses a [Virtual Tree Abstraction] to organize markup documents, templates and configuration files.
The decoupling from the file system gives additional flexibility. 

For example, when two directories are specified as input the resulting, logical tree will be a recursive merge
of the two trees. 
This way reusable templates, styles and static files can be kept separate from the markup files.  

A second example is the need to add generated content to the files from the input directory.
The content can simply be generated in-memory and get a virtual path assigned that will cause the generated
content to be "mounted" at the specified location.
There is no need to write the content back to the file system first, which is also very CI-friendly.

All internal link references between documents happen based on the virtual path, 
meaning a path in the form of `../intro.md` inside a file may refer to something generated in-memory.


Purely Functional
-----------------

All Laika APIs are designed to be purely functional. 
The only minor exception is the implementation of the sbt plugin (< 2% of Laika's code), 
which has to code against sbt's impure APIs.

The library API is fully referentially transparent and abstracts all effectful computations behind a polymorphic
effect type based on `cats-effect` typeclasses, so that any compatible library can be used with Laika (`cats.IO`,
`Monix`, `Zio`).

While the target audience of this project is larger than the purely functional part of the Scala community,
it is always easier to ignore the aspect of referential transparency of a library that supports it in
case it is not your design approach than the other way round: trying to wrap pure code around an impure library.


A Toolkit for Creating Toolkits
-------------------------------

This design goal is mostly achieved "for free" by following the previously mentioned goals of decoupling on many levels,
but still deserves an explicit entry.

While Laika does also aim to be a useful tool right out of the box, the fact that almost every parsing,
processing or rendering step can either be heavily customized or replaced in its entirety by alternative implementations,
makes it a good candidate for building other engines on top of it which offer a different focus of use cases. 
