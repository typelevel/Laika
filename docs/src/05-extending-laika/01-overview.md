
Extending Laika - Overview
==========================

Laika's Documentation comes with separate sections for "Customizing" and "Extending" Laika.
While the line between the two is naturally quite blurry, 
the distinction is mostly meant to be between these two scenarios:

* Customizing Laika refers to the things you most like want to tweak and adjust in your own project.
  
* Extending Laika refers to the kind of re-usable extensions you might want to write as an in-house library 
  or a 3rd-party open source extension.
  
This section deals with the latter scenario.


Transformation Phases
---------------------

For a better understanding of the extension points listed below it is good to a have a rough idea about how
a transformation in Laika is performed. It can be divided into 4 phases:

1) **The parsing step**. Text markup and template documents get parsed and translated into an internal AST.
   The AST is a generic abstraction of the document's structure and is not tied to any specific semantics
   of a particular input or output format.
   
2) **The AST transformation**. The original AST is only what each parser for the corresponding block or inline
   element can process locally, without access to other nodes or even other documents. 
   One of the advantages of this design, apart from separation of concerns, is that parsers can run in parallel.
   As a consequence nodes like internal references or auto-numbered footnotes require further processing with access to
   a `DocumentCursor` that allows to access content from anywhere in the input tree.

3) **Applying templates to markup documents**. 
   Since both are just AST structures, this step is merely a second AST transformation.
   The AST representing the markup document will be inserted into the node of the template AST that holds
   a reference to the content.
   
4) **Rendering**. As the last step the final AST obtained from the two previous transformation steps will get rendered
   to one or more output format. 
   This is the only step specific to a particular output format, meaning the same AST structure obtained in 3) will
   get used as the input for the renderers of all formats.


The ExtensionBundle API
-----------------------

Hooks into the 4 transformation phases described above as well as other customization hooks
can be used by implementing an `ExtensionBundle`.
See @:api(laika.bundle.ExtensionBundle) for its API documentation.

The trait comes with empty default implementations for most of its properties,
so that you only need to override the ones you intend to use.

```scala
object MyExtensions extends ExtensionBundle {

  override val docTypeMatcher: PartialFunction[Path, DocumentType] = ???

  // ... optionally other customizations
}
```

Such a bundle can then be passed to the transformer:

@:select(config)

@:choice(sbt)
```scala
laikaExtensions := Seq(
  GitHubFlavor,
  MyExtensions
)
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .using(MyExtensions)
  .build
```
@:@

You've probably already seen examples for specifying `GitHubFlavor` or `SyntaxHighlighting` extensions in this way.
These are implementations of `ExtensionBundle`, too, and come bundled with the `laika-core` module.


### The Theme API

Creating themes expands on the extension hooks provided by bundles and adds the capability of pre-populating
the input tree with templates and styles.
It allows to offer users ready-to-use rendering styles without the need to craft their own templates, 
CSS or JavaScript.

Laika also comes with a lightweight default theme called Helium, and in many cases just tweaking its settings
might give you enough flexibility for achieving the desired look & feel.
The theme comes with default styles for the three main output formats of Laika: web site, EPUB and PDF.
Configuring Helium is covered in detail in the chapter [Theme Settings].

If you need full control over all aspect of the design, see the chapter [Creating Themes] for instructions.
The remainder of this chapter deals with the functionality of the `ExtensionBundle` API introduced above. 


### Extending Markup Syntax

Laika offers two major options for extending the native syntax of a text markup language:

* [Implementing Directives] is an option that allows to quickly add custom functionality without 
  implementing a custom parser.
  Laika comes bundled with a handful of directives which are documented in [Standard Directives].
  
* [Writing Parser Extensions] is the alternative for cases where you have special requirements for the syntax
  so that the convenient path of writing a directive is not feasible.
  In this case you also need to become familiar with [Laika's Parser Combinators].

Technically directives hook into phase 2 of a transformation while parser extensions live in phase 1. 
Parsing of directives is performed by Laika's built-in parsers as they all have a common syntax.
A directive implementation is then invoked during AST transformation, 
receiving all the attributes and body elements the user specified to produce a new AST node to insert.


### Additional Syntax Highlighters

Laika has its own built-in syntax highlighters (based on its parser combinators).
See [Syntax Highlighting] for a list of languages supported out of the box.

If you want to use a language that is not supported yet, you can either decide to use an external tool like
`highlight.js` for this task (Laika renders code elements with attributes compatible with that tool)
or write your own implementation for Laika's highlighters.

If you look at the existing implementations, you'll notice that some of them are almost purely declarative,
as the library provides a set of building blocks for parsing the most common literal formats or comments.

But in most cases you'd also need to get familiar with [Laika's Parser Combinators].
See [Adding Syntax Highlighters] for more details.


### Additional Markup or Output Formats

Apart from extending or customizing existing markup or output formats, you can also add support for additional formats.
If you want to support a different text markup language like ASCIIDoc or Textile, 
or if you want to produce Word output or slide formats, 
you can implement a `MarkupFormat` or `RenderFormat` as shown in [New Markup or Output Formats].


### Customization Hooks

Two of the most commonly used hooks in the `ExtensionBundle` API are described in the 
"Customizing Laika" section of the manual:

* [AST Rewriting], a hook into phase 2 that allows to replace or remove nodes from the AST model 
  between parsing and rendering.
 
* [Overriding Renderers], a hook into phase 4 that allows to override the rendered output for specific
  AST nodes.

There are three further hooks that drive more low-level functionality:

* The `docTypeMatcher` property in `ExtensionBundle` controls how a virtual path is used to determine the document type
  (e.g. markup document vs. template vs. configuration file).

* The `pathTranslator` property in `ExtensionBundle` controls how a virtual path is translated 
  to the corresponding output path.
  The internal path translator deals with aspects like applying the suffix for the output format
  or modifying the path for versioned documents.
  An extension can perform additional translations either before or after delegating to the built-in translator. 

* The `slugBuilder` property in `ExtensionBundle` controls how the text from a section headline is translated
  to a slug for element ids. 
  It is a simple function `String => String`.


### Replacing Internal Parsers

Laika does not only come with parsers for supported text markup languages.
It uses additional parsers for its HOCON configuration support, for parsing templates
and for its "CSS for PDF" functionality.

Although probably the least likely extension point you need to use, 
these three parsers can all be replaced by implementing the respective properties in the 
ParserBundle API: `configProvider`, `templateParser` and `styleSheetParser`.
Such a bundle can then be declared in an `ExtensionBundle` with the `parsers` property.
