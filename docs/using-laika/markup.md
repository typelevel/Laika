
Supported Markup
================

The current release supports Markdown and reStructuredText. Both implementations stay as close
as possible to the original descriptions and specifications of these formats. 

The markup
gets parsed into a generic document tree model shared between all parsers. This means
that there are some adjustments in terminology (e.g. a bullet list in reStructuredText
is called an "unordered list" in the Markdown syntax description). 

It also means that
there are some tree element types which are not produced by the Markdown parser, like
table, footnotes or citations, as reStructuredText is much more feature-rich than
the original Markdown syntax. 

The unified model allows to create renderers that
do not have to deal with any of the specifics of an individual markup syntax.



Markup Extensions
-----------------

Laika supports several convenient features for processing groups of documents.
These are built as extensions to both Markdown and reStructuredText parsers.
They can be switched off when you run these two parsers in strict mode:

    Transform from Markdown.strict to 
      HTML fromFile "hello.md" toFile "hello.html"

The extensions are documented in their respective section linked to from
the list below:

* A set of [Standard Template Directives]
* [Custom Directives][../extending-laika/directive.md:Directives] for adding new constructs to the
  parser without modifying it
* [Automatic Section Ids][Document Sections] (needed for cross linking)
* [Document Fragments] that can be rendered separately from the main document content
* [Cross Linking] between different documents
* [Table of Contents], listing both, directory contents and the sections within the documents
* [Autonumbering Documents and Sections]



Markdown
--------

Markdown is very easy to learn, very lightweight, and produces
documents which are fairly easy to read, even when you don't know anything about Markdown.

However, Markdown also has a few issues. First, there is no real specification,
only a page with a syntax description that leaves many questions unanswered. Secondly, its feature
set is fairly limited, lacking functionality for more advanced usage like technical 
documentation or entire books. As a consequence extensions have been added to almost 
every Markdown implementation without much agreement between  them.

Laika currently largely sticks to the original syntax description, except for the
features listed in the previous sections which are added to all parser implementations
supported in Laika. Other popular Markdown extensions like kramdown or MultiMarkdown
may be supported in future releases.  

Laika also fully passes the official Markdown Test Suite. 
These tests are integrated into the Laika test suite.


### Getting Started

Even if you do not know much about Markdown formally, it is very likely that you have already used it.
This document is not going to provide a syntax overview and refers to the official documentation instead.
It will primarily focus on anything specific to the way Laika handles Markdown.

To get an overview over Markdown syntax, these documents may be used:

* For a description of the syntax see the [official syntax documentation][markdown docs].

* For trying out small snippets of Markdown and checking how the various existing Markdown implementations
  handle them, the [Babelmark] tool can be used.  

* For using Markdown in Laika, the [Using Laika Embedded] page should answer most of your questions.

* For the special treatment of verbatim HTML in Laika, see the following section.

Laika tries to follow the official syntax documentation. In cases where it is silent on how
to handle certain edge cases, the [Babelmark] tool has been consulted and usually the approach
the majority of available parsers have chosen has been picked for Laika, too. There is currently only
one (known) minor exception: 

* Laika does not detect a header if it is not preceded by a blank line. Supporting this would be
  disadvantageous for three reasons: it goes against Markdown's design goal of promoting
  readability, it would slow down the parser and it would open the doors for accidental headers.
  According to Babelmark, there are at least two parsers (Pandoc, Python-Markdown) that agree.
   

[markdown docs]: http://daringfireball.net/projects/markdown/syntax

[Babelmark]: http://johnmacfarlane.net/babelmark2/


### Verbatim HTML

Finally there is one major difference to standard Markdown: the parsing of verbatim HTML elements
is not enabled by default, but it can be switched on if required. 

When using this feature
you need to be aware of the fact that it ties your markup files to HTML output. Future
versions of Laika are supposed to support formats like PDF, epub and DocBook, and markup
files containing raw HTML could not be used for those.

When the markup originates from user input in a web application, it would not be safe 
to use without additional filtering. By default Laika does not filter any tags, not
even `<script>` tags, but whitelist filtering can be added fairly easily through Laika's 
customization hooks like [Document Tree Rewriting] or [Customizing Renderers].
 
To enable verbatim HTML elements you have to change this standard expression:

    Transform from Markdown to HTML
    
to

    Transform from Markdown to HTML using VerbatimHTML
    
This installs both, the required parser and renderer extensions. 


    
reStructuredText
----------------

The reStructuredText project is part of Python's Docutils project. It is considerably more feature-rich
than Markdown, with support for tables, citations and footnotes. It is also more strictly defined than 
Markdown, with a detailed specification and clearly defined markup recognition rules.

On the other hand, adoption is not nearly as high as for Markdown, and some of the syntax is more
verbose and less intuitive or legible than Markdown.

Apparently there is no official test suite for reStructuredText, therefore to add a realistic
test to the Laika test suite a full transformation of the reStructuredText specification itself
is integrated into Laika's test suite. 

  
  
### Getting Started

This document is not going to provide a syntax overview and refers to the official documentation instead.

To get started with reStructuredText, these resources may be used:

* The [official markup specification][rst spec].

* An [online tool][rst tool] for trying out small snippets of reStructuredText.  

* [Using Laika Embedded] for using reStructuredText in Laika.

* The following sections below for an overview on how to implement extensions for reStructuredText


[rst home]: http://docutils.sourceforge.net/rst.html
[rst spec]: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
[rst tool]: http://www.tele3.cz/jbar/rest/rest.html  



### Support for Standard Directives and TextRoles

The Python reference parser supports a number of directives and text roles out of the box, and Laika
offers implementations for most of them. They are all registered by the default `ReStructuredText`
parser instance so that you only have to explicitly register any further custom extensions.

For a detailed specification of these standard extensions, see:

* Specification of the [standard directives][std directives] - including both block level directives
  and span directives to be used in substitution references.
  
* Specification of the [standard text roles][std roles].

[std directives]: http://docutils.sourceforge.net/docs/ref/rst/directives.html
[std roles]:      http://docutils.sourceforge.net/docs/ref/rst/roles.html 


### Supported Directives

Laika supports the following directives:

 * Admonitions: `attention`, `caution`, `danger`, `error`, `hint`, `important`,
   `note`, `tip`, `warning` and the generic `admonition`
 * Document Structure: `contents`, `sectnum`, `header`, `footer`, `title`, `include`
 * Containers: `compound`, `container`, `topic`, `sidebar`, `rubric`, `table`, 
   `parsed-literal`, `code`
 * Quotations: `epigraph`, `highlights` and `pull-quote`
 * Images: `figure` and `image`
 * For Substitution Definitions: `replace`, `date`, `unicode`
 * Other: `meta`, `raw`
 
The following limitations apply to these directives:

 * `code` does currently not support syntax highlighting 
   (it allows to set the language though, so client-side highlighters can be integrated if required).
   
 * `sectnum` does currently not support the `prefix`, `suffix` and `start` options.
 
 * `include` does not support any of the options apart from the filename, due to the way document trees
   are traversed very differently in Laika.
  
 * `raw` does not support the `file` or `url` options. 
 
 * `unicode`: does not support the various trim options, as that would require modifying adjacent elements
   (and no other directive has this requirement, therefore API/implementation changes did not seem justified)
 
 * `date`: Uses the patterns of `java.text.SimpleDateFormat` instead of Python's `time.strftime` function.
 
 
### Supported Standard Text Roles
 
The following standard text roles are fully supported:
 
 * `emphasis`
 * `strong`
 * `literal`
 * `subscript` (and `sub` alias)
 * `superscript` (and `sup` alias)
 * `title-reference` (and `title` alias) - the default role
 * `raw` (+ format option)


### Unsupported Extensions 
 
The following extensions are not supported:
 
 * `math`, `csv-table`, `list-table`, `target-notes` and `class` directives 
 * `math`, `pep-reference` and `rfc-reference` text roles
 
There are various reasons for excluding these extensions, some of them being rather technical.
For example, the `target-notes` and `class` directives would require processing beyond the 
directive itself, therefore would require new API. Others, like the `pep-reference` text role,
seamed too exotic to warrant inclusion in Laika.


### Extension Options

Finally some of the defaults for these extensions can be changed through the API:

    ReStructuredText.withRawContent
    
enables both the `raw` directive and the `raw` text role. They are disabled by default as
they present a potential security risk.

    object RstExtensions extends RstExtensionRegistry {
      val blockDirectives = Nil
      val spanDirectives = Nil
      val textRoles = Nil
      override val defaultTextRole = "my-role-name"
    }
    
    val transformer = Transform from ReStructuredText to HTML using RstExtensions
    
sets the text role `my-role-name` as the default role for the transformer.


### Implementing a Custom Directive

Laika comes with a concise and typesafe DSL to declare custom directives and text roles.
It is fully documented in [Extending reStructuredText].




  