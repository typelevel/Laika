
Release Notes
=============

0.18.1 (Dec 12, 2021)
---------------------

* Scala.js support: this is the first release published for Scala 3 on Scala.js
* Link validation now allows duplicate headers on the same page as long as no internal link points to them
* All tests have been migrated from ScalaTest to munit
* Improvements and fixes for the Helium theme
    * The content and landing pages now render properly in portrait mode on phones
    * The version dropdown now works correctly on unversioned pages
    * Anchor placement for section headers no longer overlaps with header text
    * The font for code now falls back to `monospace` for unavailable glyphs
* Other Bugfixes:
    * Parser inputs with Windows line breaks led to errors
    * `mailto` links were incorrectly validated as internal links
    * The `laikaPreview` task of the sbt plugin did not report the port correctly when launching
    * The `laikaDescribe` task of the sbt plugin failed in case of non-existing input directories 
      (which is common when using Laika with mdoc)


0.18.0 (Aug 2, 2021)
--------------------

* Migrate to cats-effect 3
    * Adjust public API in `laika-io` module by removing all usages of `ContextShift` and `Blocker`
      which are gone in CE3.
    * Remove the old `.io(blocker)` builder step as it is now redundant.
    * Simplify requirements to just `Sync` for sequential transformation and `Async` for parallel execution.
    * PDF support: remove the old callback hacks for integration with the blocking, synchronous `ResourceResolver` API
      of Apache FOP by using the new `Dispatcher` from CE3 instead.
* Support for Scala 3.0
* New Preview Server
    * The new `laikaPreview` task of the sbt plugin can be used to browse generated sites.
    * Includes auto-refreshing when input sources change.
    * Can also be launched via the library API, using `laika.preview.ServerBuilder`.
    * Introduces a new published module `laika-preview` that the sbt plugin depends on.
* New Directives:
    * `@:path`: Validates and translates a path in a template to a path relative to the rendered document the template
      is applied to.
    * `@:attribute`: Renders an optional HTML or XML attribute.
* Helium Theme:
    * Add support for dark mode to the Configuration API for EPUB and HTML output, 
      allowing to specify a complete second color set for theme colors and syntax highlighting that becomes
      active when the user switches to dark mode in the OS or reader software.
    * Make the link of the home icon configurable.
    * Add the page background color to the configurable theme colors.
    * Add a "high-contrast" option for the top navigation bar (darker in light mode and lighter in dark mode).
* Icon Support:
    * Add new AST nodes for different kinds of icon sets: font icons, CSS icons (e.g. image sprites), inline SVG icons
      and SVG symbols (references).
    * New `@:icon` directive that allows to reference an icon by key in markup documents or templates.
* Versioning: 
     * New `renderUnversioned` flag, that can be set to false when rendering older versions 
       (e.g. from a maintenance branch) to ensure that unversioned files always come from the main branch (newest version).
     * Use existing `/laika/versionInfo.json` documents in the input directory as an alternative to scanning the
       target directory for indexing the content of older versions.
* Link Validation: new `addProvidedPath` method on `InputTreeBuilder` that adds a path representing a document which is 
  processed by some external tool, making it available during link validation.
* PDF Support: upgrade to Apache FOP 2.6 (2.4 and 2.5 were both skipped as they had an issue with non-public dependencies 
  in their POMs).
* EPUB Renderer: add support for JavaScript execution in EPUB documents
    * Scripting in EPUB requires a flag to be set for each scripted document in OPF metadata.
    * Indicators can be given in the EPUB template with the new `laika.epub.scripted` key,
      which has the values `always`, `never` and `auto`.
      The `auto` value which is also used in Helium's default template sets the flag whenever there are any
      documents in the input tree with the suffix `.epub.js` or `.shared.js`.
* Error Handling for Rewrite Rules and Cursor API
    * Change signatures of methods for registering rules in a bundle, rewriting a document or creating a cursor 
      to include error handling (rule builders can now return an `Either[ConfigError, RewriteRule]`).
    * Use these new hooks in internal rewrite rules to validate configuration values used by the rules early.
    * Include key information in `DecodingError`.
* Error Reporting: When a parser error originates in a template the error formatter now includes the path info
  for the template instead of making the error appear as if it came from the markup document
* AST: add `codeOptions` property to `CodeBlock` element for potential tool integrations
* sbt plugin: The `Laika/clean` task now preserves some directories that will not be re-generated
  (e.g. api documentation, download directory and older versions in versioned documentation).
* Bugfixes
    * The HTML renderer did not apply versions to image URLs in case of versioned documentation
    * 0.17.1 introduced a regression where transforming empty strings caused an endless loop
    * In-memory string input sources could not be read more than once
    * The content of the `@:fragment` directive was ignored in many rendering scenarios


0.17.1 (Mar 19, 2021)
---------------------

* New syntax highlighters for YAML, Haskell, Dhall and Alloy.
* AST: new companions for list and link types to reduce boilerplate and new `RawLink` type.
* Reduction of boilerplate in test suites by re-using common directory tree structures.
* Publish sbt plugin to Maven Central instead of bintray to deal with the impending sunsetting of the latter.
* Include manual build and GitHub Actions in repository.
* Bugfixes
    * Versioning support in Helium theme: fix several issues where versioned path were rendered unversioned
      and vice versa; affected links to API documentation, e-book downloads and the favicon.
    * Fix incorrect merging of configuration instances when using `InputTreeBuilder` to manually assemble inputs.
    * `Path.isSubPath` no longer fails when the only difference is suffix or fragment.
    * Markdown parser now supports literal spans with multiple backticks as delimiter.
    * The date directive for reStructured text now excludes the time component by default to align with the spec.
    * Whitespace before a configuration header in a markup file does no longer prevent detection of the header.


0.17.0 (Oct 31, 2020)
---------------------

* New Support for Versioned Documentation
    * Allows to configure individual inputs (directories or documents) as versioned or unversioned.
    * Writes versioned documents into a sub-path (e.g. `/0.17/...`).
    * Includes a version dropdown in the Helium theme to switch between versions.
    * Dropdown is populated from JSON, therefore *older versions see newer versions*.
    * Support for "smart linking" where the version switcher picks the same page in the target version when it exists.
    
* Position Tracking for Multi-Pass Parsers
    * Previous releases only supported position tracking for single-pass parsers (e.g. CSS, HOCON, templates),
      but not for multi-pass (text markup), where the 2nd and subsequent passes lost track of the position.
    * Support for multi-pass tracking was introduced by replacing the old `ParserContext` type 
      by an ADT (`SourceCursor`), with concrete sub-types for recursive parsing (e.g. `BlockSource` or `LineSource`),
      that remember their relative position into the `RootSource`.
    * Formatting of parser errors for Markdown and reStructuredText was improved to carry line number information.
    
* Configuration Enhancements
    * New configuration options `laika.targetFormats`, available for directory and document configuration,
      which allows to restrict the output formats a document is rendered in.
    * Allows to configure a document to only be part of the site output, but not the PDF and EPUB for example,
      or, when specifying an empty array, to not be rendered at all, in case you intend to use 
      a document solely via the new `@:include` directive.
    * Link Validation is aware of this configuration and prevents you from linking to a document that is available
      in fewer output formats than the link source.
    
* New Directives
    * `@:include` and `@:embed` allow to include other text markup documents or templates in the current document.
      The latter also allows to pass a parsed body element which can be referenced inside the included resources,
      which may be useful for creating "master templates" that act as a frame for child templates.
    
* New Link Validation API
    * `DocumentCursor` has several new `validate` methods to validate internal targets by relative or absolute paths, 
      allowing this previously internal logic to be used by custom rewrite rules or directives.
    
* Improved Fenced Code Blocks for Markdown
    * Lifts the requirement of a preceding blank line to detect a code fence.
    
* Bugfixes
    * One of the default fonts (Lato) in the Helium theme had a `http` URL and thus did not work over `https`.
    * `selectDocument(Path)` on `DocumentTree` did not work for title documents, which also caused issues for
      navigation directives when used on input trees with title documents.
    

0.16.1 (Sep 12, 2020)
---------------------

* New Lightweight Theme called Helium
    * Ready-to-use styles for the 3 major output formats: HTML, EPUB, PDF
    * No dependency on 3rd-party CSS or JavaScript frameworks, just a minimal amount of hand-crafted CSS and JS.
    * Responsive design of the site output.
    * Define font resources, color sets, layout and other details with the Scala configuration API for Helium
    * Support font embedding for EPUB and PDF out of the box
    * Supports auto-linking of all CSS and JavaScript files from your input tree in the HTML output for websites
      and EPUB, search paths can be restricted if necessary.
    * Obtain more low-level control over the output by overriding the theme's CSS.
    * Includes an auto-generated main navigation bar and a page navigation with configurable depth.
    * Anchors on mouse-over for headlines, providing the URL to the section.
    * Favicon support.
    * Support for font icons as well as a set of default icons included in the theme.
    * Integrated download page offering the site content as EPUB and PDF.
    * Website landing page tailored for software documentation sites.

* New API for Theme Authors
    * The Theme API allows to create 3rd-party themes by pre-populating the input tree with styles, scripts, images, 
      font resources as well as providing extensions for the Laika Core features in the form 
      of the existing `ExtensionBundle` API.

* New API for freely composing inputs
    * Individual files, in-memory strings or streams can now be freely combined and "mounted" to a logical path
      within Laika's input tree.
    * Add support for classpath resources.
    * Add support for providing inputs as a pre-built document AST, bypassing the parsing step altogether.
    * Supported for user code (when specifying the inputs to transform) as well as for theme authors.
    
* New Directives
    * `@:callout` produces decorated text blocks with background color and icon.
    * `@:image` enhances the options native markup provides by allowing to specify the intrinsic width and height
      of images to avoid layout shift in browsers as well as assigning a style for controlling the display size via CSS.
    * `@:select` is a powerful directive to create alternative versions of your content, e.g. for Scala vs. Java APIs
      or for sbt vs Mill build examples.
      In the rendered website these choices are available via tabs, for EPUB and PDF they will lead to separate
      artifacts containing only one of the choices.
    * `@:source` is a link directive, allowing to specify a fully qualified classname, similar to the existing 
      `@:api` directive, but linking to the source (e.g. on GitHub) instead of the API documentation.
    * `@:linkCSS` and `@:linkJS` can be used in HTML template files in the `head` section for auto-linking
      all CSS and JS files found in the input tree (or restricted to specific directories only).
    * `@:todo` is a little helper directive to overcome the problem that Markdown does not have comment syntax.
      Renderers will simply ignore the content of the directive.
    
* EPUB Support
    * Tweak defaults to accommodate for the fact that some popular readers like iBooks do not support the full standard
      for their navigation menus. Default navigation depth is now only 2 for EPUB output.
    * Introduction of special suffixes to distinguish CSS for EPUB from site CSS: `*.epub.css` will only be used 
      for EPUB, `*.shared.css` will be used for EPUB and site, while all other CSS files will only be used for the site.
    
* API Change for Theme Support
    * All transformers, parsers and renderers from the `laika-io` module are now provided as a cats-effect `Resource`.
      This change was necessary as themes are themselves passed to transformers as a `Resource` as they might
      require side-effecting initialization logic.
      See the [Migration Guide](07-migration-guide.md#versions-older-than-0-16-0) for the (trivial) changes that are necessary for code building transformers.

* Bugfixes
    * EPUB: the library could produce invalid XML metadata files for the EPUB container under some circumstances,
      e.g. a headline staring with a number or containing an ampersand.
    * The `excludeFromValidation` flag was ignored in some scenarios.
    * An AST rewrite rule returning an explicit `Retain` could override a `Replace` action from a previous rule.

* Project Maintenance
    * Update dependencies to cats 2.2.0, cats-effect 2.2.0 and ScalaTest 3.2.2.
    * Solely use sbt's slash syntax in Laika's build and all configuration examples for Laika's sbt plugin
    * Remove all deprecated classes and methods.
    * Remove legacy directive syntax that had been deprecated since 0.12.
    
* This release is identical with 0.16.0 which had a broken `laika-pdf` artifact.


0.15.0 (May 29, 2020)
---------------------

* Enhanced Navigation Features
    * Internal references are now fully validated and dead links cause the transformation to fail by default.
    * Introduce global link definitions that allow to avoid repetition with centralized declarations of internal
      or external link targets.
    * Introduce new `@:navigationTree` directive that allows to combine auto-generated navigation trees
      with explicit manual entries. Deprecates the old `@:toc` directive.
    * Add new `@:breadcrumb` directive that produces a list of links from the root to the current document.
    * Introduce pluggable slug builder in `ExtensionBundle` to allow customizations of how a section headline
      is translated to an id.
    * Introduce "Link Directives", a new directive type that allows to define shortcuts for commonly used
      URL patterns, (e.g. `@:rfc(2022)`).
    * Add a new `@:api` link directive as a shortcut for linking to API documentation.
    * Change default names for title documents to `README.<suffix>` for the input for better GitHub integration 
      and `index.<suffix>` for the output for better site navigation.
      Defaults can now be overridden in configuration.
    * Add methods for navigating siblings in Cursor API for document trees.
    * Laika now always picks the first header as a title, even when subsequent headers are on the same level.
    * Improved precision in detecting ambiguous references.
    * Internal overhaul/rewrite of the link resolver algorithm which was still the original design from 2013
      and degraded in clarity over several feature additions.
* Configuration
    * Introduce programmatic ways to declare all global configuration for both, sbt plugin and library API,
      which now allows to completely avoid configuration files unless per-directory configuration is required.
    * Change HOCON configuration keys to always be in a namespace (e.g. `laika.title` or `cursor.currentDocument`),
      to reduce the likeliness of name clashes with user-defined variables.
* Error Handling
    * The presence of invalid nodes in the document AST before rendering now always triggers an abortion
      of the transformation with all messages from invalid nodes listed in the error result.
    * The new default behaviour can be overridden in configuration to switch to "visual debugging" where
      invalid nodes are rendered in their original position in the result.
* Changes in the Document AST related to navigation enhancements
    * Merge the functionality of `CrossLink` into `InternalLink`
    * Merge `ExternalLink` and `InternalLink` to `SpanLink`
    * Split `Temporary` marker trait into `Unresolved` and `Hidden`
* Manual
    * Complete rewrite of the manual which still had its initial structure from 2012.
    * Better separation of tutorial-style from reference-style sections.
    * New chapters about Laika's parser combinators, HOCON API, design goals, configuration options,
      navigation features, writing parser extensions and adding new syntax highlighters.
* Bugfix
    * The combination of the `laikaSite` task with the `laikaIncludeEPUB` setting led to a zero-byte result.
    

0.14.0 (Feb 28, 2020)
---------------------

* Introduce support for Scala.js for the entire laika-core module:
    * Brings the complete functionality of Laika to Scala.js with the only
      exceptions being File/Stream IO and support for PDF and EPUB output
    * Eliminate the last, semi-hidden usages of Java reflection to enable Scala.js support
    * Avoid use of `java.time.Instant` in the shared base to not force another heavy dependency
      on Scala.js users
* Laika's integrated syntax highlighting:
    * Add support for Dotty, JSX/TSX, SQL, EBNF, Laika's own AST format
* Parser APIs:
    * Introduce `PrefixedParser` trait to make span parser optimizations implicit
      for 90% of the use cases and no longer require the developer to explicitly 
      supply a set of possible start characters for the span
    * Introduce new parser for delimiters that generalizes typical checks for preceding
      and following characters in markup parsers (e.g. `delimiter("**").prevNot(whitespace)`),
      to reduce boilerplate for inline parsers
    * Introduce shortcuts for very common usages of text parsers (e.g. `oneOf('a','b')` instead of
      `anyOf('a','b').take(1)`)
    * Expand the API of the `Parser` base trait: add `source` method for obtaining the consumed
      part of the input instead of the result of the parser, and `count` to obtain the number of
      consumed characters
    * Introduce `laika.parse.implicits._` for extension methods for common parsers, e.g.
      `.concat`, `mapN`.
    * Introduce shortcut for repeating a parser with a separator
    * Deprecate all APIs that relied on implicit conversions
    * Deprecate some of the rather cryptic symbol methods on `Parser` in favor of named methods
    * `DelimitedText` does no longer have a type parameter
* AST changes and improvements:
    * Introduce RelativePath as a separate type in addition to the existing Path type
      which is now only used for absolute paths
    * Introduce a range of shortcut constructors for Laika's AST nodes, that allow you
      to write `Paragraph("some text")` instead of `Paragraph(Seq(Text("some text")))`
      for example
* Directives:
    * Cleaner syntax for default attributes which are no longer part of the HOCON
      attribute block for named attributes
* Demo App:
    * Cleanup from six to two (bigger) panels for improved usability
    * Use Laika's own syntax highlighting in the output
    * Allow to switch from JVM execution to Scala.js
    * Move from Akka HTTP to http4s
    * Integrate source into Laika's main repository
* Bugfix:
    * Syntax highlighting for interpolated strings in Scala was broken in 0.13.0
    * Fix a flaky test that relied on the ordering of HashMap entries
    

0.13.0 (Jan 26, 2020)
---------------------

* Introduce integrated syntax highlighting based on the libraries own parsers
    * Resulting AST nodes for code spans are part of the document AST and
      can be processed or transformed like all other nodes
    * Works with any renderer, including PDF
    * Initially supported are Scala, Java, Python, JavaScript, TypeScript,
      HTML, CSS, XML, JSON, HOCON
    * Convenient base parsers for common syntax like string and number literals
      or identifiers to facilitate development of new syntax highlighters
* HOCON parser: add support for `include` statements, this final feature addition 
  makes Laika's HOCON support fully spec-compliant      
* New transformation hooks in the `laika-io` module for parallel transformers: 
  `mapDocuments(Document => Document)`, `evalMapDocuments(Document => F[Document])` 
  and the corresponsing `mapTree` and `evalMapTree`
* Transformer introspection: introduce `describe` method for parsers, renderers
  and transformers and `laikaDescribe` setting in the sbt plugin that provides
  formatted information about the transformer setup and installed extensions
* sbt plugin: improved accuracy for caching logic for EPUB and PDF output
  that still works when the artifact name or version changes
* Upgrade dependency on cats-core to 2.1.0


0.12.1 (Dec 1, 2019)
--------------------

* Fixes and Improvements for the new HOCON parser introduced in version 0.12.0
    * Significant improvements for error messages in HOCON parser
    * Fixes for nested self references, missing optional self references,
      and objects without '=' or ':' separator
* Parser Combinators: The '|' alternative parser now keeps the failure with the
  most processed characters and not the last one, for improved error messages
* Fix for script tag with attributes not being recognized in verbatim HTML in Markdown


0.12.0 (Oct 30, 2019)
---------------------

* New laika-io Module
    * Functionality extracted from existing laika-core module
    * Contains File/Stream IO, EPUB output, Parallel transformations
    * Based on cats-effect type classes (in "Bring-Your-Own-Effect" style) 
    * Leaves laika-core pure, preparing it for later support for Scala.js
* Referential Transparency
    * No method in the public API throws Exceptions anymore
    * The result of pure operations is provided by instances of `Either`
    * The result of side-effecting operations is provided by a return type of `F[A]`,
      with `F[_]` being an effect type from cats-effect
    * Eliminated all uses of runtime reflection
* Changes to the APIs for creating and running parsers, renderers and transformers
    * Necessary due to the changes listed above
    * See the migration guide for details and examples  
* Changes to the Directive Syntax in Templating
    * The syntax of separators for the attribute and body sections have changed
    * HOCON syntax is now used for attributes
    * The old syntax is still supported, but will be removed at some point before the 1.0 release
* Changes to the Directive DSL for creating directives
    * `attribute(Default)` is now `defaultAttribute`
    * `body` is now either `parsedBody` or `rawBody`
    * Type conversions happen with the new `as` method: `attribute("title").as[String]`,
      based on the `ConfigDecoder` type class that is also used for the new Config API
    * Named body parts have been replaced by the more flexible Separator Directives
    * The built-in helper for mapping directive parts with different arity has
      been replaced by cats' `mapN`
* Refactoring of AST Rewrite API to be fully type-safe and avoid runtime reflection and exceptions.
    * Return types are now more explicit (e.g. `Replace(newSpan)` instead of `Some(newSpan)`)
    * Rules for rewriting spans and blocks get registered separately for increased
      type-safety, as it is invalid to replace a span with a block element.
* Refactoring of the Render API to be referentially transparent
    * Also applies to the API for registering custom renderers for individual AST nodes
* New Config API and built-in HOCON parser
    * Removed the dependency on the Typesafe Config library and its impure Java API
    * Added a new lightweight and pure HOCON parser as part of laika-core,
      supporting the full spec except for file includes (for now).
* Enhancement for the DocumentTree API      
    * The result of a tree parsing operation is now a new type called `DocumentTreeRoot`
    * It has a `coverDocument` property and contains the recursive tree structure of the parsed content.
    * Each `DocumentTree` in the structure now has an explicit `titleDocument: Option[Document]` property
      for more explicit content organization in e-books.
    * Properties that previously held references to streams and other impure data had been
      removed from the pure content model (e.g. `DocumentTree.staticDocuments`). 
* Bug fixes for fenced code blocks with blank lines in GitHub-Flavored Markdown      


0.11.0 (June 12, 2019)
----------------------

* New Renderer for producing EPUB containers
* New `laikaEPUB` task in the sbt plugin
* New `laikaIncludeEPUB` setting for the `laikaSite` task
* Support for cover images for EPUB and PDF
* Support for document metadata (author, language, date, etc.) for EPUB and PDF
* Support for title pages per chapter
* Backwards-compatible to 0.9.0 and 0.10.0 - if you update from earlier version, please see
  the release notes for 0.9.0 for migration


0.10.0 (Dec 1, 2018)
--------------------

* Add support for GitHub Flavored Markdown:
    * Tables
    * Fenced Code Blocks
    * Auto-Links
    * Strikethrough
* Preparing for Scala 2.13    
    * Adjust use of Collection API for breaking changes and deprecations in 2.13
    * Replace use of parallel collections with custom executor
* Level of parallelism of transformations is now configurable


0.9.0 (Sep 15, 2018)
--------------------

* New ExtensionBundle APIs allow to bundle extensions into a single object for easier reuse. Supported extension
  hooks include directives, markup parser extensions, rewrite rules, custom renderers, document type matchers, 
  alternative parsers for stylesheets, templates or configuration headers and default templates per output format. 
* Reduced number of settings and tasks in the sbt plugin, by using the new ExtensionBundle API for sbt settings.
* Improved package structure to reduce number of required imports. 


0.8.0 (June 4, 2018)
--------------------

* Doubles parsing speed for both Markdown and reStructuredText
* Much lower number of parser instance creations on repeated runs
* Performance goals had been achieved through replacing the former
  Scala SDK parser combinators with a custom, optimized combinator design:
    * Fewer dependent types and base parsers in objects instead of traits, making it easier to freely compose parsers
    * Create parser error messages lazily, as most of them will never be accessed
    * Avoid cost of by-name args in all cases except | combinator
* Add support for size and align options for the image directive in reStructuredText
* Fixes for all bugs known and reported to this point 
* Address all deprecation warnings for Scala 2.12    


0.7.5 (Dec 30, 2017)
--------------------

* Support for sbt 1.0
* Laika's sbt plugin is now an AutoPlugin
* Prefixed all task and setting keys to adhere to recommended naming pattern
  (e.g. laikaGenerate) to avoid name conflicts for autoImports
* Adjustments for API changes in sbt 1.0
* Bug fixes in the library
* Drop support for sbt 0.13 and Scala 2.10


0.7.0 (April 17, 2017)
----------------------

* Support for Scala 2.12 (with continued support for 2.11 and 2.10)
* Redesign and cleanup of the Document API: use case classes wherever possible,
  extract features into pluggable traits and introduce a new `Cursor` type for tree rewriting
* Allow to customize the `FopFactory` for the PDF renderer (in API and sbt plugin)
* Fix an issue in the `laika:site` task in the sbt plugin that executed several sub-tasks
  twice which could lead to IllegalStateExceptions caused by the resulting race condition
* Fixes for the reStructuredText parser (for option lists and IP addresses)


0.6.0 (May 23, 2016)
--------------------

* Support for rendering PDF documents
* Support for rendering XSL-FO output
* New CSS parser supporting a large subset of standard CSS
* Support styling of PDF documents with CSS
* Support for different templates per output format
* New sbt tasks: `html`, `pdf`, `xslfo`, `prettyPrint` for rendering
  a single output format
* New sbt task `generate` for rendering one or more output formats
  (e.g. `laika:generate html pdf`)
* Integrate PDF output into existing sbt task `laika:site` via
  new setting `includePDF`
* New directives `pageBreak`, `style` and `format`
* Changes to the `Render` and `Transform` API to allow for the
  merging of an entire directory of input files into a single output
  file (as required by PDF rendering)


0.5.1 (Oct 10, 2015)
--------------------

* Cross-compile for Scala 2.11 and 2.10
* Publish the sbt plugin to the new plugin repository on Bintray
* Upgrade to ScalaTest 2.2.4


0.5.0 (Jan 9, 2014)
-------------------

* New sbt plugin, exposing all Laika features and customization hooks as sbt tasks and settings
* New option to merge multiple input directories into a tree structure with a single root,
  allowing to keep reusable styles or templates ("themes") separately
* New option to use Markdown and reStructuredText markup in the same input tree, including
  cross-linking between the two formats
* Move to a multi-project build and rename the main artifact from `laika` to `laika-core`
* Upgrade to ScalaTest 2.0 and sbt 0.13
* Drop support for Scala 2.9.x


0.4.0 (Nov 22, 2013)
--------------------

* Template-based site generation
* Support for tables of contents, convenient cross-linking between documents 
  and autonumbering of documents and sections for all supported markup formats
* Custom Directives for templates and text markup
* Document Fragments that can be rendered separately from the main document content
* New API for batch processing for parse, render or full transform operations
* Parallel processing of parsers and renderers 

  
0.3.0 (Aug 3, 2013)
-------------------

* Support for most of the standard directives and text roles of the reStructuredText reference
  parser (admonitions, `figure`, `image`, `code`, `raw` and many more)
* Now integrates the official Markdown test suite (and many fixes to make it pass)
* Now integrates a test for transforming the full reStructuredText specification (which, of
  course, is written in reStructuredText) and many fixes to make it pass
* Adds the renderer option `HTML.unformatted` for terse output without indentation or whitespace
  (often desirable when writing the rendered document to a database for example)
* Adds a new Demo App to try out Laika online
* General cleanup of parser implementations and alignments between Markdown and reStructuredText
  parsers, making both of them much more robust


0.2.0 (May 7, 2013)
-------------------

* Support for reStructuredText (full specification)
* Concise and type-safe API for all reStructuredText extensibility options (directives, text roles)
* New document tree nodes for tables, footnotes, citations, definition lists, internal links,
  comments, system messages, invalid elements
* Render hints for document tree nodes in the form of the new Customizable trait


0.1.0 (Jan 12, 2013) 
--------------------

* Support for Markdown as input
* Support for HTML and AST as output
* Customization hooks for renderers
* Document tree rewriting
* Various options for input and output (strings, files, java.io.Reader/Writer, java.io streams)
* Generic base traits for markup parser implementations
