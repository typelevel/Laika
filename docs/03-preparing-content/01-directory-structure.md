
Directory Structure
===================

Laika does not have any special directories and content can be nested in sub-directories down to arbitrary levels.

When you are using the sbt plugin or the tree transformer from the library API that accepts directories as input,
Laika supports additional file types and navigation features on top of just parsing text markup.
This section describes the supported [Document Types], how [Auto-Generated Navigation] works with
directory input and how the [Virtual Tree Abstraction] decouples the logic from the file system. 
 
This set of functionality is not available when you are using a transformer or parser from the library API
that expects a single input (file, stream or string) for processing.


Document Types
--------------

The library distinguishes between the following file types:


### Markup Files

Files with the extensions `.md`, `.markdown` or `.rst` will be parsed and rendered 
to the target in the same directory structure and with the same file names apart from the suffix,
which will be replaced depending on the output format (e.g. `.html`).

Apart from standard markup syntax, markup files in Laika can also contain the following non-standard elements 
(unless running in strict mode which disables all extensions):

* A configuration header enclosed between `{%` and `%}` in HOCON format
* A document title for representing the page in navigation structures.
  The title is obtained from the first header element in the page unless overridden with the `laika.title` key
  in the configuration header.
* Sections, determined by the hierarchy of headers coming after the title, 
  with auto-assigned ids based on a pluggable slug builder. See [Customization Hooks] for details.
  Sections can be part of auto-generated navigation structures and can conveniently be linked to by header text
  with "native" markup link syntax [Linking by Section Headline]. 
* Directives, which extend the text markup's functionality, either one of the built-in [Standard Directives]
  or your own, custom implementations.


### Title Documents

Each directory can contain an optional title document. 
It is recognized by the file name pattern `README.<suffix>`, e.g. `README.md`.
When rendering the name will change to `index.<suffix>`, e.g. `index.html`.

These defaults have been chosen so that markup files appear below directory navigation on GitHub
and rendered HTML files can serve as index pages. 
The names can be overridden in Laika's global configuration (you need to omit the suffix, as it'll work with all formats):

@:select(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue(LaikaKeys.titleDocuments.inputName, "title")
  .withConfigValue(LaikaKeys.titleDocuments.outputName, "title")
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(LaikaKeys.titleDocuments.inputName, "title")
  .withConfigValue(LaikaKeys.titleDocuments.outputName, "title")
  .build
```
@:@

Title Documents will also render on a level above the other chapter documents in the navigation,
(see [Auto-Generated Navigation] for details) and will be linked in [Breadcrumbs] components.
  

### Configuration Files

Each directory can contain an optional `directory.conf` file for configuring the directory (and its sub-directories).
The expected format of the document is HOCON.

The available options are described in [Configuration for Directories] below.


### Template Files

You can provide a default template per directory with the name `default.template.<suffix>`,
where the suffix matches the output format (e.g. `.html`). 
They will also be applied to sub-directories, unless overridden. 
Default templates in one of your input directories always override default template provided by themes.

Additionally you can add templates with the name pattern `*.template.<suffix>`, which will only
be applied when a markup document explicitly refers to them in its configuration header.

For more details on the template engine, see the chapter [Creating Templates].


### Static Files

All other files, like CSS, JavaScript, images, etc., will be handled depending on the output format:
 
* **HTML**: When generating sites, static files will be copied over to the 
  target in the same directory structure and with identical file names.
  
* **EPUB**: Static files will be integrated into the EPUB container,
  in the same directory structure and with identical file names.
  See [E-Books (EPUB & PDF)] for supported file types and other options.
  
* **PDF**: When generating PDF only assets actually used and referred to by markup pages
  or templates will be embedded in the final binary file.
  See [E-Books (EPUB & PDF)] for supported file types and other options.


Auto-Generated Navigation
-------------------------

The directory structure in a Laika project is not only a way to organize files in a workspace,
the structure will also be used by various features of the library that auto-generate navigation structures. 

* **EPUB & PDF**: The supported E-book formats will include auto-generated navigation. 
  The hierarchy of the navigation will match the directory structure of your inputs and the configured navigation order. 
  The actual content will get flattened into a linear book flow, 
  with title documents becoming the first section in each chapter.
  See [E-Books (EPUB & PDF)] on how to configure navigation depth and other options.

* **Web Sites**: For HTML output you can use directives like [The navigationTree Directive] in templates to auto-generate
  a navigation structure from the root of your input tree or any other specific sub-node.
  The default Helium theme also provides a main navigation tree in the left sidebar and page navigation on the 
  right out of the box.

The presence of title documents would determine how exactly chapter title are rendered in the navigation structure.

@:pageBreak

**Example for a structure with title documents** 

@:image(../img/dir-with-title-docs.png) {
  alt = Input Directory with Title Documents
  intrinsicWidth = 680
  intrinsicHeight = 405
}

In this example each directory has a title document. 
The title of that document is taken from the first header.
In the navigation tree on the right it is rendered one layer above the other documents in the same directory,
and the title is linked to the document.


**Example for a structure without title documents** 

@:image(../img/dir-without-title-docs.png) {
  alt = Input Directory without Title Documents
  intrinsicWidth = 680
  intrinsicHeight = 405
}
  
In this example there are no title documents and chapter titles are taken from the file `directory.conf`
in each directory.
In the navigation tree on the right these titles now only serve as separators and are not linked to any document.


Configuration for Directories
-----------------------------

Each directory can contain an optional `directory.conf` in HOCON format.
The configuration applies to the directory and all its sub-directories unless overridden on a lower level.


### Directory Title

The title for a directory can be set explicitly:

```hocon
laika.title = Introduction
```

This title will then be used in auto-generated navigation structures.
In case you are using [Title Documents] this step is not necessary as the title of that document will be used
instead by default (either coming from its first headline or overridden in its own configuration header).
If you still set this attribute for the directory it will override whatever title has been set for the title document.


### Navigation Order

You can also set the navigation order for the directory explicitly:

```hocon
laika.navigationOrder = [
  apples.md
  oranges.md
  strawberries.md
  some-subdirectory
  other-subdirectory
]

```

The directory in the example above contains both markup files and sub-directories.
They would appear in this order in tables of contents and the same order would be applied when autonumbering.

The default ordering, when not provided explicitly, is alphabetical.
Note that documents omitted from this list but present in the directory would still appear in navigation trees,
below the entries with an explicit order from configuration.
For excluding documents entirely, see [Limiting the Output Formats] below.  


### Limiting the Output Formats

If you produce multiple output formats, you may want to limit the formats that get rendered for some documents
or directories.
If, for example, you generate a documentation site that also contains blog entries, 
you might want to exclude the blog content from generated EPUB or PDF documents and point to the site instead.

In this case you first need to specify which target formats a directory or document should render to:

```hocon
laika.targetFormats = [html]
```

The `laika.targetFormats` key expects an array of string values.

Secondly, you may want to benefit from Laika's convenient feature of auto-translating all internal links to 
documents that are excluded from some formats to an external link instead.
For this to work, the library or plugin needs to know where your site is hosted.

If you are using the Helium theme, there is a property you can use for this purpose:

```scala
Helium.defaults.site.baseURL("https://my-docs/site")
```

If you are not using Helium, you can use the standard configuration API to set this value:

@:select(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue(LaikaKeys.siteBaseURL, "https://my-docs/site")
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .withConfigValue(LaikaKeys.siteBaseURL, "https://my-docs/site")
  .build
```
@:@

You can also set an empty array and prevent the rendering of any output format:

```hocon
laika.targetFormats = []
```

This might be useful if you have a directory that contains only snippets and partial documents
you want to use via the `@:include` or `@:embed` directives.
With an empty array you prevent not only the rendering of those document, 
they also won't show up in any navigation structure.

Finally, the `laika.targetFormats` key can also be used for individual documents, 
by placing it in the configuration header of a text markup document.  


### Disabling Link Validation

Normally an internal link will be validated and (with default error handling) cause the transformation to fail
if one or more targets are invalid.
A target is invalid if either the linked document does not exist, does not contain the specified id or fragment,
or does not support the same set of output formats as the referring document.

In some cases this kind of strict validation may not be desired. 
You may, for example, have an external process that populates a directory before or after Laika is run.
In this case you can disable validation for all link targets within that directory or its sub-directories:

```hocon
laika.validateLinks = false
```


Virtual Tree Abstraction
------------------------

While in most scenarios a single directory to provide all input files is probably sufficient, 
Laika's functionality is not tied to the file system in any way, but instead builds on top of a
virtual tree abstraction.

As a consequence, you can alternatively merge multiple directories into one logical tree 
or generate additional files programmatically and "mount" them at a specific point in the virtual tree.

When merged directories contain sub-folders with the same name, those will be merged recursively. 
Only files with the same name in the same folder are treated as errors.

@:image(../img/merged-directories.png) {
  alt = Merging Directories
  style = medium-image
  intrinsicWidth = 564
  intrinsicHeight = 718
}

In the example above two directories get merged into a single virtual root (`/`).
The `images` directory exists in both sources, so it will be merged recursively.

The configuration mechanism for specifying multiple input directories is different for the 
[sbt plugin][Preparing Content] and the [Library API][Merging Multiple Directories]

Internally, the virtual path is represented by the `laika.ast.Path` type and relative links between them by
`laika.ast.RelativePath`. These types are used frequently throughout the classes forming the document AST.
All internal links are expressed with with these virtual paths and not with file system paths.

@:todo(examples for abs/rel links in HOCON and programmatically)
