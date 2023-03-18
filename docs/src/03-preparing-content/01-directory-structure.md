
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

```scala mdoc:invisible
import laika.sbt.LaikaPlugin.autoImport._
import sbt.Keys._
import sbt._
```

### Title Documents

Each directory can contain an optional title document. 
It is recognized by the file name pattern `README.<suffix>`, e.g. `README.md`.
When rendering the name will change to `index.<suffix>`, e.g. `index.html`.

These defaults have been chosen so that markup files appear below directory navigation on GitHub
and rendered HTML files can serve as index pages. 
The names can be overridden in Laika's global configuration (you need to omit the suffix, as it'll work with all formats):

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.config.LaikaKeys

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LaikaKeys.titleDocuments.inputName, "title")
  .withConfigValue(LaikaKeys.titleDocuments.outputName, "title")
```

@:choice(library)
```scala mdoc:compile-only
import laika.config.LaikaKeys
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor

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

```scala mdoc
import laika.helium.Helium

Helium.defaults.site.baseURL("https://my-docs/site")
```

If you are not using Helium, you can use the standard configuration API to set this value:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.config.LaikaKeys

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LaikaKeys.siteBaseURL, "https://my-docs/site")
```

@:choice(library)
```scala mdoc
import laika.config.LaikaKeys
import laika.api._
import laika.format._

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


Versioned Documentation
-----------------------

Laika supports versioned documentation, where the current inputs are interpreted as belonging to one version only.
The Helium theme contains a version switcher in the top navigation bar.

### Configuration

Each directory and each individual document can be marked as either versioned or unversioned.
All versioned document will be rendered to a sub-directory of the root that contains only content for this
specific version.

Therefore, configuration for versioned documentation involves two steps:

**1) Configure all existing versions**

This is a global configuration artifact that you can define with the Helium configuration API:

```scala mdoc
import laika.rewrite.{ Version, Versions }

val versions = Versions(
  currentVersion = Version("0.42.x", "0.42", canonical = true),
  olderVersions = Seq(
    Version("0.41.x", "0.41"),
    Version("0.40.x", "0.40", fallbackLink = "toc.html")
  ),
  newerVersions = Seq(
    Version("0.43.x", "0.43", label = Some("dev"))
  )
)
Helium.defaults.site.versions(versions)
```

The two required properties of the `Version` class are `displayValue` which is the version name to be used in
UI elements and `pathSegment` which is the string to be used as part of URLs pointing to this version.

The remaining properties are optional. First, there is `label` which can be used to associate categories 
like `EOL`, `Stable` or `Dev` with each version.
Those three values come with default styles in the Helium CSS, but you can define additional labels if you manually
include the CSS for those.

Secondly, `canonical` is a boolean that allows to mark one version as the canonical version.
When using Helium this will trigger the automatic insertion of a `<link rel="canonical" ...` into the output's
`<head>` element in case the canonical version has a page with the same path. 
For all other cases the canonical link can alternatively be set manually, 
for details see [Metadata for Individual Documents].

Finally, `fallbackLink` allows to define a link target that the version switcher should pick, if a target version
does not have a page corresponding to the current page the user is on.

Note that this kind of "smart linking" currently only works if existing rendered versions can be found in the 
output directory of the transformer operation.
In all other cases, the version switcher will always use the `fallbackLink`.


**2) Configure which directories and documents are versioned**

This step is necessary as each documentation site may contain any number of documents with content that is independent
of the version of the project, e.g. a landing page, a blog/news section, a contributor guide, a COC, etc.

The most common scenario will be that most documents are versioned while only a few are not.
You can benefit from Laika's support for hierarchical configuration for this purpose, where a value can be set
for an entire directory, including sub-directories, unless overridden in a directory or document.

So most likely you would put a `directory.conf` into the root directory that switches versioning on:

```hocon
laika.versioned = true
```

And then exclude individual directories or documents by overriding this value with `false`.
When overriding for an individual document, you can do this with the standard HOCON headers (frontmatter),
enclosed between `{%` and `%}`.


### Index for Smart Version Switcher

The Helium theme contains a version switcher that is aware of the directory structure of all versions.
This means that when switching to a different version that contains the same document (based on its path and
filename) it will navigate to that document instead of the entry page of the other version.

This configuration step is entirely optional. If omitted, it will just mean that the version switcher
drop-down will always navigate to the start page of the target version.

The index for this flexible switching can be built up in two different ways, depending on your use case:

1) **Older versions rendered by other toolkits**: In this case you need to run a transformation once where
   the version configuration for Laika is complete, including all the older versions rendered by other tools
   and the configuration for the version scanner itself:
   
   ```scala mdoc:nest
   val versions = Versions(
     currentVersion = Version("0.42.x", "0.42"),
     olderVersions  = Seq(Version("0.41.x", "0.41", label = Some("EOL"))),
     scannerConfig  = Some(VersionScannerConfig(
       rootDirectory = "/path/to/old/site-output",
       exclude       = Seq(Root / "api")
     ))
   )
   Helium.defaults.site.versions(versions)
   ```

   The transformer will scan the `rootDirectory` and index all sub-directories on the top level where
   the directory name corresponds to the configured `pathSegment` of a version.
   The `exclude` property is a path within each version that will not be scanned
   (API documentation, for example, would bloat the generated JSON file even though the version switcher
   does not need those paths).
   
   This index will be written to `/laika/versionInfo.json` in the output directory.
   It needs to be part of the deployment, and can also be used as input for subsequent transformations (see below),
   so that the directory scanning is only necessary once.
   
2) **Using an existing `versionInfo.json` document**: 
   This more convenient option is available when either step 1 above has been performed once, 
   or the site has been rendered with Laika and version configuration from the beginning.
   Just place a valid, existing `versionInfo.json` document into the `laika` directory of any of your
   input directories.
   It will use this index during a transformation while amending any new documents found for the current version.


### Re-Rendering Older Versions

When switching to a maintenance branch to fix something in the documentation for an older version,
you might want to ensure that the build step excludes unversioned documents as you most likely would want
them to originate in the sources for the newest version.

You can achieve this by setting the `renderUnversioned` flag to `false` in your version config on
the maintenance branch:

```scala mdoc:nest
val versions = Versions(
  currentVersion = Version("0.42.x", "0.42"),
  olderVersions = Seq(),
  newerVersions = Seq(
    Version("0.43.x", "0.43", label = Some("dev"))
  ),
  renderUnversioned = false
)
Helium.defaults.site.versions(versions)
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
