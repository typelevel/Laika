
sbt Plugin
==========

The core of the Laika toolkit does not depend on sbt in any way. 
The sbt plugin merely provides a thin layer on top of the library API, 
exposing Laika's feature set as sbt tasks and settings.

This chapter only covers the topics which are specific to using Laika with sbt, 
while most other parts of the manual apply to both use cases, library and plugin.


Adding the Plugin to the Build
------------------------------

Laika's sbt plugin requires sbt 1.x.

If you still need to use sbt 0.13.x, you can use Laika 0.7.0 which was the last release
supporting that sbt version.

First add the plugin to `project/plugins.sbt`:

```scala
addSbtPlugin("org.planet42" % "laika-sbt" % "0.14.0")
```

Then enable the plugin in your project's `build.sbt`:

```scala
enablePlugins(LaikaPlugin)
```


Preparing Content
-----------------

The plugin expects the source files in the `src/docs` directory inside your project.
This default can be overridden with:

```scala
sourceDirectories in Laika := Seq(target.value / "somewhere-else")
```

You can provide multiple input directories which will be merged into Laika's [Virtual Tree Abstraction].

Laika does not have any special directories and content can be nested in sub-directories down to arbitrary levels.
For more details on how to organize content, see [Directory Structure].

The plugin distinguishes between the following file types:

* **Markup Files**: Files with the extensions `.md`, `.markdown` or `.rst` will be parsed and rendered
  to the target in the same directory structure and with the same file names apart from the suffix,
  which will be replaced depending on the output format (e.g. `.html`).
* **Configuration Files**: Each directory can contain an optional `directory.conf` file for specifying
  things like navigation order or chapter title. See [Configuration Files] for details.
* **Template Files**: You can provide a default template per directory with the name `default.template.<suffix>`
  with the suffix matching the output format (e.g. `.html`). They will also be applied to sub-directories, unless
  overridden. You can also add additional templates with the name pattern `*.template.<suffix>`, which will only
  be applied when a markup document explicitly refers to them in its configuration header.
  (The upcoming 0.16 release will introduce Themes which will provide default templates out of the box. 
  Until then you need to provide at least one default template in the root directory unless you are fine with the
  very basic fallback template.)
* **Static Files**: All other files, like CSS, JavaScript, images, etc., will be copied over to the 
  target in the same directory structure and with identical file names.


Generating a Site
-----------------

For parsing your markup files and generating a site simply run:

```scala
laikaSite
```

This will parse all files with the extensions `.md`, `.markdown` or `.rst` and render
them as `.html` files in the same directory structure as the input.

It will also copy all other file types over to the target folder 
(apart from templates and laika configuration files).

The site will be generated in the directory `target/docs/site` within your project.
The default can be changed with:

```scala
target in laikaSite := target.value / "somewhere-else"
``` 

Laika builds on sbt's caching features, therefore if neither any input files have been modified
nor any output files removed, it is a no-op.


### Including EPUB and PDF

EPUB and/or PDF files can be included with these settings:

```scala
laikaIncludeEPUB := true
laikaIncludePDF  := true
```

In this case the `laikaSite` task will generate EPUB and/or PDF files in the root directory of your output,
in addition to the generated HTML site.
The name of the files will be `<project-name>-<version>.<epub|pdf>` by default. You can change it with:

```scala
artifactPath in laikaEPUB := target.value / "my-docs.epub"
artifactPath in laikaPDF  := target.value / "my-docs.pdf"
```

For other e-book configuration options like navigation structures and cover images see [E-Books (EPUB & PDF)].


### Including Scaladoc

The following setting causes the scaladoc output of your project to be included in the generated site:

```scala
laikaIncludeAPI := true
```

It will be copied into the `api` directory of your site's target, unless you change its setting:

```scala
target in laikaCopyAPI  := (target in laikaSite).value / "somewhere-else"
```


Generating Individual Formats
-----------------------------

Instead of using the `laikaSite` task, you can also selectively pick the output formats to generate on each run
with the `laikaGenerate` task:

```scala
laikaGenerate html epub pdf
```

This will parse all files with the extensions `.md`, `.markdown` or `.rst` and render them in the specified list of formats. 
It is efficient in that the parsing of markup files only happens once to obtain a document AST and use this structure to render all formats.

Valid format arguments are `html`, `epub`, `pdf`, `xslfo`, `ast`. 
The latter is a debug output that renders the document AST in a formatted structure.

Finally if you only work with a single format there are also shortcut tasks for those: 
`laikaHTML`, `laikaEPUB`, `laikaPDF` and `laikaAST`.


Plugin Settings
---------------

The remaining configuration options are not specific to the plugin use case and merely mirror the features of the library API,
apart from differences in the syntax/mechanics which with they are applied, which are reflected in the corresponding code examples.
For this reason this section only gives a very brief overview while linking to the relevant sections in the other chapters.

- [Strict mode]: Disables all non-standard extensions Laika adds to the supported markup formats, like directives.

- [Raw Content]: Enables the inclusion of raw sections of the target format in markup files, 
  e.g. snippets of verbatim HTML embedded in Markdown files. By default this is disabled.

- [Character Encoding]: Sets the character encoding of input and output files, the default is UTF-8.

- [Error Handling & Debugging]: Specify log levels or switch to "visual debugging", where recoverable errors are
  rendered in the page context where they occur instead of causing the transformation to fail.

- **laikaExtensions Setting**: Use Laika's customization options to override renderers for specific AST nodes ([Overriding Renderers]),
  transform the document AST before rendering ([AST Rewriting]), install custom directives ([Implementing Directives])
  or use some of the lower level hooks in ([The ExtensionBundle API]).

- **laikaConfig Setting**: Offers basic configuration options like setting [Strict Mode], including [Raw Content],
  setting the [Character Encoding], controlling [Error Handling] and specifying [User-Defined Variables].

- [Inspecting Laika's Configuration]: Run `show laikaDescribe` to get a formatted summary of the active configuration,
  installed extension bundles and lists of input and output files.
