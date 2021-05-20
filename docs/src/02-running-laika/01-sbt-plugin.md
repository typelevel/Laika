
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
addSbtPlugin("org.planet42" % "laika-sbt" % "0.17.1")
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
Laika / sourceDirectories := Seq(target.value / "somewhere-else")
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
  with the suffix matching the output format (e.g. `.html`). 
  They will also be applied to sub-directories, unless overridden. 
  Default templates in one of your input directories always override default templates provided by themes.
  You can also add additional templates with the name pattern `*.template.<suffix>`, 
  which will only be applied when a markup document explicitly refers to them in its configuration header.
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
laikaSite / target := target.value / "somewhere-else"
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
laikaEPUB / artifactPath  := target.value / "my-docs.epub"
laikaPDF / artifactPath   := target.value / "my-docs.pdf"
```

For other e-book configuration options like navigation structures and cover images see [E-Books (EPUB & PDF)].


### Including Scaladoc

The following setting causes the scaladoc output of your project to be included in the generated site:

```scala
laikaIncludeAPI := true
```

It will be copied into the `api` directory of your site's target, unless you change its setting:

```scala
laikaCopyAPI / target := (laikaSite / target).value / "somewhere-else"
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


Using the Preview Server
------------------------

Laika contains a preview server that can be used to browse generated sites.
Simply run the `laikaPreview` task and navigate to `localhost:4242` in the browser.
For overriding the default configuration for the preview server, see [laikaPreviewConfig setting] below.

If you are using versioned documentation you can tell the server where the other, existing versions are located,
so that it can serve those documents, too.
If you don't need to test the version switcher drop-down, you can omit this step.
For details about the configuration for the version scanner, see [Index for Smart Version Switcher].

The page will auto-refresh whenever changes to any input document are detected.
The default poll interval is 3 seconds.
If you are using an IDE with auto-save you might need to tweak its preferences
for seeing changes while editing the Markdown sources. 
IntelliJ, for example, only auto-saves when you run or compile an application or when you switch to a
different application in the OS. 
You can either use `cmd-S` to manually force saving or change the preferences to auto-save in fixed time intervals. 


Plugin Settings
---------------

The remaining configuration options are not specific to the plugin use case and merely mirror the features of the library API,
apart from differences in the syntax/mechanics which with they are applied, which are reflected in the corresponding code examples.
For this reason this section only gives a very brief overview while linking to the relevant sections in the other chapters.


### laikaTheme Setting

This setting allows to configure the built-in Helium theme or a 3rd-party theme you might use.
You can also use it to specify an empty theme if you want to put all templates and styles right 
into your input directory.

Example for applying a few Helium settings:

```scala
laikaTheme := Helium.defaults
  .all.metadata(
    title = Some("Project Name"),
    language = Some("de"),
  )
  .epub.navigationDepth(4)
  .pdf.navigationDepth(4)
  .build
```

Setting an empty theme:

```scala
laikaTheme := Theme.empty
```

If you do not use this property Laika will run with the default settings of the Helium theme, 
meaning your site and e-books will look exactly like this documentation, including all color and font choices.

Since the theme offers a lot of configuration options it has its own dedicated chapter [Theme Settings].

To just give a brief overview, those settings allow you to configure:

* The [Fonts] to embed in EPUB and PDF files or to link in HTML files.

* The [Colors] for the main theme and for syntax highlighting.

* Several aspects of the theme's [Layout], like column widths, block spacing or PDF page sizes.

* [Metadata] (title, authors, description, language, etc.) to include in the generated site or e-books.

* Adding [Favicons] or custom links to the [Top Navigation Bar]

* Adding an optional [Download Page] for the site's content as EPUB or PDF

* Providing logos, text and links for an optional [Website Landing Page].

* Configure [Cover Images for E-books] or [Auto-Linking CSS & JS Files].


### laikaConfig Setting

A builder-style API for basic configuration options grouped in a single sbt setting.
These are configuration options not tied to the default theme and are available even when using a 3rd party theme
or no theme at all.

Example:
  
```scala
laikaConfig := LaikaConfig.defaults.strict.withRawContent
```

- [Strict mode]: Disables all non-standard extensions Laika adds to the supported markup formats, like directives.

- [Raw Content]: Enables the inclusion of raw sections of the target format in markup files, 
  e.g. snippets of verbatim HTML embedded in Markdown files. By default this is disabled.

- [Character Encoding]: Sets the character encoding of input and output files, the default is UTF-8.

- [Error Handling]: Specify log levels or switch to "visual debugging", where recoverable errors are
  rendered in the page context where they occur instead of causing the transformation to fail.
  
- [User-Defined Variables]: Define variables globally that you can refer to in templates and markup files
  with substitution references.
      
      
### laikaExtensions Setting

Register implementations of the `ExtensionBundle` API, either provided by the library or extensions you created yourself.
  
Example:

```scala
laikaExtensions := Seq(GitHubFlavor, SyntaxHighlighting)
``` 

- [Overriding Renderers]: adjust the rendered output for specific AST node types.
  
- [AST Rewriting]: transform the document AST between parsing and rendering.
   
- [Implementing Directives]: install custom directives.
  
- Or use any other hook in [The ExtensionBundle API]).
 

### Configuring Input and Output

Most of these setting are introduced in the sections above.

- `Laika / sourceDirectories` default `Seq(sourceDirectory.value / "docs")`, usually `Seq("src/docs")`.
  Specifies one or more source directories to process to be merged into a tree with a single root.
  See [Preparing Content] for details.
  
- `Laika / target` - default `target.value / "docs",`, usually `Seq("target/docs")`.
  Specifies the directory where the plugin should generate the site. 
  See [Generating a Site] for details.
  
- `Laika / excludeFilter` - default `HiddenFileFilter`  
  Specifies files to exclude from processing.
  Note that Laika ignores any `includeFilter` you set, as the tool needs more than a simple yes/no
  decision for deciding how to process files. 
  
- `laikaDocTypeMatcher` extension:
  Where a simple `excludeFilter` is not sufficient you can customize the way Laika determines the document type.
  
    ```scala
    laikaExtensions += laikaDocTypeMatcher {
      case Root / "templates" / _ => DocumentType.Template
      case Root / "images" / _ => DocumentType.Static
    }
    ```
  
  The list of pattern matches can be incomplete, they will be added to the built-in default matches.
  
  
### Freely Composing Inputs

If you need additional flexibility instead of just configuring one or more input directories, 
e.g. when there is a need to generate content on-the-fly before starting the transformation,
you can use the `laikaInputs` setting. 
This setting completely overrides any value set with `Laika / sourceDirectories`.

```scala
val inputs = InputTree[F]
  .addDirectory("/path-to-my/markup-files")
  .addDirectory("/path-to-my/images", Root / "images")
  .addClasspathResource("my-templates/default.template.html", DefaultTemplatePath.forHTML)
  .addString(generateStyles(), Root / "css" / "site.css")
```

In the example above we specify two directories, a classpath resource and a string containing CSS generated on the fly.
By default directories get merged into a single virtual root, but in the example we declare a mount point
for the second directory, which causes the content of that directory to be assigned the corresponding logical path.

@:callout(info)

Always keep in mind that declaring inputs and outputs are the only places in the Laika universe where you'd ever
use concrete file system paths.
Beyond this configuration step you are entirely within Laika's virtual path abstraction and refer to other
resources by their virtual path.
This is true for linking, using image resources, specifying templates to use in configuration headers, and so on.
It means that everything you can refer to in your markup files needs to be included in the input composition step.

@:@

The `InputTreeBuilder` API gives you the following options:

* Add entire directories and optionally specify a "mount point" at which they should be linked in the virtual tree.

* Specify individual files or classpath resources.

* Add in-memory string which will enter the parsing pipelines like file resources.

* Add in-memory AST nodes which will by-pass the parsing step and added directly to the other document AST
  obtained through parsing.
  
When generating input on the fly it is usually a question of convenience or reducing boilerplate 
whether you choose to generate a string for parsing or the AST directly.

For the complete API see @:api(laika.io.model.InputTreeBuilder).


### Settings for the laikaSite task

Finally, there are three boolean flags that only affect the laikaSite task.

- `laikaIncludeAPI` - default `false` - see [Including Scaladoc].

- `laikaIncludeEPUB` - default `false` - see [Including EPUB and PDF].

- `laikaIncludePDF` - default `false` - see [Including EPUB and PDF].


### laikaPreviewConfig setting

You can override the defaults by for the `laikaPreview` task if necessary:

```scala
laikaPreviewConfig :=
  LaikaPreviewConfig.defaults
    .withPort(8080)
    .withPollInterval(5.seconds)
    .verbose
```

By default, the port is 4242, the poll interval is 3 seconds.
With the `verbose` options the console will log all pages served.


### Inspecting Laika's Configuration

Run `show laikaDescribe` to get a formatted summary of the active configuration,
installed extension bundles and lists of input and output files.

Example:

```
Parser(s):
  Markdown
  reStructuredText
Renderer:
  AST
Extension Bundles:
  Laika's Default Extensions (supplied by library)
  Laika's directive support (supplied by library)
  Laika's built-in directives (supplied by library)
  Default Syntax Highlighters for Code (supplied by library)
  Document Type Matcher for Markdown (supplied by parser)
  Default extensions for reStructuredText (supplied by parser)
  Support for user-defined reStructuredText directives (supplied by parser)
  Standard directives for reStructuredText (supplied by parser)
  Document Type Matcher for reStructuredText (supplied by parser)
Settings:
  Strict Mode: false
  Accept Raw Content: false
  Render Formatted: true
Sources:
  Markup File(s)
    File '/dev/project/src/intro.md'
    File '/dev/project/src/details.md'
  Template(s)
    In-memory string or stream
  Configuration Files(s)
    -
  CSS for PDF
    -
  Copied File(s)
    File '/dev/project/src/main.css'
  Root Directories
    -
Target:
  Directory '/dev/project/target/docs'""""
```
