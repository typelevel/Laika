
E-Books (EPUB & PDF)
====================

Laika puts equal emphasis on the generation of e-books as on website generation.

Like all functionality, e-book generation does not require installation of external tools.
The EPUB format is supported by Laika's own Scala implementation, 
whereas PDF support is partially based on [Apache FOP].

The latter is not based on LaTeX like many other PDF renderers, but uses XSL-FO as an interim format.
The generation of this interim format happens in Laika's own renderers, 
only the final transformation step to the binary PDF format is delegated to Apache FOP.

[Apache FOP]: https://xmlgraphics.apache.org/fop/


Generating E-Books
------------------

If you are using the sbt plugin you can use several of its task for generating e-books:

* `laikaEPUB` or `laikaPDF` for transforming a directory of input files to a single output format
* `laikaGenerate epub pdf <other formats>` for transforming a directory of input files to EPUB and PDF
  and other output formats with a single parse operation
* `laikaSite` for generating a site that also contains PDF and/or EPUB files for download
  when setting the `laikaIncludeEPUB` and/or `laikaIncludePDF` settings to true.

See [sbt Plugin] for more details.

When using the library API, the `EPUB` and `PDF` renderers can be passed to the `Transformer` or `Renderer` APIs:

```scala
val transformer = Transformer
  .from(Markdown)
  .to(EPUB)
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build

transformer
  .fromDirectory("src")
  .toFile("hello.epub")
  .transform
```

See [Library API] for more details on these APIs and ensure you have the necessary dependencies in place -
the `laika-pdf` module for PDF or the `laika-io` module for EPUB - see [Dependencies].


Directory Structure
-------------------

An e-book generated by Laika is always a single, binary file, even when the input is an entire directory of markup files.
But the directory structure will be reflected in the navigation elements inside the e-book.

Markup files from the input directory will form the content of the e-book 
while static files like images and fonts will be embedded into the output 
when they are referenced from within one or more markup files. 

For EPUB specifically it is important to consider what file size to choose for your content.
The directory structure inside the generated EPUB container (which is essentially a glorified ZIP) 
will mirror exactly the structure of the input directory,
apart from the additional metadata files it needs to generate.
Therefore it is best to avoid very large markup files as they might slow down the experience in the e-book reader,
and instead distribute content over a higher number of smaller input files.

See [Document Types] for general info about the various types of input files Laika supports,
and [Supported Document Types] in this chapter for additional info specific to EPUB and PDF.


Book Navigation
---------------

Laika supports a directory structure with sub-directories of any depth. 
Markup files from the input directory will form the content of the e-book, 
linearized in depth-first traversal and using your configured [Navigation Order][Configuration Files].

Laika will generate navigation elements compatible with e-book readers,
as shown in the images below:

PDF Navigation in Preview for Mac:

@:image(../img/nav-pdf.png) {
  title = PDF Navigation
  width = 473px
}

EPUB Navigation in iBooks:

@:image(../img/nav-epub.png) {
  title = EPUB Navigation
  width = 473px
}

The navigation depth is unlimited by default and will also include links to each section inside your documents.
The depth to traverse can be changed via Laika's global configuration:

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue(BookConfig(navigationDepth = Some(3)))
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(BookConfig(navigationDepth = Some(3)))
  .build
```
@:@

In the example above, the specified `BookConfig` will apply to both formats, EPUB and PDF.
They can alternatively be set separately:

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue(EPUB.BookConfig(navigationDepth = Some(3)))
  .withConfigValue(PDF.BookConfig(navigationDepth = Some(4)))
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(EPUB.BookConfig(navigationDepth = Some(3)))
  .withConfigValue(PDF.BookConfig(navigationDepth = Some(4)))
  .build
```
@:@


Cover Images
------------

A cover image can be specified for EPUB and PDF:

@:choices(config)

@:choice(sbt)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(BookConfig(
    coverImage = Some(Root / "images" / "book-cover.jpg")
  ))
  .build
```

@:choice(library)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue(BookConfig(
    coverImage = Some(Root / "images" / "book-cover.jpg")
  ))
```

@:@


Or two different cover images can be configured for EPUB and PDF separately:

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue(EPUB.BookConfig(
    coverImage = Some(Root / "images" / "epub-cover.jpg")
  ))
  .withConfigValue(PDF.BookConfig(
    coverImage = Some(Root / "images" / "pdf-cover.jpg")
  ))
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(EPUB.BookConfig(
    coverImage = Some(Root / "images" / "epub-cover.jpg")
  ))
  .withConfigValue(PDF.BookConfig(
    coverImage = Some(Root / "images" / "pdf-cover.jpg")
  ))
  .build
```
@:@

See [Supported Document Types] for a list of supported image formats.


Document Metadata
-----------------

You can add document metadata to the configuration that Laika will apply to the generated output formats where supported:

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue(BookConfig(metadata = DocumentMetadata(
     identifier = Some("urn:isbn:978-3-16-148410-0"),
     authors = Seq("Deborah Green", "Maria Brown"),
     language = Some("en:GB"),
     date = Some(Date.from(Instant.now))
  )))
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(BookConfig(metadata = DocumentMetadata(
     identifier = Some("urn:isbn:978-3-16-148410-0"),
     authors = Seq("Deborah Green", "Maria Brown"),
     language = Some("en:GB"),
     date = Some(Date.from(Instant.now))
  )))
  .build
```
@:@

Note that the `DocumentMetadata` type uses the venerable `java.util.Date` API.
This is for staying platform-neutral between JVM and Scala.js 
which does not have the `java.time` APIs in its core library.


Supported Document Types
------------------------

You can also place images, fonts and other supported file types into the input directory.
Laika will embed these files in the generated EPUB container and/or PDF file.

The supported file types / suffixes are:

**For EPUB**

* Images: `jpg`, `jpeg`, `gif`, `png`, `svg`
* Audio: `mp3`, `mp4`
* HTML: `html`, `xhtml`
* JavaScript: `js`
* CSS: `css`   
* Fonts: `woff2`, `woff`, `ttf`, `otf` 

**For PDF**

* Images: `jpg`, `jpeg`, `gif`, `png`, `svg`, `bmp`, `tiff`
* Fonts: `pfb` (Type 1), `ttf`, `otf` 

@:todo(example for registering Fonts with FOP)


CSS for EPUB
------------

Since content files for EPUB are standard XHTML files (apart from optional EPUB-specific attributes), 
you can style your e-books with standard CSS. 

It is sufficient to simply place all CSS into the input directory,
alongside the text markup and other file types. 
References to these CSS files will be automatically added to the header section of all generated HTML files. 

When referencing images or fonts from your CSS files,
you can use relative paths, as the directory layout will be retained inside the EPUB container.


CSS for PDF
-----------

Laika offers the unusual, but convenient feature of CSS styling for PDF.
It allows for customization in a syntax familiar to most users. 

The CSS files need to be placed into the root directory of your input tree with a name
in the format `<name>.fo.css`.

However, as PDF is a page-based format and in Laika's case expects `XSL-FO` as an interim format,
there are a few subtle differences:

* All CSS attributes must also be valid XSL-FO attributes. 
  There is quite a big overlap, so you can use the familiar `font-family`, `font-weight`, `font-size`
  attributes as well as most of the border, padding and margin attributes.
  For an overview over the available attributes you can refer to the [Formatting Properties][fo-props] chapter
  in the XSL-FO specification.
  
* While id selectors and class selectors function like in web CSS, the type selectors are quite different.
  Since there is no interim HTML result, the types you can refer to are not HTML tag names like `div`,
  but the class names of the Laika AST nodes, e.g. `Header` or `Paragraph`.
  See [The Document AST] for details.
  
* Some selector types of the CSS specification are not supported as most of them do not add much value
  in the context of a Laika document tree:
  
    * Pseudo-classes like `:hover`
    * Attribute selectors like `[attribute~=value]` (since Laika nodes do not have many properties)
    * `Element1+Element2` or `Element1~Element2` for selecting based on sibling elements

[fo-props]: http://www.w3.org/TR/xsl11/#pr-section

An example for styling a level-2 header:

```css
Header.level2 {
  font-family: sans-serif;
  font-weight: bold;
  font-size: 14pt;
}
```

The `Header` type selector refers to the name of the AST node Laika produces for headers.
The `level2` class is a style that gets rendered for each header node to be able to style levels differently

Only a few of Laika's nodes get rendered with class attributes. 
You can examine HTML output to get an overview over rendered attributes as they are largely identical to those
rendered for EPUB and PDF. 

Likewise, out of the box only a subset of nodes get rendered with an auto-generated id: 
headers, footnotes or citations.


Customized Output
-----------------

In cases where styling with CSS alone is not sufficient, there are additional,
lower-level hooks to customize the rendered output.

### Templates

You can place custom default templates into the root directory of the input tree,
named `default.template.epub.xhtml` for EPUB and `default.template.fo` for PDF.

For EPUB the template will be applied to each rendered file individually,
for PDF the generated XSL-FO will first be concatenated and then the template will be applied 
to the single final FO document.

Customizing the PDF template would require knowledge of `XSL-FO`, but is hopefully rarely ever
necessary as PDFs can be styled by Laika's [CSS for PDF] feature.

See [Creating Templates] for general info about Laika's template engine.


### Overriding Renderers

Apart from adjusting the surrounding template the AST nodes will be rendered into, 
you can also customize how each individual AST node itself is rendered. 
 
This is a general customization hook in the library and not different from overriding renderers for site output.
See [Overriding Renderers] for more details.


Configuring Apache FOP
----------------------

Rendering a PDF file is (roughly) a 3-step process:

1. Parsing all markup files and producing an in-memory document tree representation
2. Applying templates and CSS to the document tree and render it as XSL-FO
3. Produce the final PDF file from the resulting XSL-FO

While step 1 and 2 are entirely managed by Laika, without any dependencies to external
libraries or tools, step 3 is almost solely taken care of by Apache FOP. 

Therefore, any customization for this step is best left for the configuration hooks of FOP itself.
They allow configuration of aspects like the target resolution, custom fonts, custom stemmers and a lot more.
The available options are described in the [Apache FOP documentation][fop-config-docs].

When you are using the sbt plugin, you can specify an Apache FOP configuration file with
the `fopConfig` setting:

```scala
fopConfig := Some(baseDirectory.value / "customFop.xconf")
```

Note that the default is `None` as FOPs default configuration is often sufficient.

When you are using the library API, the PDF renderer has a hook to specify a custom
`FopFactory`:

```scala
val configFile = "/path/to/customFop.xconf"
val factory = FopFactory.newInstance(new File(configFile))

val transformer = Transformer
  .from(Markdown)
  .to(PDF.withFopFactory(factory))
  .using(GitHubFlavor)
  .io(blocker)
  .parallel[IO]
  .build
```

Note that a `FopFactory` is a fairly heavy-weight object, so make sure that you reuse
either the `FopFactory` instance itself or Laika's `PDF` transformer.
In case you do not specify a custom factory, Laika ensures that the default
factory is reused between renderers.

[fop-config-docs]: https://xmlgraphics.apache.org/fop/2.1/configuration.html