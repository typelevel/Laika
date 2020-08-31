
Creating Themes
===============

Creating your own theme is a way to build an alternative for Laika's default theme called Helium in cases
where its feature set or look and feel is not meeting your requirements.

The effort of designing and implementing a theme is justified if you plan to re-use it across multiple projects,
either as an in-house library or published as Open Source for the entire community.

If you just want to create a custom design for a single project, it's easier to just install an empty theme
and simply place all necessary templates and CSS files in your regular input directory.

@:select(config)

@:choice(sbt)

```scala
laikaTheme := Theme.empty
```

@:choice(library)

```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .io(blocker)
  .parallel[IO]
  .witTheme(Theme.empty)
  .build
```

@:@


The Theme Trait
---------------

Before describing the typical steps for implementing a theme, let's have a quick look at the (simple) `Theme` trait,
to get an idea of what a theme represents in technical terms.

```scala
trait Theme[F[_]] {

  def inputs: InputTree[F]
  
  def extensions: Seq[ExtensionBundle]
  
  def treeProcessor: PartialFunction[Format, Kleisli[F, ParsedTree[F], ParsedTree[F]]]
  
}
```

The `inputs` property is the most important aspect of the trait, as it allows to pre-populate the input tree
with templates and styles. 
See [Generating or Loading Documents] below for details.

The `extensions` property enables theme authors to "auto-install" one or more extension bundles that are required
for the theme's feature set.
For simple themes this collection may be empty.
You can use any existing 3rd-party bundle or create your own.

For example the default Helium theme uses this option to install a few custom renderers 
that deal with UI aspects not covered by Laika Core, like decorating callouts with one of Helium's icons. 

Bundles are not covered in this chapter as this extension point is documented in [The ExtensionBundle API].

A bundle that is automatically installed as part of the theme should be essential for the theme to function.
If it is opt-in, your library should make it available separately so that the user can mix and match. 

Finally, the `treeProcessor` property is another optional hook that allows to inspect and modify the document AST
between parsing and rendering.
This can be useful if some of your theme's configuration logic needs to look at the input documents or configuration
supplied by the user before generating the final AST.
It is a partial function that expects the output format, so you can create separate logic for producing the AST for the
site, EPUB and PDF output.
If this hook is not needed for your theme, simply define `PartialFunction.empty` here.


Designing a Configuration API
-----------------------------

Every theme should have a configuration API that allows to tweak the look and feel or add metadata or custom links.
It's usually the only public API of your library to reduce the likeliness of issues with binary compatibility.

You can have a look at Helium's API documented in [Theme Settings] for inspiration.
Such an API usually covers some or all of the following aspects:

* Configure font resources for embedding
* Tweak colors, font sizes, line height and layout of the pages
* Define metadata like language, authors, description
* Configure special pages like table of contents, download or landing pages
* Add custom links to navigation bars
* Specify cover images for EPUB and PDF documents

The package `laika.theme` contains a few base types that you can reuse for defining some common types:

* The `Color` type gives you the `hex` and `rgb` methods for defining colors.
* `LengthUnit` offers constructors for all length units permitted in CSS, e.g. `px(12)` or `em(1.5)`.
* The `FontDefinition` type allows to define fonts for embedding. 
  Adding these instances to the final base configuration will trigger the embedding in EPUB and PDF documents
  without any further implementation within your theme.
  
Another aspect users might appreciate is if you allow to define most options separately per output format.
Users might pick a more colorful design for the site for example, but switch to a more black-and-white feel
for the PDF output so that the content looks good when printed.

The Helium API solves this by requiring a selector in front of all configuration methods which is either
`all`, `site`, `epub` or `pdf`:

```scala
val theme = Helium.defaults
  .all.metadata(
    title = Some("Project Name"),
    language = Some("de"),
  )
  .epub.navigationDepth(4)
  .pdf.navigationDepth(4)
  .build
```


Generating or Loading Documents
-------------------------------

The one aspect probably all theme implementations are dealing with is to pre-populate the input tree with 
default templates and styles.
For this you can use the same `InputTree` API that users can use if they need full flexibility for defining
their inputs.

@:callout(warning)

While Laika offers the full API as for user inputs for maximum flexibility and consistency, 
it is recommended to avoid any file system resources by using `addFile` or `addDirectory`.

This might be acceptable for a library shared in-house where you can rely on a specific project setup,
but for a public library it is most convenient for users when the theme is just an additional dependency
and does not require any additional setup.

@:@


### Templates

Each theme supporting the three major output formats (site, EPUB, PDF) would come with at least three template
documents: one default template for each format.
Additionally a theme can provide opt-in templates that users can explicitly select in the configuration header
of a markup document with (`laika.template = /<theme-dir>/<name>-template.html)`), where `theme-dir` is the
directory your theme files are generated into.

There are two approaches you can choose from:

* Generate the entire template as an in-memory string based on the user's theme configuration and 
  add the result to the `InputTreeBuilder`:
  
  ```scala
  val builder: InputTreeBuilder[F] = ???
  val templateString: String = MyTemplateGenerator.generate(config)
  builder.addString(templateString, DefaultTemplatePath.forHTML)
  ```
  
  This might be quite convenient to implement, but has the disadvantage that the user does not have a way
  to look at the default template in case she wants to place a modified copy into the input tree.
  
* Or place the entire default template into the resource folder of your library and load it from there:

  ```scala
  val builder: InputTreeBuilder[F] = ???
  val resourcePath = "my-theme/templates/default.template.html"
  builder.addClasspathResource(resourcePath, DefaultTemplatePath.forHTML)
  ```
  
  In this case you would use Laika's template syntax to access your theme's configuration, 
  usually mostly by using the following features:
  
    * HOCON substitution references in the form of `${myTheme.site.fontFamilies}`.
      For details see [Generating Configuration for Templates] below.
      
    * Laika directives, `@:for` to loop over content placed into the configuration and `@:if` for
      conditionally including content.
      
  In this approach your documentation can point to the source of the template in your repository,
  making it easier for the user to use a modified copy if required.
  
  For details on the supported template syntax, see [Creating Templates].


### Generating Configuration for Templates

When using the second approach described in the section above where you use directives and substitution references
inside your templates, the base configuration for the transformer needs to be pre-populated with all the corresponding
values.

This can be achieved using Laika's `ConfigBuilder` API which allows to programmatically construct `Config` instances,
which are normally obtained by parsing HOCON.
The builder accepts all the types supported in JSON/HOCON, e.g. Strings, Numbers, Arrays and Objects,
but also, as a Laika extension, the inclusion of AST nodes.

Using AST nodes has the advantage that you do not have to pre-render the output for all formats.
If it is a node type supported by Laika Core it is already known to all renderers, 
allowing you to reduce the boilerplate and stringly logic of rendering the format directly.

The below example shows how the `ThemeBuilder` API can be used to pre-populate the transformer configuration:

```scala
val logo = ThemeLogo(ThemeTarget.internal(Root / "logo.png"), alt = Some("Project Logo"))

val baseConfig = ConfigBuilder.empty
  .withValue("theme-name.logo", logo)
  .build

ThemeBuilder("Theme Name")
  .addBaseConfig(baseConfig)
  .build
```

It defines a logo AST element, based on the virtual path `Root / "logo.png"` and associates it with the key
`theme-name.logo`.
Finally it passes the configuration to the theme builder, making the logo available for templates via a
substitution reference (`${theme-name.logo}`).

The indirection via the configuration key means that even if the user customizes the default templates
of the theme you created, these references can still be used by the end user.

Of course the above example is a minimal excerpt of a typical theme builder, which would normally add more
keys to the configuration and also use the theme builder to pre-populate templates and styles.


### CSS

The second content type you would most likely include is CSS generated based on user configuration.
Here the most convenient approach might be to place static CSS files into the resource folder of your library,
and use CSS variables to capture all aspects which the user can configure. 
This is the approach that Helium has also chosen.

```scala
val builder: InputTreeBuilder[F] = ???
val resourcePath = "my-theme/css/theme.css"
val vars: String = MyCSSVarsGenerator.generate(config)
builder
  .addString(vars, Root / "my-theme" / "vars.css")
  .addClasspathResource(resourcePath, Root / "my-theme" / "theme.css")
```


### Fonts

If a theme supports EPUB or PDF output, it would be convenient for the user if a theme includes a set of default fonts.
Otherwise PDF output would always be based on the few standard fonts that are available for every PDF generator,
which might be somewhat limiting stylistically.

Ensure that the fonts your are using have a license that allows for redistribution as part of a library.
Beware that some web fonts might allow linking the font in websites for free, but not embedding them into 
EPUB or PDF documents.
Ideally the license should also not require the users of the theme to add an attribution to each page.

When including font defaults for convenience, the theme's configuration API should always allow for their replacement.
The API should accept a sequence of `FontDefinition` instances that define the fonts to be embedded.

These definitions can then be passed to the base configuration of the theme (which will be merged with the 
user configuration):

```scala
val fonts: Seq[FontDefinition] = ???
val baseConfig = ConfigBuilder.empty
  .withValue("laika.epub.fonts", fonts)
  .withValue("laika.pdf.fonts", fonts)
  .build
  
ThemeBuilder("Theme Name")
  .addBaseConfig(baseConfig)
  .build
```

Of course, like with Laika's default Helium theme, you can allow to define different fonts for EPUB and PDF.

@:todo(add shortcut for adding configured fonts to `ThemeBuilder` API)


Constructing a Theme Instance
-----------------------------

Finally all the templates, styles, configuration and fonts that you gather from the user's theme configuration
or from your defaults if omitted, need to be assembled into a theme provider instance.

This step will be very different for each theme depending on its feature set, 
so we just show Laika's own Helium theme builder as an example:

```scala
private[helium] class HeliumThemeBuilder (helium: Helium) extends ThemeProvider {

  def build[F[_]: Sync: Runtime]: Resource[F, Theme[F]] = {

    import helium._

    val treeProcessor = new HeliumTreeProcessor[F](helium)
    val htmlOverrides = HeliumRenderOverrides
                          .forHTML(siteSettings.layout.anchorPlacement)

    ThemeBuilder("Helium")
      .addInputs(HeliumInputBuilder.build(helium))
      .addBaseConfig(ConfigGenerator.populateConfig(helium))
      .addRewriteRules(HeliumRewriteRules.build(helium))
      .addRenderOverrides(HTML.Overrides(htmlOverrides))
      .addRenderOverrides(XSLFO.Overrides(HeliumRenderOverrides.forPDF))
      .processTree(treeProcessor.forHTML, HTML)
      .processTree(treeProcessor.forAllFormats)
      .build

  }
}
```

As you can see, it is a very tiny class as it delegates to various other builders to properly separate concerns.
Let's just highlight the most important aspects:

* Your theme builder must implement `ThemeProvider` as that is ultimately the type that can be passed
  to the library's transformer API or to the `laikaTheme` sbt setting.
  An instance of this type is what the final call to `build` in your configuration API should produce.
  
* The `build` method will be invoked by Laika when the transformer resource is created.
  It passes in the effect type as it is in the hand of the end user to select the concrete implementation,
  any implementation of `cats.Sync` is supported.
  It also provides a `Runtime` instance, which is a Laika type that encapsulates user runtime configuration
  (the execution contexts to use, level of parallelism, etc.).
  It has an API that allows to call `runBlocking` or `runParallel` which will use the user provided contexts.
  
* The `ThemeBuilder` is an optional API, but usually recommended as it reduces boilerplate.
  The only requirement is that the `build` method returns a cats-effect `Resource[F, Theme[F]]`.
  In the case of Helium, you can see that the API is used to: 
  
    a) Pre-populate the input tree with documents (templates, styles, JavaScript) via `addInputs`,
       as described in [Generating or Loading Documents] above.
    
    b) Pre-populate the transformer configuration (e.g. with text or AST nodes that can be referenced in templates)
       via `addBaseConfig` as shown in [Generating Configuration for Templates] above.
       
    c) Customize the rendering of some AST nodes with theme-specific features via `addRenderOverrides`.
       In Helium this is used for rendering tabs for user-selectable content, anchors for copying section links,
       and for adding custom icons to callouts.
       See [Overriding Renderers] for details.
       
    d) Add AST rewrite rules via `addRewriteRules`, which is somewhat more low-level and allows to remove or swap
       AST nodes between parsing and rendering.
       See [AST Rewriting] for details on this topic.
       
    e) Finally, it adds or removes additional documents via `processTree` which allows for a final modification
       of the document tree per output format, e.g. to remove a JavaScript file that your theme from the EPUB
       output which is only intended for websites.


Publishing a Theme
------------------

Since a theme is just a dependency, you can publish it like any other library.

As a minimum set of documentation it's recommended to include:

* Listing all licenses, both the one you are using for the theme implementation as well as potentially
  3rd-party licenses for any font or icon resources you might include.
  
* Document the supported output formats.
  Ideally themes support Laika's main target formats, HTML, EPUB and PDF, 
  but if you want to focus on fewer or different formats, let your users know.
  
* Document the compatible Laika versions.
  Laika is still in the 0.x version range, so some breaking changes are still expected before reaching the 1.0 milestone.
  But the big rewrite phases are a thing of the past now, so you won't be exposed to massive disruptions.
  A 1.0 version is expected at some point after the 0.18 or 0.19 releases (we are at 0.16 right now).
  
* Document your theme configuration API.

Finally, let us know about your theme! We are happy to add links and short descriptions to Laika's documentation,
so that users know which 3rd-party alternatives exist.
