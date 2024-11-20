
Creating Themes
===============

If you want to create your own look & feel as an alternative to Laika's default theme called Helium 
there are several options depending on your concrete use case, 
some of which would not require to fully implement a theme yourself.


Common Use Cases
----------------

Here we will first describe the possible use cases,
which will help you determine which sections of the manual to work through
as some of these use cases are not actually covered by this chapter.


### Adjusting Colors, Fonts and Layout

This is the most minimal level of adjustment and can be done with the Helium configuration API.
For an impression of how far you can get with this lightweight approach,
you can check the following documentation sites which all come with their own color scheme for Helium:

* [**http4s**](https://http4s.org/)
* [**Parsley**](https://j-mie6.github.io/parsley/)
* [**cats**](https://typelevel.org/cats/)

The advantage of this approach is that you do not have to re-invent the wheel and can still benefit
from some of Helium's more advanced features (e.g. versioning or info boxes with tabs) without the huge
effort of creating those features from scratch.
The downside is that you are somewhat limited in how far you can deviate from the overall look & feel of Helium.

If you choose this approach you can skip the remainder of this chapter and read through [Theme Settings] instead.


### Customizing Theme Templates

This goes one step further than the previous use case, but would still rely on an existing theme.
Your freedom is greatly increased by the option of reassembling the features of a theme in a different layout.
This can be done by placing template files into a specific directory of your inputs.
The names and locations are dependent on the theme, for Helium they are documented in [Customizing Template Fragments].

Helium's templating is modular, meaning you can, for example, easily replace the template for the left navigation bar 
without affecting anything else. 
This is relevant as overwriting a template means you need to regularly merge up changes from the original whenever 
Helium has a new release (unless your alternative template does not bear any resemblance to the original).
Only replacing the parts of the layout you actually want to adjust minimizes this work.
The need for regular merges is also the biggest downside of this approach. But, in the case of Helium,
the templates have stabilized with the 1.0 release in a way that frequent changes are very unlikely.

Like with the previous use case you can ignore the rest of this chapter when going down this route.


### Creating Theme Extensions

This is similar to the previous two use cases as it relies on an existing theme, 
but instead of just loosely assembling functionality in your build and input directories,
you can bundle them up as a theme extension for reuse.

There are two slightly different approaches, depending on whether your extension is specific to Helium
or would work with any other existing theme, too.

**Extensions specific to Helium**

If it is specific to Helium, you can simply implement it as a function of type `Helium => Helium`:

```scala mdoc:silent
import laika.helium.Helium

object MyHeliumBundle extends (Helium => Helium) {
  def apply(helium: Helium) : Helium = {
     helium // your configuration code here
  }
}
```

Such an object can then be applied by users in their own Helium configuration:

```scala mdoc:silent
Helium.defaults.extendWith(MyHeliumBundle).build
```

For this approach the relevant documentation can be found in [Theme Settings], 
and the rest of this chapter can be skipped.

**Generic theme extensions**

If you do not need to rely on Helium functionality, it's better to create a generic theme extension
by implementing `ThemeProvider`. 
This is the same API that you'd implement for creating a new theme from scratch, 
the only difference is what you put inside.
In the case of an extension it would only be a subset of a typical theme feature set,
expanding or overwriting functionality of the host theme.

For details on how to implement this API, see [Implementing Themes] below.

An extension can be applied to a host theme like this (note that both are of the same type):

```scala mdoc:invisible
import laika.sbt.LaikaPlugin.autoImport._
import sbt._
```

```scala mdoc:compile-only
import laika.theme.ThemeProvider

def hostTheme: ThemeProvider = ???
def themeExtension: ThemeProvider = ???

laikaTheme := hostTheme.extendWith(themeExtension)
```

Some real world code examples for theme extensions:

* [protosearch]: this extension adds several JavaScript files to the generated output which provide search functionality for Laika.

* [sbt-typelevel]: this extension adds some existing bundles to Laika and a redirect for the `/api` URL.


[protosearch]: https://github.com/cozydev-pink/protosearch/blob/main/laikaIO/src/main/scala/pink/cozydev/protosearch/ui/SearchUI.scala#L24
[sbt-typelevel]: https://github.com/typelevel/sbt-typelevel/blob/main/site/src/main/scala/org/typelevel/sbt/site/GenericSiteSettings.scala#L53


### A Single Project without Helium

This is the first use case that completely bypasses Helium or any other existing theme and implements
the look & feel completely from scratch.
If this is for a single project only, and you do not plan to reuse the theme or publish it for other users,
then implementing it as a theme is entirely optional and more of a stylistic choice.

If you want to avoid any optional steps, the quickest approach is to just install an empty theme
and simply place all necessary templates and CSS files in your regular input directory.

@:select(config)

@:choice(sbt)
```scala mdoc:invisible
import laika.sbt.LaikaPlugin.autoImport._
import sbt.Keys._
import sbt._
```

```scala mdoc:compile-only
import laika.theme.Theme

laikaTheme := Theme.empty
```

@:choice(library)

```scala mdoc:compile-only
import cats.effect.IO
import laika.api._
import laika.format._
import laika.io.syntax._
import laika.theme.Theme

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .parallel[IO]
  .withTheme(Theme.empty)
  .build
```

@:@

If you choose this approach you can skip the remainder of this chapter and read through [Creating Templates] instead.


### Creating Reusable Themes

The effort of designing and implementing a new theme from scratch is justified if you plan to re-use it across multiple projects,
either as an in-house library or published as Open Source for the entire community.

For this approach it is recommended to work through the remaining sections of this chapter.

The need for the creation of a configuration API depends on whether this is a personal or in-house type of 
use case where configurability can be constrained to the differences between your particular use cases,
or whether you plan to publish the theme for the community.
In the latter case, it's best to also work through the section [Designing a Configuration API] to give
sufficient flexibility to your end users like the Helium API does.


Theme Functionality
-------------------

While it is obviously up to the respective theme author to decide on the feature set of their theme,
there are nevertheless a few implementation details that every theme must include:

* **Implementing a `ThemeProvider`**: This is the only public type your published theme JAR
   must provide, as this is the type that users will pass to a Laika transformer or
   configure as a setting in the sbt plugin.
   It will bundle all the functionality listed in the following bullets into a single type.
   See [Implementing Themes] below for the steps needed to implement this API.

* **Templates**: a theme needs to include at least one template for rendering site output.
    The `laika-core` module itself does not come with any templates, 
    since producing complete HTML documents is the job of themes.
    See [Templates] below for how to include templates in a theme either as classpath
    resources or generated in-memory.

* **CSS**: Like with templates, `laika-core` does not contain any style sheets itself, 
    so you need to include at least one style sheet for each supported output format.
    See [CSS] below for details.

* **Font and Icon Resources**: If you only support HTML output you can avoid embedding
    any font or icon resources into your JAR, as you can use web fonts and simply
    link them in the `<head>` section of your templates.
    However, if you also support EPUB and PDF output, embedding fonts and icons becomes a necessity.
    See [Fonts] below for details.

**Optional Theme Features**

In addition to the mandatory functionality listed above, themes can support any other
functionality that is impossible to support within `laika-core`, such as features
relying on JavaScript resources which need be referenced from custom templating solutions.

A list of examples for such functionality living in themes is the support for site search provided
by the [protosearch](https://github.com/cozydev-pink/protosearch) library or Helium
features like [Versioned Documentation], [Mermaid Diagrams] or the [`@:select`][select-dir] directive
for tabbed info boxes.

[select-dir]: /07-reference/01-standard-directives.md#select


Implementing Themes
-------------------

If you've reached this part of the chapter it's safe to assume that your use case is either
the 3rd or last of those listed under [Common Use Cases] which are the only two scenarios
where implementing a `ThemeProvider` is required.

This section attempts to guide you through all steps of the process.


### Creating a `ThemeProvider`

`ThemeProvider` is a simple trait with a single method and the only public member
a minimal theme needs to provide as that is ultimately the type that can be passed
to the library's transformer API or to the `laikaTheme` sbt setting:

```scala mdoc:silent
import cats.effect.{ Async, Resource }
import laika.theme.{ Theme, ThemeProvider }

object MyTheme extends ThemeProvider {

  def build[F[_]: Async]: Resource[F, Theme[F]] = ???
  
}
```

For the implementation you can construct a `Theme` instance in any way, 
but the most convenient way is usually to use [The `ThemeBuilder` API].
See that section for details and example implementations.


### Using a Custom Theme

The theme implementation shown above can then be registered with Laika,
essentially replacing the built-in Helium theme which would otherwise be chosen by default:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
laikaTheme := MyTheme
```

@:choice(library)

```scala mdoc:compile-only
import cats.effect.IO
import laika.api._
import laika.format._
import laika.io.syntax._

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .parallel[IO]
  .withTheme(MyTheme)
  .build
```

@:@


### The `ThemeBuilder` API

This is a convenient API that takes away some of the boilerplate of implementing a `Theme` instance manually.

It helps to assemble all the templates, styles, configuration and fonts that you gather 
from the user's theme configuration or from your defaults if omitted.

**A minimal example**

The example below shows a very minimal theme that only supports HTML and only includes
a single template document, a single CSS file and the name of the theme:

```scala
import cats.effect.{ Async, Resource }
import laika.ast.laika.ast.DefaultTemplatePath
import laika.io.model.InputTree
import laika.theme.{ Theme, ThemeBuilder, ThemeProvider }

object MinimalTheme extends ThemeProvider {

  def build[F[_]: Async]: Resource[F, Theme[F]] = {

    val inputs = InputTree[F]
      .addClassLoaderResource(
        "minimalTheme/default.template.html",
        DefaultTemplatePath.forHTML
      )
      .addClassLoaderResource(
        "minimalTheme/theme.css",
        Path.Root / "minimal" / "theme.css",
      )

    ThemeBuilder("MinimalTheme")
      .addInputs(inputs)
      .build
  }
}
```

First we assemble the only two inputs (the template and the stylesheet) as classpath resources.
Alternatively you can generate these in-memory, but you should avoid the use of any file system
resources, so that your theme can be a simple build dependency that does not require any additional installations.

The first argument to `addClassLoaderResource` is the path to the resource within the jar, 
the second is the virtual path within the Laika document tree where the document will be placed.
The first path (`DefaultTemplatePath.forHTML`) is a location for the default HTML template that
will be used for all markup documents that do not specify an alternate template in their configuration.
The virtual path for the CSS file can be anything as long as it is referenced properly from within the template.

Then we instantiate a `ThemeBuilder`, passing the name of the theme as an argument.
The name is only used for logging purposes, like when using the `laikaDescribe` task of the sbt plugin
to inspect your configuration.

Finally, we pass the inputs and call `build` to create a theme resource.

@:callout(warning)

While Laika offers the full API as for user inputs for maximum flexibility and consistency,
it is recommended to avoid using `addFile` or `addDirectory` to add any file system resources.

This might be acceptable for a library shared in-house where you can rely on a specific project setup,
but for a public library it is most convenient for users when the theme is just an additional dependency
and does not require any additional setup.

For this reason the best option is usually to either generate resources in memory and/or load them
from the JAR, both of which is directly supported by the APIs.

@:@

**Other `ThemeBuilder` methods**

If your requirements are less minimal than in our example, 
there are additional methods in `ThemeBuilder` beyond `addInputs` with which you can:

* Pre-populate the transformer configuration (e.g. with text or AST nodes that can be referenced in templates)
  via `addBaseConfig` as shown in [Generating Variables for Templates] below.

* Customize the rendering of some AST nodes with theme-specific features via `addRenderOverrides`.
  In Helium this is used for rendering tabs for user-selectable content, anchors for copying section links,
  and for adding custom icons to callouts.
  See [Overriding Renderers] for details.

* Add AST rewrite rules via `addRewriteRules`, which is somewhat more low-level and allows to remove or replace
  AST nodes between parsing and rendering.
  See [AST Rewriting] for details on this topic.

* Add any other type of extension via `addExtensions`, which accepts a list of `ExtensionBundle` implementations.
  These can be any 3rd party bundle or one provided by the theme itself.
  
  A bundle that is automatically installed as part of the theme should be essential for the theme to function.
  If it is opt-in, your library should make it available separately so that the user can mix and match.

  See [The ExtensionBundle API] for details on all available extension hooks.

* Finally, you can use the `processTree` method to inspect and modify the document AST
  between parsing and rendering in an effectful way.


### Templates

Each theme supporting the three major output formats (site, EPUB, PDF) would come with at least three template
documents: one default template for each format.
Additionally, a theme can provide opt-in templates that users can explicitly select in the configuration header
of a markup document with (`laika.template = /<theme-dir>/<name>-template.html)`), where `theme-dir` is the
directory your theme files are generated into.

There are two approaches you can choose from:

* Generate the entire template as an in-memory string based on the user's theme configuration and 
  add the result to the `InputTreeBuilder`:
  
    ```scala
    import laika.io.model.InputTreeBuilder
    import laika.rewrite.DefaultTemplatePath
  
    val builder: InputTreeBuilder[F] = ???
    val templateString: String = MyTemplateGenerator.generate(config)
    builder.addString(templateString, DefaultTemplatePath.forHTML)
    ```
  
  This might be quite convenient to implement, but has the disadvantage that the user does not have a way
  to look at the default template in case she wants to place a modified copy into the input tree.
  
* Or place the entire default template into the resource folder of your library and load it from there:

    ```scala
    import laika.io.model.InputTreeBuilder
    import laika.rewrite.DefaultTemplatePath
  
    val builder: InputTreeBuilder[F] = ???
    val resourcePath = "my-theme/templates/default.template.html"
    builder.addClassResource(resourcePath, DefaultTemplatePath.forHTML)
    ```
  
  In this case you would use Laika's template syntax to access your theme's configuration, 
  usually mostly by using the following features:
  
    * HOCON substitution references in the form of `${myTheme.site.fontFamilies}`.
      For details see [Generating Variables for Templates] below.
      
    * Laika directives, `@:for` to loop over content placed into the configuration and `@:if` for
      conditionally including content.
      
  In this approach your documentation can point to the source of the template in your repository,
  making it easier for the user to use a modified copy if required.
  
  For details on the supported template syntax, see [Creating Templates].


### Generating Variables for Templates

When using the second approach described in the section above where you use directives and substitution references
inside your templates, the base configuration for the transformer needs to be pre-populated with all the corresponding
values.
This approach allows to transfer user configuration from your theme's Scala configuration APIs to substitution variables
which can be referenced from within your theme's templates.
The indirection shields the end user from the need to configure your theme with stringly HOCON files.

This can be achieved using Laika's `ConfigBuilder` API which allows to programmatically construct `Config` instances,
which are normally obtained by parsing HOCON.
The builder accepts all the types supported in JSON/HOCON, e.g. Strings, Numbers, Arrays and Objects,
but also, as a Laika extension, the inclusion of AST nodes.

Using AST nodes has the advantage that you do not have to pre-render the output for all formats.
If it is a node type supported by Laika Core it is already known to all renderers, 
allowing you to reduce the boilerplate and stringly logic of rendering the format directly.

The below example shows how the `ThemeBuilder` API can be used to pre-populate the transformer configuration:

```scala mdoc:silent
import cats.effect.IO
import laika.api.config.ConfigBuilder
import laika.ast.Image
import laika.ast.Path.Root
import laika.theme.ThemeBuilder

val logo = Image.internal(
  path = Root / "logo.png", 
  alt = Some("Project Logo")
)

val baseConfig = ConfigBuilder.empty
  .withValue("theme-name.logo", logo)
  .build

ThemeBuilder[IO]("Theme Name")
  .addBaseConfig(baseConfig)
  .build
```

It defines a logo AST element, based on the virtual path `Root / "logo.png"` and associates it with the key
`theme-name.logo`.
Finally, it passes the configuration to the theme builder, making the logo available for templates via a
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

```scala mdoc:compile-only
import cats.effect.IO
import laika.ast.Path.Root
import laika.io.model.InputTreeBuilder

def builder: InputTreeBuilder[IO] = ???
val resourcePath = "my-theme/css/theme.css"
val vars: String = "<... generated-CSS ...>"
builder
  .addString(vars, Root / "my-theme" / "vars.css")
  .addClassResource(resourcePath, Root / "my-theme" / "theme.css")
```


### Fonts

If a theme supports EPUB or PDF output, it would be convenient for the user if a theme includes a set of default fonts.
Otherwise, PDF output would always be based on the few standard fonts that are available for every PDF generator,
which might be somewhat limiting stylistically.

Ensure that the fonts you are using have a license that allows for redistribution as part of a library.
Beware that some web fonts might allow linking the font in websites for free, but not embedding them into 
EPUB or PDF documents.
Ideally the license should also not require the users of the theme to add an attribution to each page.

When including font defaults for convenience, the theme's configuration API should always allow for their replacement.
The API should accept a sequence of `FontDefinition` instances that define the fonts to be embedded.

These definitions can then be passed to the base configuration of the theme (which will be merged with the 
user configuration):

```scala mdoc:compile-only
import cats.effect.IO
import laika.api.config.ConfigBuilder
import laika.theme.ThemeBuilder
import laika.theme.config.FontDefinition

def fonts: Seq[FontDefinition] = ???
val baseConfig = ConfigBuilder.empty
  .withValue("laika.epub.fonts", fonts)
  .withValue("laika.pdf.fonts", fonts)
  .build
  
ThemeBuilder[IO]("Theme Name")
  .addBaseConfig(baseConfig)
  .build
```

Of course, like with Laika's default Helium theme, you can allow to define different fonts for EPUB and PDF.


### Designing a Configuration API

Every theme that is not for internal use should come with a configuration API that allows to tweak the look and feel or add metadata or custom links.
It's usually the only public API of your library to reduce the likeliness of issues with binary compatibility.

You can have a look at Helium's API documented in [Theme Settings] for inspiration.
Such an API usually covers some or all of the following aspects:

* Configure font resources for embedding
* Tweak colors, font sizes, line height and layout of the pages
* Define metadata like language, authors, description
* Configure special pages like table of contents, download or landing pages
* Add custom links to navigation bars
* Specify cover images for EPUB and PDF documents
* Let the user define which JavaScript and CSS resources should be auto-linked in HTML headers.

The package `laika.theme` contains a few base types that you can reuse for defining some common types:

* The `Color` type gives you the `hex` and `rgb` methods for defining colors.
* `LengthUnit` offers constructors for all length units permitted in CSS, e.g. `px(12)` or `em(1.5)`.
* The `FontDefinition` type allows to define fonts for embedding.
  Adding these instances to the final base configuration will trigger the embedding in EPUB and PDF documents
  without any further implementation within your theme.
* The `IncludeDirective` type allows to construct instances of the `@:includeCSS` and `@:includeJS`
  directives to be used within the theme's templates, allowing the users to define where CSS and JS
  resources for auto-linking into the generated HTML output can be found.

Another aspect users might appreciate is if you allow to define most options separately per output format.
Users might pick a more colorful design for the site for example, but switch to a more black-and-white feel
for the PDF output so that the content looks good when printed.

The Helium API solves this by requiring a selector in front of all configuration methods which is either
`all`, `site`, `epub` or `pdf`:

```scala mdoc:silent
import laika.helium.Helium

val theme = Helium.defaults
  .all.metadata(
    title = Some("Project Name"),
    language = Some("de"),
  )
  .epub.navigationDepth(4)
  .pdf.navigationDepth(4)
  .build
```


Publishing Themes
-----------------

Since a theme is just a dependency, you can publish it like any other library.

As a minimum set of documentation it's recommended to include:

* Listing all licenses, both the one you are using for the theme implementation as well as potentially
  3rd-party licenses for any font or icon resources you might include.
  
* Document the supported output formats.
  Ideally themes support Laika's main target formats, HTML, EPUB and PDF, 
  but if you want to focus on fewer or different formats, let your users know.
  
* Document the compatible Laika versions.
  It will most likely be compatible with all 1.x versions, but there may be exceptions when you build on a feature
  introduced in a minor 1.x release.
  
* Document your theme configuration API.

Finally, let us know about your theme! We are happy to add links and short descriptions to Laika's documentation,
so that users know which 3rd-party alternatives exist.
