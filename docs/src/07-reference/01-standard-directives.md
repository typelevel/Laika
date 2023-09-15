
Standard Directives
===================

Laika comes with a small number of built-in directives intended to cover the most common scenarios.

Some of them can only be used in templates, whereas others can also be used in text markup, 
as stated for each individual directive below.

For details on creating your own directives, see [Implementing Directives].


Navigation
----------

This set of directives helps with building complex navigation structures or serve as shortcuts for creating links.

The directives in this section are all documented in detail in the [Navigation] chapter and only listed
here for the sake of completeness.


### `@:navigationTree`

Can be used in templates and as block elements in text markup.

This flexible directive allows to freely combine auto-generated navigation trees based on your input tree's
structure with manual entries.

It is documented in detail in [Generating Navigation Trees] in the Navigation chapter.


### `@:breadcrumb`

Can be used in templates and as block elements in text markup.

This standard breadcrumb component generates a simple, flat list of links reflecting the directory structure
from the root to the current document. 
See [Breadcrumbs] in the Navigation chapter for details.


### `@:api`

Can be used as inline element and within links in text markup.

Serves as a shortcut for creating links to API documentation, e.g. `@:api(laika.api.Transformer)`.

See [Linking to API Documentation] for details.


### `@:source`

Can be used as inline element and within links in text markup.

Serves as a shortcut for creating links to the source code of types, e.g. `@:source(laika.api.Transformer)`.

See [Linking to Source Code] for details.


### `@:target`

Can only be used in templates.

Renders a link target which is specified as the only (required) attribute of this directive.

The attribute can either be a literal target or the key of a config value of type string.

External targets will be rendered unmodified, internal targets will be interpreted as relative to the template,
validated and then translated. 
For every document the template is applied to it is resolved as relative to that rendered document.

Example:
```laika-html
<link rel="icon" href="@:target(../styles/manual.css)" />
```

See [Disabling Validation] when you only want the path translation without the validation.


Inclusions
----------

Laika offers the option to include one markup document inside another, as well as including a template inside another.

There are two directives for this purpose: `@:include` is a simple inclusion that only allows to pass HOCON attributes
to the included document, and `@:embed` with which a parsed directive body can be passed to the other document.

Keep in mind that all references in Laika are based on the [Virtual Tree Abstraction]. 
This means that you cannot specify plain file system paths with these directives, but instead absolute or relative
links pointing to a resource of your input tree.
For this reason all included documents must be part of the known input tree that you specify when starting
a transformation.
They can stem from any of the supported input types, files, streams or content generated in-memory.


### `@:include`

Can be used as a block-level directive in markup documents and as a template directive.

The main attribute can either be a literal, relative or absolute path or the key of a config value of type string.
Like all internal path references it represents a virtual path within the transformation inputs specified
by the user and does not have to match a physical file path.

The following example shows a simple HTML template where a header section is added via an include directive:

```laika-html
<html>
  @:include(../inc/head.template.html) { css = "../css/nav.css" }
  <body>
    ${cursor.currentDocument.content}
  </body>
</html>
```

The optional, named attributes specified for the directive, in the example just `css` can be referenced in the included template
with Laika's common `_` prefix for directive scopes:

```laika-html
<head>
  <title>${cursor.currentDocument.title}</title>
  <link rel="stylesheet" href="${_.css}">
</head>
```

As you can see in the example the included template has access to the `cursor` of the including document to obtain
the title, as well as access to the special scope prefix `_` which holds all the attributes passed directly in the directive.

The attributes are standard HOCON, meaning they can have namespace prefixes and can refer to other variables defined
in higher scopes.


### `@:embed`

Can be used as a block-level directive in markup documents and as a template directive.

This directive is very similar to the `include` directive, but also allows to pass a parsed body element to
the included template or document.

We can modify the example to pass a few tags to the included template instead of just a single attribute:

```laika-html
<html>
  @:embed(../inc/head.template.html) 
    <link rel="stylesheet" href="nav.css">
    <link rel="stylesheet" href="main.css">
  @:@
  <body>
    ${cursor.currentDocument.content}
  </body>
</html>
```

The body of the directive in our modified example:

```laika-html
<head>
  <title>${cursor.currentDocument.title}</title>
  ${_.embeddedBody}
</head>
```


Styles & Themes
---------------

Laika already produces a small set of class attributes in the rendered output, 
e.g. `nav` and `level1` for a navigation element on the first level.
But you can alternatively assign style hints to markup explicitly.
   

### `@:style`

Can be used in block or span elements in text markup.

Adds a style to the root element of the body of the directive, 
that can be used by renderers to change the appearance of the rendered element.

For HTML and EPUB output it will get rendered as a `class` attribute that can be used in CSS declarations.
For PDF it can also be used in Laika's [CSS for PDF] support, even though there is no interim HTML output.

Block directive: 

```laika-md
@:style(subtitle) 

This paragraph gets the 
subtitle style applied.

@:@
  
While this paragraph does not.
```

Span directive:

```laika-md
Burrito - @:style(price) £3.50 @:@.
```


### `@:icon`

Can be used in templates or span elements in text markup.

Allows to reference an icon by key that had been registered in global configuration:

```laika-html
<li>@icon(close)</li>
```

The available icon set can be registered as part of the transformer setup:

@:select(config)

@:choice(sbt)
```scala mdoc:invisible
import laika.sbt.LaikaPlugin.autoImport._
```
```scala mdoc:compile-only
import laika.ast._
import laika.config.IconRegistry

laikaConfig := LaikaConfig.defaults
  .withConfigValue(IconRegistry("open" -> IconStyle("open"), "close" -> IconGlyph('\ueedd')))
```

@:choice(library)
```scala mdoc:compile-only
import laika.ast._
import laika.api._
import laika.config.IconRegistry
import laika.format._
import laika.markdown.github.GitHubFlavor

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(IconRegistry("open" -> IconStyle("open"), "close" -> IconGlyph('\ueedd')))
  .build
```
@:@

There are four available icon types:

* `IconStyle`: renders a class attribute in HTML output, so that it can be selected in CSS.
  This mechanism is used for different styles of icons: font icons that often come with pre-built CSS declarations or
  image sprite or SVG sprite icons defined via CSS.
  This icon type does not work for PDF output.
  
* `IconGlyph`: can be used for font icons only. Hard-codes the glyph in the AST, but has the advantage that it does
  work for PDF output.
  
* `InlineSVGIcon`: renders an SVG icon inline. While less space-efficient than SVG symbol references, this is currently
  the only kind of SVG icon that works in PDF, too.
  
* `SVGSymbolIcon`: renders a reference to an SVG icon defined elsewhere.
  Can be a local reference to an SVG block in the current page or point to an external file.
  This icon type does not work for PDF output.



Markup Blocks
-------------

The directives in this section can only be used in place of block-level elements in text markup.


### `@:image`

Can be used in block or span elements in text markup.

The image directive is an alternative to the native syntax for including images that supports additional attributes.

```laika-md
@:image(logo.png) {
  intrinsicWidth = 64
  intrinsicHeight = 64
  style = logo
  alt = Project Logo
  title = Tooltip Text
}
```

Note that the path, like everything in Laika, is interpreted to be within the virtual path 
of the input tree you configured.
It can be relative or absolute, in the latter case it would start with a `/` and 
See [Virtual Tree Abstraction] for details.

All other attributes shown in the example are optional:

* `intrinsicWidth` and `intrinsicHeight` can be used to avoid "layout shift" by telling the browser the size
  of the image before it is loaded.
  As the attribute names imply, they are not intended to be used for the actual display size.
  For this purpose you should use the `style` attribute and provide corresponding CSS for sizing the image.
  
* `alt` and `title` are pass-through attributes for the generated image tag.
  They are both ignored for PDF output.


### `@:callout`

Can only be used in block elements in text markup.

A callout is a decorated block element that stands out from the surrounding paragraphs.

In markup the directive simply surrounds the content of the callout:

```laika-md
@:callout(warning)

This text appears inside the decorated callout box.
Any block level elements can be used as the content, 
including lists, tables or code blocks.

@:@
```

When using the Helium theme it will be rendered as shown in the example below.

@:callout(warning)

This text appears inside the decorated callout box.
Any block level elements can be used as the content, 
including lists, tables or code blocks.

@:@

Helium comes with pre-defined styles and icons for the attributes `info`, `warning` and `error`.


### `@:select`

The select directive is a special directive that allows to create alternative versions of the same documentation,
for example one with Scala code examples and one with Java. 
Or as in the case of Laika's own documentation, one showing configuration setup with sbt and the other 
through the library API.

When using the default Helium theme these alternatives will be rendered as tabs in the website:

@:image(../img/selection-tabs.png) {
  alt = Tabs for User Selections
  style = medium-image
  intrinsicWidth = 500
  intrinsicHeight = 106
}

While for EPUB and PDF output it triggers the generation of separate books for each of the alternatives
and offers them on the download page:

@:image(../img/e-book-downloads.png) {
  alt = Separate Downloads for E-Books per Selection
  style = medium-image
  intrinsicWidth = 447
  intrinsicHeight = 300
}

This accounts for the fact that tabs would not make much sense when printed on paper!

Before using this directive you need to put a bit of configuration in place.
Laika expects the labels to use for the alternatives to be declared centrally, 
so that they do not need to be repeated with each use of the directive.

This is how the settings for Laika's manual look:

@:select(config)

@:choice(sbt)

```scala mdoc:compile-only
import laika.config.{ ChoiceConfig, SelectionConfig, Selections }

laikaConfig := LaikaConfig.defaults
  .withConfigValue(Selections(
    SelectionConfig("config",
      ChoiceConfig("sbt", "sbt Plugin"),
      ChoiceConfig("library", "Library API")
    ).withSeparateEbooks
  ))
```

@:choice(library)

```scala mdoc:compile-only
import laika.api._
import laika.config.{ ChoiceConfig, SelectionConfig, Selections }
import laika.format._
import laika.markdown.github.GitHubFlavor

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(Selections(
    SelectionConfig("config",
      ChoiceConfig("sbt", "sbt Plugin"),
      ChoiceConfig("library", "Library API")
    ).withSeparateEbooks
  ))
  .build
```

@:@

The configuration above uses three identifiers: `config` as the name of the group of alternatives,
and `sbt` and `library` as the name of the two choices together with their labels.

With this configuration in place the directive can be used like this:

````laika-md
@:select(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults.strict
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .strict
  .build
```

@:@

````

The `@:choice` directives are child directives called "separator directives" that can only be used inside
the corresponding parent directive to mark specific sections within the directive's body.


### `@:fragment`

Can only be used in block elements in text markup.

Produces a block element which is not part of the main body of the markup document.
Instead, it can be referred to by `${cursor.currentDocument.fragments.<fragmentName>}`.

This allows to keep some sections of your document separate, to be rendered
in different locations of the output, like headers, footers or sidebars.

This directive is explained in detail in the [Document Fragments] chapter.


### `@:format`

Can only be used in block elements in text markup.

The body of such a directive will only be included into the output by renderers for the specified type:

```laika-md
@:format(html)

This text only appears in the HTML output,
not in PDF, EPUB or any other format.

@:@
```


HTML Templates
--------------

These directives can only be used in HTML or EPUB templates, 
as they drive the inclusion of tags in the HTML `<head>` section or render content in HTML/XML syntax.

If you use the Helium theme and do not design your own templates or themes, 
you do not need to use these directives directly, as the Helium template already contain them.
In that case you can control which CSS and JS files to link via the theme's configuration
as described in [Auto-Linking CSS & JS Files].

### `@:attribute`

This directive can also be used in XSL-FO templates for PDF as it renders attribute syntax that is valid in both,
HTML and XML.

The primary use case for this directive is to render an attribute value from the configuration that may not be present:

```laika-html
<a @:attribute(src, myConf.myURL)/>
```

In the above example, the `src` attribute will only be rendered if the `myConf.myURL` key is defined in the 
configuration. 
Like all configuration values it can originate in global scope, in directory configuration or in a configuration 
header of a markup document.

When the value is not optional, it is recommended not to use this directive, 
as a missing value will not trigger any warnings or errors, but instead just cause the omission of the attribute.
Required attributes can be rendered with standard substitution syntax instead:

```laika-html
<a src="${myConf.myURL}"/>
```


### `@:date`

Renders a formatted, localized date.

```laika-html
Date published: @:date(laika.metadata.datePublished, ISO_LOCAL_DATE, en-US).
```

The directive has two required and one optional attribute:

* The first attribute is the key in the configuration that contains the date.
  The internal representation for the date is `java.time.OffsetDateTime` on the JVM and `js.Date` for Scala.js.
  Configuration values must provide dates in ISO-8601 extended offset date-time format 
  (e.g. `2012-01-01T12:30:00+03:00`).
  The time component can be omitted.

* The second attribute is the format to use for rendering the date.
  Supported values are one of these three options:

    1. (JVM only): A constant referring to a pattern provided by `java.time.format.DateTimeFormatter`
       (e.g. `ISO_LOCAL_DATE`). The full list of supported constants can be found here:
       <https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#predefined>.
     
    2. A constant for a localized style (either `medium` or `short`). 
       Scala.js additionally supports the `long` and `full` constants.

    3. (JVM only): A literal pattern (e.g. `yyyy-MM-dd'T'HH:mm:ss`) as documented here:
       <https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns>
  
* The final attribute is optional and specifies the locale to use (a IETF BCP 47 language tag such as `en-GB`).
  If not specified directly with the directive, the locale lookup happens in the following order:
   
    1. A `laika.metadata.language` attribute defined in the configuration header of the markup document.
    2. A `laika.metadata.language` attribute defined in the configuration file for the directory (or root directory).
    3. When using the Helium theme, a language specified with the [Helium Configuration API].
    4. The platform default.


Conditionals and Loops
----------------------

These directives are kept very simple on purpose. 
For example they do not include any expression language right now that would allow you to express conditions
like `@:if(catalogue.items > 5)`, but instead only check for booleans or non-empty values.
For more complex use cases it's often easier to implement your own directive 
instead of adding a lot of logic to your templates.  

### `@:for`

Can only be used inside templates.

Executes the body of the directive multiple times for each element of a non-empty array-type entry 
in the configuration.
It does not include the body at all for the values `null`, `false`, `None`, empty strings and empty collections,
and exactly once for all other values.

In case of non-empty values their properties will be made available for use inside the body part without any prefix:

```laika-html
@:for(catalog.products)

<li><a href="#${_.id}">${_.description}</a></li>

@:@ 
```

In this example `id` and `title` are properties of the `catalog.products` property that the user defined 
in the configuration. See [User-Defined Variables] for details on defining them.

You can also specify a fallback, a body part that gets executed for empty values:

```laika-html
@:for(catalog.products)
<li><a href="#${_.id}">${_.description}</a></li>
 
@:empty
<p>No products available.</p>

@:@
```


### `@:if`

Can only be used inside templates.

Executes the body of the directive exactly once for a Boolean `true` or the strings `true`, `on`, `yes` and `enabled`.

```laika-html
@:if(showSidebar)
<div class="sidebar">...</div>
@:@
```

In this example `showSidebar` is a custom configuration entry that the user defined 
in the configuration. See [User-Defined Variables] for details on defining them.

You can also specify a fallback with `@:else`, or even secondary conditions with `@:elseIf`:

```laika-html
@:if(showSidebar)
<div class="sidebar">...</div>

@:elseIf(showInfobox)
<div class="infobox">...</div>

@:else
<p>Something else</p>

@:@
```


PDF Output
----------

The following directive is only processed with PDF output and ignored with other formats.


### `@:pageBreak`

Can only be used in block elements in text markup.

The directive does not have any attributes or body elements and simply inserts a page break
into the final PDF document.

```laika-md
@:pageBreak
```


Comments
--------

Since Markdown does not have any native comment syntax, Laika offers a directive to add todo's to text markup:


### `@:todo`

Can be used in block and span elements in text markup.

```laika-md
@:todo(add diagram)
```

The directive will be ignored by all renderers and won't show up in any output documents.
