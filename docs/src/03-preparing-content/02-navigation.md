
Navigation
==========

```scala mdoc:invisible
import laika.sbt.LaikaPlugin.autoImport._
```

Laika's functionality for navigation can roughly be divided into four categories:

* Features added on top of the native link syntax of supported markup formats, 
  like validated [Validated Internal Links] or [Global Link Definitions].
  
* Link directives for individual links that provide non-standard shortcuts for convenience, 
  like [Linking by Section Headline] or [Linking to API Documentation].

* Directives that produce larger navigation structures, 
  like [Breadcrumbs] or [Generating Navigation Trees].

* If you want to create your own link shortcuts, you can create [Custom Link Directives].


Validated Internal Links
------------------------

Internal Links in Laika are validated, as long as they use the source document's name (e.g. `target.md`)
and not the suffix of the output document.
First this ensures that the links work in all target formats, including e-books. 
Secondly it also means that they work on GitHub, which is convenient, when your project is hosted there.

For regular, internal links you can use the text markup's native link syntax.
You can express an internal link either with a relative path:

```markdown
Please read the [Introduction](../basics/introduction.md).
```   

Or an absolute path from the root of the virtual input tree:

```markdown
Please read the [Introduction](/basics/introduction.md).
```

The paths do not have to be valid relative or absolute paths in the file system.
You can always work within the [Virtual Tree Abstraction]. 

It means that a relative path like `../images/foo.jpg` can point to an `images` directory that
has been merged into the input tree in the project's configuration, 
and does not exist relative to the source document in the file system it is linked from.

Likewise, absolute paths like `/images/foo.jpg` always refer to the virtual root of the input tree,
not to the root of your file system or your website.


### Disabling Validation

Transformations that contain internal links that cannot be resolved will fail in Laika's default run configuration.
If you work with input files that will be generated or copied by other tools and are not known to Laika, 
you can explicitly disable validation for certain paths within the virtual tree:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.ast.Path.Root
import laika.rewrite.link.LinkValidation

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LinkValidation.Global(excluded = Seq(Root / "generated")))
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.ast.Path.Root
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.rewrite.link.LinkValidation

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(LinkValidation.Global(excluded = Seq(Root / "generated")))
  .build
```
@:@

This disables validation for the specified directories and all their subdirectories.

In the unusual case you want to disable all validation you can alternatively use
`LinkValidation.Off` or `LinkValidation.Local`, where the latter will still validate all links
pointing to sections within the same document, but not those pointing elsewhere.


Global Link Definitions
-----------------------

Both supported markup formats support the use of link definitions in input files that can be referred to by
link references.

Example for Markdown:

```markdown
Please read the [Introduction][intro]. 
(Or in the short form, just [intro]).

[intro]: ../basics/introduction.md
```

Example for reStructuredText:

```rst
Please read the intro_.

.. _intro: ../basics/introduction.md
```

This is a convenient mechanism within a single input file. 
But if you have a larger project and use the same link definition in multiple files you would still end up
with unnecessary repetition. 
You could use the `@:import` directive and define them in a separate markup file, 
but Laika has an even more convenient way to centrally define links. 
Simply add them to the project's configuration:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.rewrite.link.{ LinkConfig, TargetDefinition }

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LinkConfig.empty
    .addTargets(
      TargetDefinition.external("Example 1", "https://example1.com/"),
      TargetDefinition.external("Example 2", "https://example2.com/")
    )
  )
```

@:choice(library)
```scala mdoc:silent
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.rewrite.link.{ LinkConfig, TargetDefinition }

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(LinkConfig.empty
    .addTargets(
      TargetDefinition.external("Example 1", "https://example1.com/"),
      TargetDefinition.external("Example 2", "https://example2.com/")
    )
  )
  .build
```
@:@

They can then be used within text markup the same way as if they would be defined within the file:

```markdown
Please have a look at [Example 1] or [Example 2]. 
```


External Links
--------------

External links can also be expressed by the text markup's native syntax, but in contrast to internal links 
they are not validated during a transformation.

Example for an inline link in Markdown:

```markdown
Please read the [Specification](https://markup/specification.html). 
```

Example for an indirect link via separate definition:

```markdown
Please read the [Specification][markup-spec].

[markup-spec]: https://markup/specification.html
```

Like with internal links, [Global Link Definitions] can be used to avoid the repetition of
declaring the same link target in multiple files.


Linking by Section Headline
---------------------------

The native linking syntax provided by the supported text markup formats can be used to directly refer
to a section by its headline.

If you have the following headline in one of your documents:

```markdown
Monkey Gone To Heaven
---------------------
```

Then you can use this title with standard link syntax:

```laika-md
Here are the lyrics for [Monkey Gone To Heaven].
```

In the example above, the section headline will also become the link title.
In cases where you want to use a link text that differs from the headline text,
you can also use the alternative syntax:

```laika-md
You have to listen to this [song][Monkey Gone To Heaven].
```


### Resolving Ambiguity

One convenient aspect of linking by section headline is that the headline you refer to does not even have to be
globally unique within your input tree. 
It will find the closest headline in the current scope.

For example, if you have a structure where every chapter has a page titled "Introduction", 
any other page within that chapter (directory) can refer to this page simply by using `[Introduction]`.

Even if the headline is not unique within the current scope, it will further disambiguate based on the level
of the headline.
A level 1 headline with have precedence over a level 3 headline for example.

Only duplicate headlines within the same scope and with the same level will lead to an error and cause the
transformation to fail.


Linking to API Documentation
----------------------------

The `@:api` directive can be used to link to API documentation by providing just the fully qualified type name:

```laika-md
See @:api(scala.collection.immutable.List) for details.
```

This directive requires the base URI to be defined in the project's configuration:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.rewrite.link.{ LinkConfig, ApiLinks }

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LinkConfig.empty
    .addApiLinks(ApiLinks(baseUri = "https://example.com/api"))
  )
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.rewrite.link.{ LinkConfig, ApiLinks }

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(LinkConfig.empty
    .addApiLinks(ApiLinks(baseUri = "https://example.com/api"))
  )
  .build
```
@:@

If you use different APIs hosted on different servers, you can associate base URIs with package prefixes,
while keeping one base URI as a default for all packages that do not match any prefix:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.rewrite.link.{ LinkConfig, ApiLinks }

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LinkConfig.empty
    .addApiLinks(ApiLinks("https://example.com/api"))
    .addApiLinks(ApiLinks("https://somewhere-else/").withPackagePrefix("com.lib42"))
  )
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.rewrite.link.{ LinkConfig, ApiLinks }

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(LinkConfig.empty
    .addApiLinks(ApiLinks("https://example.com/api"))
    .addApiLinks(ApiLinks("https://somewhere-else/").withPackagePrefix("com.lib42"))
  )
  .build
```

@:@

In cases where this simple mechanism is not sufficient, you can always define [Custom Link Directives].


Linking to Source Code
----------------------

The `@:source` directive can be used to link to the source code of types by providing just the fully qualified type name:

```laika-md
You can examine the source code of @:source(laika.api.Transformer) for inspiration.
```

It is very similar to the `@:api` directive with only a few subtle differences in its configuration.
This directive requires the base URI and suffix to be defined in the project's configuration:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.rewrite.link.{ LinkConfig, SourceLinks }

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LinkConfig.empty
    .addSourceLinks(
      SourceLinks(baseUri = "https://github.com/team/project", suffix = "scala")
    )
  )
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.rewrite.link.{ LinkConfig, SourceLinks }

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(LinkConfig.empty
    .addSourceLinks(
      SourceLinks(baseUri = "https://github.com/team/project", suffix = "scala")
    )
  )
  .build
```
@:@

If you use link to different sources hosted on different servers, you can associate base URIs with package prefixes,
while keeping one base URI as a default for all packages that do not match any prefix:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.rewrite.link.{ LinkConfig, SourceLinks }

laikaConfig := LaikaConfig.defaults
  .withConfigValue(LinkConfig.empty
    .addSourceLinks(SourceLinks(
      baseUri = "https://github.com/team/project", 
      suffix = "scala"
    ))
    .addSourceLinks(SourceLinks(
      baseUri = "https://github.com/elsewhere/project", 
      suffix = "scala"
    ).withPackagePrefix("com.lib42"))
  )
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.rewrite.link.{ LinkConfig, SourceLinks }

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(LinkConfig.empty
    .addSourceLinks(SourceLinks(
      baseUri = "https://github.com/team/project", 
      suffix = "scala"
    ))
    .addSourceLinks(SourceLinks(
      baseUri = "https://github.com/elsewhere/project", 
      suffix = "scala"
    ).withPackagePrefix("com.lib42"))
  )
  .build
```

@:@


Generating Navigation Trees
---------------------------

Depending on the output formats you intend to generate, you have different options for producing
tables of contents and navigation bars.


### Websites

The default Helium theme provides a main navigation tree in the left sidebar and page navigation on the 
right out of the box.

For minor customizations like changing the depth of the generated navigation tree or manually adding
entries to the auto-generated ones, you can use the Helium API as documented in [Main Navigation].

If you need more advanced customization and want to include your own templates, 
either for overriding the Helium templates or when not using a theme at all,
you can easily generate custom navigation structures with the `@:navigationTree` directive described below. 

For more details on templating, see [Creating Templates].


### EPUB & PDF

Laika's E-book support automatically generates navigation elements for EPUB and PDF readers.
These are not part of the templates for these output formats, therefore have to be configured separately
as shown in [E-Books (EPUB & PDF)].

You can also additionally include a table of content in the rendered pages, by using the `@:navigationTree` 
directive described below in the templates for EPUB and PDF.


### The navigationTree Directive

This flexible directive allows to freely combine auto-generated navigation trees based on your input tree's
structure with manual entries.

It inserts a `NavigationList` node into the tree - like all directives it does generate
an AST element as its result and does not directly produce string output in the target format. 

A navigation bar for the content of the entire project can be generated by referring to the virtual root:

```laika-md
@:navigationTree {
  entries = [ { target = "/" } ]
}
```

For generating a navigation tree for the current page with all its sections, you can refer to the current document
with `#`:

```laika-md
@:navigationTree {
  entries = [ { target = "#" } ]
}
```

But you can also specify any absolute or relative path as the root of the entry:

```laika-md
@:navigationTree {
  entries = [ 
    { target = "../introduction" }
    { target = "../reference" }
  ]
}
```

These path references can either point to a directory or an individual document.

#### Limiting the Depth

By default the navigation tree will allow an arbitrary depth and not only reflect the hierarchy of directories
and document and their titles, but also include the individual sections within the documents.

If you want to restrict the tree to a particular navigation depth, you can do so with the `defaultDepth`
attribute on the root level, or the `depth` attribute on each individual entry:

```laika-md
@:navigationTree {
  entries = [ { target = "/", depth = 3 } ]
}
```

#### Excluding Document Sections

If you want to generate a pure document-based navigation, which is quite common for the main navigation side bar,
you can also generally disable section links with:

```laika-md
@:navigationTree {
  entries = [ { target = "/", excludeSections = true } ]
}
```

This way it will render documents as the leaf nodes and ignore their sections, even if the maximal depth has not been
reached yet. 
This attribute can be used on the root level where it applies to all entries,
or for each individual entry as shown in the example above.

#### Excluding the Root Node

By default each entry listed will be represented by its root node with its title, 
with all children nested one level below. 
Alternatively, when `excludeRoot` is set, 
the root can be skipped and its children inserted into the final structure directly.


#### Specifying Additional Styles

By default, all navigation items get the styles `nav` and `levelN` applied, with N being the level number. 
For HTML output these will then be rendered as class attributes.

If you generate different kinds of navigation trees and want additional class attributes for styling them
differently, you can set the `itemStyles` attribute:

```laika-md
@:navigationTree {
  itemStyles = [ "page-nav" ]
  entries = [ { target = "/", depth = 3 } ]
}
```

#### Adding Manual Nodes

Finally you can also include hard-coded link targets in addition to the auto-generated nodes:

```laika-md
@:navigationTree {
  entries = [ 
    { target = "/", depth = 3, excludeRoot = true }
    { target = "http://twitter.com/me-me-me", title = "Twitter" }
  ]
}
```

In this example an auto-generated navigation tree with depth 3 will be inserted, but an additional element
with an external link target will be added as the final node.

Manual nodes can also form a nested structure like this:

```laika-md
@:navigationTree { 
  entries = [
    { title = Link 1, target = "http://domain-1.com/"}
    { title = Section, entries = [
      { title = Link 2, target = "http://domain-2.com/"}
      { title = Link 3, target = "http://domain-3.com/"}
    ]}   
  ] 
}
```

In this case the entry with the title 'Section' does not have a target defined and only serves as a section
header without a link.


Breadcrumbs
-----------

The classic `@:breadcrumb` component generates a simple, flat list of links reflecting the directory structure
from the root to the current document:

@:image(../img/breadcrumb.png) {
  alt = Breadcrumb
  style = small-image
  intrinsicWidth = 321
  intrinsicHeight = 88
}

This component works best in a directory structure that contains [Title Documents], as otherwise the
segments which represent a directory level would have nowhere to link to and would render as plain text.


Auto-Numbering
--------------

Laika supports auto-numbering of documents or sections or both. 
The term sections in Laika refers to structure obtained from the headlines within documents.

If you enable both the section numbers will be added to the document number. 
E.g. in the document with the number `2.1` the number for the first section will be `2.1.1`.

Auto-numbering can be switched on per configuration:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.rewrite.nav.AutonumberConfig

laikaConfig := LaikaConfig.defaults
  .withConfigValue(AutonumberConfig.allEnabled.withMaxDepth(3))
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.markdown.github.GitHubFlavor
import laika.rewrite.nav.AutonumberConfig

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue(AutonumberConfig.allEnabled.withMaxDepth(3))
  .build
```
@:@

The configuration above will number both documents and sections within documents, but stop after the third level. 
The numbers will be added to the headers of the sections and also appear in tables of contents.

@:image(../img/auto-numbering.png) {
  alt = Auto-Numbering
  intrinsicWidth = 680
  intrinsicHeight = 405
}

The default setting for Laika has auto-numbering switched off completely.


Pretty URLs
------------

The library contains an extension for publishing a site with so-called "pretty URLs", 
where a path like `foo/bar.html` gets translated to `foo/bar/index.html` which in turns means
that links to that page can be expressed as `foo/bar/`, essentially stripping the `.html` suffix.

The extension changes both, the physical structure of files of the generated site as well as 
the rendering of internal links. It has no effect on other formats like EPUB or PDF.

In contrast to some other site generators, this extension is disabled by default.
It can be enabled like any other extension:

@:select(config)

@:choice(sbt)
```scala mdoc:compile-only
import laika.rewrite.nav.PrettyURLs

laikaExtensions += PrettyURLs
```

@:choice(library)
```scala mdoc:compile-only
import laika.api._
import laika.format._
import laika.rewrite.nav.PrettyURLs

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(PrettyURLs)
  .build
```
@:@


Custom Link Directives
----------------------

The directives presented in this chapter are only a default selection of navigation features that hopefully
already cover most common scenarios.

If you have very specific requirements you can always fall back to implementing your own directives.
You can examine the implementation of Laika's default link directives as an example.

For details see [Implementing Directives].
