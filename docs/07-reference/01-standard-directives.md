
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


Applying Styles
---------------

Laika already produces a small set of class attributes in the rendered output, 
e.g. `nav` and `level1` for a navigation element on the first level.
But you can alternatively assign style hints to markup explicitly.
   

### `@:style`

Can be used in block and span elements in text markup.

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


Special Markup Blocks
---------------------

These directives mark blocks in a way that they may not be part of the main content block that is accessed
via `@{cursor.currentDocument.content}` in templates.


### `@:fragment`

Can only be used in block elements in text markup.

Produces a block element which is not part of the main body of the markup document.
Instead it can be referred to by `${cursor.currentDocument.fragments.<fragmentName>}`.

This allows to keep some sections of your document separate, to be rendered
in different locations of the output, like headers, footers or sidebars.

This directive is explained in detail in the [Document Fragments] chapter.


### `@:format`

Can only be used in block elements in text markup.

The body of such a directive will only be included into the output by renderers for the specified type:

```laika-html
@:format(html)

This text only appears in the HTML output,
not in PDF, EPUB or any other format.

@:@
```


HTML Templates
--------------

These directives can only be used in HTML or EPUB templates, 
as they drive the inclusion of tags in the HTML `<head>` section.

If you use the Helium theme and do not design your own templates or themes, 
you do not need to use these directives directly, as the Helium template already contain them.
In that case you can control which CSS and JS files to link via the theme's configuration
as described in [Auto-Linking CSS & JS Files].


### @:linkCSS

Causes the automatic inclusion of `<link>` tags for CSS files found in the input tree.

```laika-html
@:linkCSS
```

When used without attributes as in the example above it will simply link any CSS file found in the input tree.

If you only want to pick files from specific locations, you can use the `paths` attribute:

```laika-html
@:linkCSS {
  paths = [ /shared, /css ]
}
```

The order of the link tags will reflect the order of paths specified.
You can point to individual files or directories. 
In the latter case all files found in that directory or any sub-directory will be included.

Note that the paths, like everything in Laika, are within the virtual path of the input tree you configured.
See [Virtual Tree Abstraction] for details.


### @:linkJS

Causes the automatic inclusion of `<script>` tags for JavaScript files found in the input tree.

```laika-html
@:linkJS
```

When used without attributes as in the example above it will simply link any JavaScript file found in the input tree.

If you only want to pick files from specific locations, you can use the `paths` attribute:

```laika-html
@:linkJS {
  paths = [ /shared, /js ]
}
```

The order of the script tags will reflect the order of paths specified.
You can point to individual files or directories. 
In the latter case all files found in that directory or any sub-directory will be included.

Note that the paths, like everything in Laika, are within the virtual path of the input tree you configured.
See [Virtual Tree Abstraction] for details.


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
