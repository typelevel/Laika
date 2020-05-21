
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

It is documented in detail in [The @:navigationTree Directive] in the Navigation chapter.


### `@:breadcrumb`

Can be used in templates and as block elements in text markup.

This standard breadcrumb component generates a simple, flat list of links reflecting the directory structure
from the root to the current document. 
See [The @:breadcrumb Directive] in the Navigation chapter for details.


### `@:api`

Can be used as inline element and within links in text markup.

Serves as a shortcut for creating links to API documentation, e.g. `@:api(laika.api.Transformer)`.

See [Linking to API Documentation] for details.


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
in the configuration. See @:(User-Defined Variables) for details on defining them.

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
in the configuration. See @:(User-Defined Variables) for details on defining them.

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
