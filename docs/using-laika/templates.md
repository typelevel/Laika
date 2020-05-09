
Templates
=========

Laika comes with a lightweight, logic-less template engine that allows you to customize the output
of the transformed markup documents.

It is different than common template engines as the mechanisms to implement custom
logic in tags differs from most other template systems. These tags do not immediately 
render the final string output. Instead, they insert nodes into the document tree model (a tree
of case classes) which are not specific to any particular output format. 
As a result, a custom template directive can be reused for different output formats.

The engine also differs from template engines in the Scala ecosystem in that it does not
get compiled. The resulting lack of type safety is a negligible issue, since in the particular
context of text markup transformation most of the things you refer to from a template
do not originate in program code anyway, but instead come from markup and configuration 
files. It also helps keeping the engine very lightweight. 

The parsing of a template is therefore very similar
to the parsing of text markup in that the interim result is a tree of case classes.
Applying a template to a markup document then only means inserting nodes from one
into the other before rendering, based on the types and location of tags you 
used in the template. 


from embedded ====================================================================================

### Templates

The directories that contain the markup documents can optionally contain
templates that get applied to the parsed documents. A very basic template
for HTML output may look like this:

```laika-html
<html>
  <head>
    <title>${document.title}</title>
  </head>
  <body>
    <div class="content">
      ${document.content}
    </div>
  </body>
</html>
```
    
The two placeholders enclosed in double curly braces will be replaced with the
title and content of the parsed markup document. Everything else will be copied
to the final document unchanged.

Every directory can contain a file with the name `default.template.<suffix>` that
automatically gets applied to all documents in that directory and its subdirectories,
unless overridden in a subdirectory. There are also ways to manually declare a
template for a specific markup document only. 

The suffix must match the output format of the transformation, e.g. `.html` for HTML,
`.fo` for PDF (as it renders via XSL-FO) and `.epub.xhtml` for EPUB.

For more details on the template engine, see the chapter [Templates].

For features like titles, sections, cross-linking, auto-numbering 
and tables of content, see the chapter [Document Structure].

==================================================================================================


Template Syntax
---------------

The following example shows all three markup constructs you can use in
templates:

```laika-html
{% 
  siteLinks {
    catalogue: ../catalogue/
    contact: ../contact/
  }
%}  
<html>
  <head>
    <title>${document.title}</title>
  </head>
  <body>
    @:toc
    <div class="content">
      ${document.content}
    </div>
  </body>
</html>
```

It contains a configuration header (enclosed between `{%` and `%}`),
two variable references (enclosed between `${` and `}`) and
a standard directive (starting with `@:`), in this case the
`toc` directive for generating a table of contents.

These three constructs are described in the following sections.


### Configuration Headers

A configuration header can only appear at the beginning of a
template document and has to be enclosed between `{%` and `%}`.
The supported syntax is equivalent to the one used in the
[Typesafe Config] library. It is a superset of JSON called [HOCON].

Laika comes with its own parser implementation for HOCON,
to retain the overall design of full referential transparency
and avoid the Java API of the Typesafe Config library.

There are a few configuration entries that have a special meaning
for Laika, e.g the `autonumbering`, `pdf` or `epub` entries.
These are all documented in their respective chapters.

But you can also add arbitrary entries like in the example above 
and then refer to them from a variable reference. 
The `contact` entry above for example can be referred to with 
`${siteLinks.contact}` anywhere inside this template. 

Likewise you can refer to variables defined in:

* The document that is rendered with this template
* The configuration of the directory the rendered document resides in 
  and all its parent directories up to the (virtual) root of the tree. 

HOCON configuration is more often found in headers of text markup
documents or in directory configuration, but nevertheless also
supported in template headers if needed.

[Typesafe Config]: https://github.com/typesafehub/config
[HOCON]: https://github.com/typesafehub/config#features-of-hocon


### Variable References

A variable reference can appear anywhere in a template
and has to be enclosed between `${` and `}`. During
rewriting these references get replaced by their corresponding
value, which may be a standard document node which gets inserted
into the tree or a simple string.

These are the variables you can use in templates:

* `document.content`: inserts the full content from the corresponding
  markup document at the location of the reference
* `document.title`: inserts the title of the document, see
  [Document Title] for details
* `document.fragments`: inserts the fragment with the corresponding
  name into the template (e.g. `document.fragments.footer` refers to
  the fragment with the name `footer`), see [Document Fragments]
  for details.
* `document.path`: the absolute (virtual) path of the document inside the tree
* `parent.path` the path of the immediate parent tree of this document
* `parent.title` the title of the parent tree 
* `root.title`: the title of the root tree
* any other path: let's you access any configuration variable, e.g. ${autonumbering.scope}
  refers to the scope attribute from the template example at the beginning of this section.
  You can refer to configuration entries from any of the following sources (which the
  resolver will try in this order):
    - a configuration header in the template
    - a configuration header in the corresponding document  
    - a configuration file with the name `directory.conf` inside the current directory
    - a configuration file with the name `directory.conf` inside any parent directory
    - a configuration file specified programmatically
  
The use of these references is meant to be quite straightforward. The template engine
is logic-less, so if you find yourself creating complex constructs of these variable
references and directives for looping, you should probably consider creating a custom
directive. These are easy to implement and let you access all the variables listed
above (and more) in Scala code.

An example would be a highly customized generator for a table of contents (in case
the one built-in is not sufficient for your needs). This can probably be easier
achieved with a directive than with variable references.


### Directives

Directives are Laika's extension hook for both, templates and text markup.
They always start with `@:`, the name of the directive, optionally followed 
by one or more attributes enclosed between `{` and `}` and an optional
body element ending on a `@:@` fence.

A very minimal example is the `toc` directive for inserting a table of contents.
Since all its attributes are optional, it can simply be used like this:

```laika-html
@:toc
```

A more complete example is the use of the `for` directive:

```laika-html
@:for(catalog.products)

<li><a href="#${_.id}">${_.description}</a></li>

@:@
```

Here `for` is the name of the directive, `catalog.products` is an unnamed
attribute (where in this case the value is interpreted as a variable reference,
pointing to a path the user defined in the configuration),
and finally, the body of the directive followed by the `@:@` fence. The exact
semantics of this particular directive are explained in the section 
[Standard Template Directives].

You can either use one of the built-in directives or create your own as
described in [Directives][../extending-laika/directive.md:Directives].



How Templates are Matched To Documents
--------------------------------------

There are two simple options for specifying which template to use:


### Default Template per Directory

The most convenient way is to simply add a file with the name `default.template.<suffix>`
(e.g. `default.template.html`) to the directory containing your text markup documents. 
This template is then used for all documents in this directory and its subdirectories, 
unless it is overridden by either a different default template in one of the 
subdirectories or by explicitly specifying a template in one of the text markup documents.


### Explicit Template per Document

Alternatively (or in addition to default templates) you can explicitly specify a template
in a configuration header of a text markup document:

```laika-html
{%
  template: custom.template.html
%}
```
    
This will override any default template for the directory. The path is relative to the
document, but has to be somewhere below the root directory processed by Laika, as
templates are parsed and cached upfront, before getting applied to documents.

All templates must have the suffix `.template.<suffix>` where the suffix matches
the output format of the transformation operation, e.g. `.template.html`.



Standard Template Directives
----------------------------

Laika comes with a small number of built-in directives intended to cover the most
common scenarios. Some of them can only be used in templates, whereas others can
also be used in text markup, as stated for each individual directive below.



### The `toc` directive

Can be used in templates and as block elements in text markup.

Generates a table of contents containing the documents of the tree
and/or the sections inside these documents.

This directive is explained in detail in the [Document Structure] chapter.


### The `fragment` directive

Can only be used in block elements in text markup, not in templates (yet).

Produces a block element which is not part of the main body of the markup
document (so will not be rendered with a `${document.content}` reference).
Instead it can be referred to by `${document.fragments.<fragmentName>}`.

This allows to keep some sections of your document separate, to be rendered
in different locations of the output, like headers, footers or sidebars.

This directive is explained in detail in the [Document Structure][Document Fragments] chapter.


### The `format` directive

Can only be used in block elements in text markup.

The body of such a directive will only be included into the
output by renderers for the specified type:

```laika-html
@:format(html)

This text only appears in the HTML output,
not in PDF, EPUB or any other format.

@:@
```

Note that the body of a block directive needs to be indented,
and does not appear between curly braces.


### The `style` directive

Can be used in block and span elements in text markup.

Adds a style to the root element of the body of the directive,
that can be used by renderers to change the appearance of the
rendered element.

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

### The `for` directive

Can only be used inside templates.

Executes the body of the directive multiple times for each element of
a non-empty collection (or array-type entry in a configuration section),
not at all for the values `null`, `false`, `None`, empty strings and
empty collections, and exactly once for all other values.

In case of non-empty values their properties will be made available
for use inside the body part without any prefix:

```laika-html
@:for(catalog.products)

<li><a href="#${_.id}">${_.description}</a></li>

@:@ 
```

In this example `id` and `title` are properties of the `catalog.products`
property that the user needs to define in once of the locations where 
HOCON attributes are accepted.

You can also specify a fallback, a body part that gets executed
for empty values:

```laika-html
@:for(catalog.products)
<li><a href="#${_.id}">${_.description}</a></li>
 
@:empty
<p>No products available.</p>

@:@
```

Each directive can have one unnamed default body and any number
of named bodies. The `for` directive supports a body element
named `empty`, the name has to be enclosed in `~` and `:` and
the body in braces like always. This combination of characters
is still readable, relatively easy to memorize, while minimizing
the likeliness to conflict with plain template text.


### The `if` directive

Can only be used inside templates.

Executes the body of the directive exactly once for a Boolean
`true` or the strings `true`, `on`, `yes` and `enabled`.

```laika-html
@:if(showSidebar)
<div class="sidebar">...</div>
@:@
```

In this example `showSidebar` is a custom configuration entry
that you have set explicitly in a configuration header or file.

You can also specify a fallback with `@:else`, 
or even secondary conditions with `@:elseIf`:

```laika-html
@:if(showSidebar)
<div class="sidebar">...</div>

@:elseIf(showInfobox)
<div class="infobox">...</div>

@:else
<p>Something else</p>

@:@
```

### The `pageBreak` directive

Can only be used in block elements in text markup.

Only page-oriented output formats like PDF or XSL-FO
know how to render page break elements.

This directive does not support any attributes:

```laika-md
@:pageBreak
```
