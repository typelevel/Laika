
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


Template Syntax
---------------

The following example shows all three markup constructs you can use in
templates:

    {% 
      autonumbering {
        scope: sections
        depth: 2      
      }
    %}  
    <html>
      <head>
        <title>{{document.title}}</title>
      </head>
      <body>
        @:toc.
        <div class="content">
          {{document.content}}
        </div>
      </body>
    </html>
    
It contains a configuration header (enclosed between `{%` and `%}`),
two variable references (enclosed between `{{` and `}}`) and
a standard directive (starting with `@:`), in this case the
`toc` directive for generating a table of contents.

These three constructs are described in the following sections.


### Configuration Headers

A configuration header can only appear at the beginning of a
template document and has to be enclosed between `{%` and `%}`.
The supported syntax is equivalent to the one used in the
[Typesafe Config] library (and in fact that library is used
under the hood for parsing these headers). The syntax
is a superset of JSON called [HOCON].

There are a few configuration entries that have a special meaning
for Laika, like the `autonumbering` entry in the example above.
These are all documented in their respective chapters.

But you can also add arbitrary entries and then refer to them
from a variable reference. The `scope` entry above for example
can be referred to with `{{config.autonumbering.scope}}`.

You can also have a configuration header in a markup document
(supported for both Markdown and reStructuredText) and then
refer to entries in a header in a markup document from
inside the corresponding template (and vice versa)

[Typesafe Config]: https://github.com/typesafehub/config
[HOCON]: https://github.com/typesafehub/config#features-of-hocon


### Variable References

A variable reference can appear anywhere in a template
and has to be enclosed between `{{` and `}}`. During
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
* `document.sections`: the section structure of the document which you can loop
  over with the `@:for` directive described below; the section objects have the 
  following properties:
    - `id`: the id to use when linking to that section
    - `title.content`: the formatted title of the document (retaining all inline markup)
    - `title.text`: the unformatted plain text of the title
    - `content`: a sequence of further child sections
* `parent.path` and `root.path`: the path of the immediate parent tree of this document
   and the path of the root tree
* `parent.title` and `root.title`: the title of the parent and root trees
* `parent.documents` and `root.documents`: the documents of the parent and root tree 
   respectively, these objects have the same properties like the `document` references
   listed above
* `parent.subtrees` and `root.subtrees`: the subtrees of the parent and root tree
  which in turn have the same properties like the `parent` and `root` references listed
  here
* `config`: let's you access any configuration variable, e.g. {{config.autonumbering.scope}}
  referes to the scope attribute from the template example at the beginning of this section.
  You can refer to configuration entries from any of the following sources (which the
  resolver will try in this order):
    - a configuration header in the corresponding document  
    - a configuration header in the template
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
by one or more attributes and then either ending with a `.` and no body element 
or a `:` followed by one or more bodies enclosed between `{` and `}`.

A very minimal example is the `toc` directive for inserting a table of contents.
Since all its attributes are optional, it can simply be used like this:

    @:toc.
    
A more complete example is the use of the `for` directive:

    @:for "document.sections": {
      <li><a href="#{{id}}">{{title.content}}</a></li>
    } 

Here `for` is the name of the directive, `"document.sections"` is an unnamed
attribute (where in this case the value is interpreted as a variable reference),
and finally, enclosed in curly braces the body of the directive. The exact
semantics of this particular directive are explained in the section 
[Standard Template Directives].

You can either use one of the built-in directives or create your own as
described in [Directives][../extending-laika/directive.md:Directives].



How Templates are Matched To Documents
--------------------------------------

There are two simple options for specifying which template to use:


### Default Template per Directory

The most convenient way is to simply add a file with the name `default.template.html`
to the directory containing your text markup documents. This template is then used
for all documents in this directory and its subdirectories, unless it is overridden
by either a different default template in one of the subdirectories or by explicitly
specifying a template in one of the text markup documents.


### Explicit Template per Document

Alternatively (or in addition to default templates) you can explicitly specify a template
in a configuration header of a text markup document:

    {%
      template: custom.template.html
    %}
    
This will override any default template for the directory. The path is relative to the
document, but has to be somewhere below the root directory processed by Laika, as
templates are parsed and cached upfront, before getting applied to documents.

All templates must have the suffix `.template.html`.

This also means that you should not have too many unused templates inside these
directories, as they will get parsed nevertheless.



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
document (so will not be rendered with a `{{document.content}}` reference).
Instead it can be referred to by `{{document.fragments.<fragmentName>}}`.

This allows to keep some sections of your document separate, to be rendered
in different locations of the output, like headers, footers or sidebars.

This directive is explained in detail in the [Document Structure][Document Fragments] chapter.


### The `format` directive

Can only be used in block elements in text markup.

The body of such a directive will only be included into the
output by renderers for the specified type:

    @:format html: This text only appears in the HTML output,
      not in PDF, EPUB or any other format.

Note that the body of a block directive needs to be indented,
and does not appear between curly braces.


### The `style` directive

Can be used in block and span elements in text markup.

Adds a style to the root element of the body of the directive,
that can be used by renderers to change the appearance of the
rendered element.

Block directive: 

    @:style subtitle: This paragraph gets the 
      subtitle style applied.
      
    While this paragraph does not.

Span directive:

    Burrito - @:style price: { £3.50 }.


### The `for` directive

Can only be used inside templates.

Executes the body of the directive multiple times for each element of
a non-empty collection (or array-type entry in a configuration section),
not at all for the values `null`, `false`, `None`, empty strings and
empty collections, and exactly once for all other values.

In case of non-empty values their properties will be made available
for use inside the body part without any prefix:

    @:for "document.sections": {
      <li><a href="#{{id}}">{{title.content}}</a></li>
    } 
    
In this example `id` and `title` are properties of the `SectionInfo`
objects inside the `document.sections` collection.

You can also specify a fallback, a body part that gets executed
for empty values:

    @:for "document.sections": {
      <li><a href="#{{id}}">{{title.content}}</a></li>
    } 
    ~empty: {
      <p>This document does not have any sections</p>
    }

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

    @:if "config.showSidebar": {
      <div class="sidebar">...</div>
    } 
    
In this example `showSidebar` is a custom configuration entry
that you have set explicitly in a configuration header or file.

You can also specify a fallback, a body part that gets executed
for other values:

    @:if "config.showSidebar": {
      <div class="sidebar">...</div>
    } 
    ~else: {
      <p>This document does not have any sections</p>
    }


### The `pageBreak` directive

Can only be used in block elements in text markup.

Only page-oriented output formats like PDF or XSL-FO
know how to render page break elements.

This directive does not support any attributes:

    @pageBreak

