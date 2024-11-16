
Creating Templates
==================

Laika comes with a lightweight template engine that allows to customize the output
of the transformed markup documents.

When using a theme, creating a custom template is often not necessary, 
as the theme will have higher level configuration options that drive the look and feel of the output,
based on its own internal templates.

When subtle tweaks to a theme are not sufficient or when working without a theme,
adding custom templates will give you additional flexibility in controlling the output.

A template lets you specify where the content from text markup documents gets inserted
and allows to add additional dynamic elements like navigation bars. 
It does not give control over how individual AST nodes from the markup document get rendered.
For customizing this aspect, see [Overriding Renderers]. 


Document Names and Locations
----------------------------

There are two kinds of template documents you can add to your input directories:

* Default templates that get applied to all markup documents that do not explicitly specify a template

* Additional templates that only get applied when a markup document explicitly refers to them


### Default Templates

A default template for each output format can be placed into the root directory of the input tree.
If you have multiple input directories, it needs to be placed into one of the roots.

The naming pattern for default templates is `default.template.<suffix>`.
Concretely this would be:

* `default.template.html` for generating sites.
* `default.template.epub.xhtml` for EPUB.
* `default.template.fo` for PDF (since PDF uses XSL-FO as an interim format).

This template is then used for all markup documents unless overridden by one of the two mechanisms described
in the following sections. 


### Overrides per Directory

A sub-directory may contain an overriding default template.
It would be detected by the same naming pattern as the root default template, 
and would be applied to all markup documents in this directory and its sub-directories.


### Overrides per Document

Finally, when you want to apply a custom template to individual markup documents,
you can explicitly refer to it in the configuration header of the markup document:

```laika-html
{%
  laika.template = ../custom.template.html
%}
```

The path is interpreted relative to the document, 
but has to be somewhere within one of the specified input directories, 
as templates are parsed and cached upfront before getting applied to documents.

All non-default templates must have a name with the pattern `<name>.template.<suffix>` 
where `<name>` is anything other than `default` and `<suffix>` is the same matching suffix as for default templates.


### Overriding Template Fragments in the Helium Theme

The sections above dealt with the core feature set of replacing the entire template, either globally 
or for an individual page.
When using the Helium theme there is an alternative option of just replacing a specific fragment of the main template,
e.g. only the part that defines the left navigation pane.

More fine-grained template replacements avoid the problems arising from replacing a template in its entirety:
when the library enhances or fixes an aspect of the built-in template, these changes would need to be carefully merged
back into your local copy of the template, or you'd miss out on all future enhancements.

For details about replacing Helium template fragments see [Customizing Template Fragments]. 


Template Syntax
---------------

The following, minimal example shows a template for HTML output:

```laika-html
<html>
  <head>
    <title>${cursor.currentDocument.title}</title>
  </head>
  <body>
    @:navigationTree { entries = [{ target = "/" }] } 
    <div class="content">
      ${cursor.currentDocument.content}
    </div>
  </body>
</html>
```

Apart from raw content in the target format HTML, it contains:

* A substitution variable that inserts the title (`${cursor.currentDocument.title}`).
  See [Substitution Variables] for details.
  
* Another substitution variable that inserts the content of the markup document (`${cursor.currentDocument.content}`).
  See [Inserting Content from Markup Documents] for details.

* A directive `@:navigationTree` for rendering a navigation tree from the root (`"/"`) of the input tree.
  See [Directives] for details.


Inserting Content from Markup Documents
---------------------------------------

When a template gets applied to text markup documents, 
you need to explicitly specify where the content from those documents should be inserted.


### Main Document Body

As shown in the example above, the substitution reference `${cursor.currentDocument.content}` causes
the main body of the associated text markup document to be rendered in that position.

The main body is the entire content of the markup document except for regions marked as fragments.

The internal mechanics of this insertion, which is technically just a merging of two ASTs,
is described in more detail in [Internal Representation].


### Document Fragments

In some cases you might want the rendered content from the markup document to appear in more than just one place.
You may have a central column in the template for the main content and a sidebar or footer that needs to be
populated separately.

The separate sections can be marked in the text markup with the `@:fragment` directive:

```laika-md
@:fragment(sidebar)

This content will be *parsed* like all other
content, but will be available separately from the document content.
The block elements extend until the `@:@` fence below.
  
Therefore this line still belongs to the fragment.

@:@
  
This line doesn't and will be part of the main document content.
```

Within templates you can then refer to this content with the substitution variable: 

```laika-html
`${cursor.currentDocument.fragments.sidebar}`
```

The fragment names (`sidebar` in this case) can be freely chosen, they do not have any predefined meaning within Laika.


Substitution Variables
----------------------

The previous section already introduced the substitution variables for inserting content from markup documents,
but there are plenty of other predefined variables you can use, in addition to your own definitions.

The syntax is part of Laika's HOCON support and matches the [HOCON spec].

A mandatory reference can be inserted with the key of the variable enclosed between `${` and `}`:

```laika-html
${cursor.currentDocument.title}
```

If such a value is undefined, the transformation will fail (unless you tweaked the configuration for [Error Handling]).

An optional reference can be expressed by adding an additional `?` right before the variable key:

```laika-html
${?cursor.currentDocument.title}
```

If an optional reference points to an undefined value, the reference in the template is substituted with an empty string.

For a full list of predefined values see [Substitution Variables](../07-reference/02-substitution-variables.md) in the Reference section.
For instructions on defining your own see [User-Defined Variables].


Directives
----------

Directives are Laika's mechanism for extending the syntax of templates or text markup without the need to implement
a custom parser.

A directive always starts with an `@:`, the name of the directive and optionally attributes or body elements.

* Positional attributes can follow enclosed in `(` and `)`, e.g. `@:fragment(sidebar)`.

* Named attributes in HOCON format can follow between `{` and `}` and can be any valid HOCON structure,
  e.g. `@:navigationTree { entries = [{ target = "/" }] }`.
  
* Body elements is regular markup or template content immediately after the directive declaration until
  a `@:@` fence, as shown in the example in [Document Fragments]. 
  The AST for these parsed body elements will be passed to the directive implementation for processing. 
  
* Positional and named attributes can theoretically both appear in the same directive, 
  but this is usually avoided for the sake of clarity and simplicity.
  
Some directive implementations require a set of mandatory attributes and may cause the transformation to fail
should they be missing.

The most relevant directives for templates are those for generating navigation elements, 
like [Generating Navigation Trees] or [Breadcrumbs].
 
For a full reference of the directives provided by Laika out of the box, see [Standard Directives].

For instructions on how to write your own, see [Implementing Directives].


Internal Representation
-----------------------

Templates get parsed into an AST just like text markup documents.
This has several convenient consequences.

First, custom elements like directives can be implemented in a way that they produce AST nodes and not
immediately string content in the output format.
Therefore, a single implementation of a template directive can usually be used for all supported output formats. 

Secondly the implementation of "applying a template to a markup document" becomes close to trivial.
It is merely the insertion of one AST at a particular node in another AST.
Let's illustrate this with an example.

This is the template shown earlier in this chapter, further simplified to keep the AST output concise:

```laika-html
<html>
  <head>
    <title>${cursor.currentDocument.title}</title>
  </head>
  <body>
    ${cursor.currentDocument.content}
  </body>
</html>
```

The AST for this template looks like this:

```laika-ast
TemplateRoot - TemplateSpans: 5
. TemplateString - '<html>|  <head>|    <title>'
. TemplateContextReference(cursor.currentDocument.title,true)
. TemplateString - '</title>|  </head>|  <body>|    '
. TemplateContextReference(cursor.currentDocument.content,true)
. TemplateString - '|  </body>|</html>'
```

The raw content gets represented by `TemplateString` nodes.
The AST renderer truncates beyond a certain length and replaces newlines with `|` for putting more emphasis 
on the structure.
The variable reference gets represented by a `TemplateContextReference` which will be resolved later.

More complex real-world scenarios will have additional node types, like those for representing a directive.

Next we create an equally minimal text markup document:

```markdown
Headline
========

Some Text.
```

This produces just the Title node and a Paragraph in the resulting AST:

```laika-ast
RootElement - Blocks: 2
. Title(Id(headline) + Styles(title)) - Spans: 1
. . Text - 'Headline'
. Paragraph - Spans: 1
. . Text - 'Some Text.'
```

This is just the tip of the iceberg of available node types, for a general overview see [The Document AST].

We then finally apply the template to the document. 
In a normal Laika transformation this happens automatically as the final step before rendering,
but when working directly with the document model, 
you can alternatively call `applyTo` on a `TemplateDocument` instance.

The result will look like this:

```laika-ast
RootElement - Blocks: 1
. TemplateRoot - TemplateSpans: 5
. . TemplateString - '<html>|  <head>|    <title>'
. . TemplateElement(0)
. . . SpanSequence - Spans: 1
. . . . Text - 'Headline'
. . TemplateString - '</title>|  </head>|  <body>|    '
. . EmbeddedRoot(4) - Blocks: 2
. . . Title(Id(headline) + Styles(title)) - Spans: 1
. . . . Text - 'Headline'
. . . Paragraph - Spans: 1
. . . . Text - 'Some Text.'
. . TemplateString - '|  </body>|</html>'

```

The two nodes which previously represented the two context references have been replaced
by the corresponding AST from the parsed markup.

In case of the main content, the node `EmbeddedRoot` represents the insertion point of one AST into another.

This final AST is the model that will be passed on to renderers.
