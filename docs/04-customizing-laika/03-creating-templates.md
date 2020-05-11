
Creating Templates
==================


From old doc structure:

Document Fragments
------------------

Fragments allow to keep some sections of your document separate, to be rendered
in different locations of the output, like headers, footers or sidebars.

They produce a block element which is not part of the main body of the markup
document (so will not be rendered with a `${document.content}` reference).
Instead it can be referred to by `${document.fragments.<fragmentName>}`.

Example:

```laika-md
@:fragment(sidebar)

This content will be *parsed* like all other
content, but will be available separately from the document content.
The block elements extend until the `@:@` fence below.
  
Therefore this line still belongs to the fragment.
  
* As does
* this list

@:@
  
This line doesn't and will be part of the main document content.
```


Fragments are also available in code through a `Document` instance:

```scala
val doc: Document = ???
doc.fragments // Map[String, Element]
```

=============================================================================================

from old output page:

### Templating

Laika supports Templating for most output formats. The following example
uses variable references to include the title and content of the input
document, as well as a directive called `@toc` to inset a table of contents:

```laika-html
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
    
If you save such a template in a file called `default.template.html` in the
root directory of your input sources it will get applied to all markup documents
in those directories. You can optionally override the template for an individual
sub-directory simply by including a different template named `default.template.html`
in that sub-directory.

See [Templates][../using-laika/templates.md:Templates] for more details.

