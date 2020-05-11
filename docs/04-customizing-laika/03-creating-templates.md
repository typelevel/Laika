
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

