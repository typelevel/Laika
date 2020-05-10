
The Extension Bundle API
========================


from old Doc Structure chapter:

If you need to customize the document type recognition,
you can do that with a simple function and install it as
part of an extension bundle:

```scala
object MyExtensions extends ExtensionBundle {

  override val docTypeMatcher: PartialFunction[Path, DocumentType] = ...

  // ... optionally other customizations
}

val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(MyExtensions)
  .build
```

The valid return types of the matcher function correspond to the document types listed
above:

```scala
Markup, Template, Styles, Static, Config, Ignored
```
