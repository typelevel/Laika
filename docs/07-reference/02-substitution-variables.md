
Substitution Variables
======================

Substitution Variables can used in text markup documents and in templates for any output format.


Syntax
------

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

[HOCON spec]: https://github.com/lightbend/config/blob/master/HOCON.md


Pre-Defined Values
------------------

Laika provides a set of pre-defined variables in two namespaces.
One is the `cursor` namespace that holds information about the current document and its siblings and parents.
The other is the `laika` namespace that holds all values coming from Laika's internal configuration.


### Cursor Values

Values from the cursor namespace expose information about the current document and its siblings and parents.
They can be used in templates or markup documents to create links or insert content.
If you need to create complex navigation structures though, it is usually best to prefer [Implementing Directives]
over complex template logic.

Internal processing in Laika is not stringly, but based on traversing and modifying ASTs. 
Therefore, if a substitution variable listed below is described as holding an AST, 
the node in the AST representing the variable will be replaced by this referenced AST.
Rendering of the AST happens later as a final step in the processing pipeline.

This is a complete list of values exposed in the `cursor` namespace:

* `cursor.currentDocument`: A pointer to the currently processed document.
  In templates this is populated for each document the template is applied to - 
  it refers to the markup document, not the template document itself.
  Sub-keys are:
  
    * `content`: The entire AST of the markup document, see [Inserting Content from Markup Documents] for details.
    
    * `fragements`: The AST of all named fragments declared in the markup document, under their respective keys,
      e.g. `cursor.currentDocument.fragments.sidebar` for a fragment declared with `@:fragment(sidebar)` inside
      the markup document. See [Document Fragments] for details.

    * `title`: the AST of the title of this document - including formatting.

    * `path`: the absolute (virtual) path of the document inside the input tree.
    
* Access to surrounding documents via `cursor.parentDocument`, `cursor.previousDocument`, `cursor.nextDocument`,
  `cursor.flattenedSiblings.previousDocument` and `cursor.flattenedSiblings.nextDocument`.
  Any of these references may be empty, depending on the position of the current document.
  
  The `flattenedSiblings` differ from the other pointers in that they go beyond the current chapter.
  If the current document is the last in the current chapter (directory), then `cursor.nextDocument` will be empty,
  but `cursor.flattenedSiblings.nextDocument` will point to the first document in the next chapter.
  The `flattenedSiblings` are useful if you want to create a linearized, book-like navigation.
  
  Sub-keys of these document pointers are:
  
    * `title`: the AST of the title of the document - including formatting.
    
    * `absolutePath`: the absolute path in the virtual input tree as a string, e.g. `/herbs/parsley.md`.
    
    * `relativePath`: the path relative to this document as a string, e.g. `../parsley.md`.
    
* `root.title`: The title of the root node, usually the title of the website or e-book.
    

### Laika Configuration Values

These are usually not accessed in user templates and mostly intended for Laika's own internal processing, 
but are nevertheless exposed like any other value in the configuration.

* `laika.<format>.metadata`: holds the [Document Metadata] specified in configuration.

* `laika.links`: holds navigation configuration for [Global Link Definitions], [Linking to API Documentation]
  and [Disabling Validation].

* `laika.autonumbering`: the configuration for auto-numbering documents and sections. 
  See [Auto-Numbering] for details.

* `laika.pdf` and `laika.epub`: e-book configuration, see [E-Books (EPUB & PDF)] for details

* `laika.titleDocuments`: configures the names of documents that should be treated as title documents.
  See [Title Documents] for details.  


User-Defined Values
-------------------

In addition to the library's pre-defined values users can define their own values in any scope.

The only reserved namespaces for variable keys are `cursor` (e.g. `cursor.currentDocument.title`)
and `laika` (e.g. `laika.pdf.coverImage`). 
You can freely choose any key outside of these two namespaces.


### Programmatic Definition

You can add arbitrary configuration values when building a `Parser`, `Renderer` or `Transformer`:

@:select(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue("project.version", "2.4.6")
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue("project.version", "2.4.6")
  .build
```
@:@

You can refer to these values in templates and text markup:

```laika-html
The latest stable release is ${project.version}.
```

Values declared in a builder instance are available globally, 
but can be overridden by HOCON definitions in narrower scopes, as shown in the next section.


### HOCON Definition

Programmatic configuration is recommended over file-based HOCON configuration wherever possible, 
but if you need to associate values with a particular directory, text markup document or template, 
instead of declaring them globally, you can define them in the corresponding locations:

- a configuration file with the name `directory.conf` in any input directory
- a configuration header in a template
- a configuration header in a text markup document 

Just be aware that these values are then not available globally, 
but only in the corresponding scope they were defined in.

Example for defining a variable in a configuration header in a text markup document:

```laika-md
{%
  project.version = 2.0.2
%}

Document markup here...
```

This value can then be accessed within that document or inside a template applied to that document
with the usual syntax:

```laika-html
The latest stable release is ${project.version}.
```


Programmatic Access
-------------------

The same variables that you can access with HOCON-style substitution references in templates and markup files,
are also available programmatically if you are working with a document tree. 
The `DocumentCursor`, `DocumentTree`, `Document` and `TemplateDocument` types 
all have a `config` property that exposes those values:

```scala
val doc: Document = ???
val version: Either[ConfigError, String] = 
  doc.config.get[String]("project.version")
```

See @:api(laika.config.Config) for details about the Config API.
