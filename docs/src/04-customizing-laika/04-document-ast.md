
The Document AST
================

Laika decouples the semantics of the various markup formats and those of the supported output formats
by representing every document in a generic AST between parsing and rendering.
This allows to add custom processing logic only operating on the document structure itself, 
so it can be used with all supported input and output formats unchanged.

This chapter gives a brief overview over the hierarchy of the main base traits for AST nodes
as well as short listings for about 80% of available node types.


Trait Hierarchy
---------------

All AST nodes extend one of the base traits from the hierarchy as well as optionally various additional mixins.

Providing a rich collection of traits that assist with classification and optional functionality
helps with developing generic processing logic that does not need to know all available concrete types.
You can, for example, collectively process all `BlockContainer` or `ListContainer` nodes without caring
about their concrete implementation.

The traits are not sealed as the model is designed to be extensible.


### Base Traits

At the top of the hierarchy the AST contains the following node types:

@:image(../img/document-ast.png) {
  alt = Base Traits of the Document AST
  intrinsicWidth = 872
  intrinsicHeight = 405
}

* `Element` is the base trait of the hierarchy. 
  It extends `Product` and `Serializable` and is otherwise empty.

* `Customizable` is a mixin almost every node type supports that allows to optionally associate an id and/or 
  a set of style names to a node.
  The latter can be interpreted differently as hints by various renderers, 
  in HTML they simply get rendered as class attributes for example.
  It also includes API to add or remove ids and style names polymorphically to any AST node.

* `Block` is one of the two major element types in the AST.
  Block level elements always start on a new line and in markup they often (but not always)
  require a preceding blank line.
  They can contain other block or span elements.
  It is similar to a `display: block` element in HTML.

* `Span` is the other of the two major element types in the AST.
  Span elements can start anywhere in the middle of a line or span multiple lines.
  They can contain other span elements, but no block elements.
  It is similar to a `display: inline` element in HTML.

* A `ListItem` can only occur as a child of a `ListContainer`.
  See [Lists] below for a list of available list types.

* While most other node types represent content parsed from text markup,
  a `TemplateSpan` represents a portion of a parsed template.
  
* The `Container` sub-hierarchy (explained in the next section) is not mutually exclusive to the base traits like
  `Block` or `Span`. 
  Each container is always also one of the base types, but the mapping is not fixed and therefore not expressed
  in the hierarchy (e.g. a `SpanContainer` can be a `Span` or `Block` element itself). 


### Containers

Most element types in the AST are containers.

* `Container[+T]` is the base trait for all container types. 
  It has a single property `content: T`.

* `TextContainer` is a simple `Container[String]`, a leaf node of the AST.
  See [Text Containers] below for a list of concrete types.

* `ElementContainer[+E <: Element]` is a `Container[Seq[E]]`. 
  It is the base trait for all containers which hold other AST nodes.
  
  It mixes in `ElementTraversal` which has an API that allows to select child nodes (recursively) based on a predicate
  or partial function. 

* `RewritableContainer` is the trait all containers need to mix in when they want to participate 
  in an AST transformation. 
  It has a single abstract method `rewriteChildren (rules: RewriteRules): Self`.

* `BlockContainer` is an `ElementContainer[Block]` and a `RewritableContainer`.
  See [Block Containers] below for a list of concrete types.

* `SpanContainer` is an `ElementContainer[Span]` and a `RewritableContainer`.
  See [Span Containers] below for a list of concrete types.

* `ListContainer` is an `ElementContainer[ListItem]` and a `RewritableContainer`.
  See [Lists] below for a list of concrete types.


### Special Types

The first group of types are traits that can be mixed in to a concrete type:

* `Hidden` is a node type that holds metadata or link definitions and will be ignored by renderers.

* `Unresolved` is a node type that needs to be resolved during an AST transformation,
  for example a reference to a link definition by id.
  Having such a node type in the tree after all transformations are performed and right before rendering
  is an error and will cause the transformation to fail.

* `Invalid` is a node that has been inserted by a parser or an AST transformation rule to indicate a problem.
  Depending on the configuration the presence of such a node may either cause the transformation to fail,
  or get rendered as part of the page content for visual debugging. 
  See [Error Handling] for details on how these nodes drive rendering and error reporting.
  
* `Fallback` is a node that holds an "alternative" node type representing the same content.
  When a parser or rewrite rule inserts a custom element type potentially not known by some renderers,
  it can offer a more common alternative, too.

* `SpanResolver` or `BlockResolver` nodes are special node types that know how to replace themselves during
  an AST transformation.
  Usually the node types and the rules that replace them during a transformation are decoupled.
  In cases where this is not necessary or not desired, this logic can be embedded in the node type itself.
  These traits have a `resolve` method that passes in a `DocumentCursor` that can be used to create the replacement
  node.
  It's useful for scenarios where producing the final node requires access to the configuration or other documents,
  which is not possible during the parsing phase.
  See [Cursors] below and the chapter on [AST Rewriting] for more details.

Finally there is a group of concrete types with special behaviour:

* `DocumentFragment` holds a named body element that is not supposed to be rendered with the main content.
  It may be used for populating template elements like sidebars and footers.
  See [Document Fragments] for details.

* `TargetFormat` holds content that should only be included for the specified target formats.
  All other renderers are supposed to ignore this node.
  As an example, you may want to render some content on the generated website, 
  but not in the e-books (EPUB and PDF).

* `RuntimeMessage` is a node type that contains a message associated with a level.
  The presence of any message with a level of `Error` or higher usually causes the transformation to fail,
  unless you switched to visual debugging. 
  See [Error Handling] for the relevant configuration options.
  Parser extensions, directives or rewrite rules that encounter errors can use this node type to report
  back on the issue in the exact location of the AST it occurred.


Container Elements
------------------

Since the document AST is a recursive structure most elements are either a container of other elements
or a `TextContainer`. 
The few that are neither are listed in [Other Elements].

The following lists describe the majority of the available types, only leaving out some of the more exotic options.


### Lists

There are four different list types in the core AST model. 
All types listed below have a corresponding `ListItem` container, e.g. `BulletList` nodes hold `BulletListItem`
children.

* `BulletList` represents a standard bullet list containing a definition of the bullet type alongside the list items.

* `EnumList` is an enumerated list containing a definition of the enumeration style alongside the list items.
  Supported enumeration styles are `Arabic`, `LowerRoman`, `UpperRoman`, `LowerAlpha` and `UpperAlpha`.
  reStructuredText supports all enumeration styles, Markdown only `Arabic`.

* `DefinitionList` is a list of definitions that associate terms with descriptions.

* `NavigationList` is a nested tree structure of internal or external links, 
  often auto-generated from a particular node in the document tree.


### Block Containers

* `RootElement` represents the root node of a document AST.

* `Section` represents a single section, containing a title and the block content of the section.
  This node type may contain further nested sections.

* `TitledBlock` associates a title with block content, but in contrast to a `Section` it will not 
  contribute to auto-generated navigation structures for the document. 

* `Figure` groups an image with a caption and an optional legend.

* `QuotedBlock` represent a quotation.

* `Footnote` represents a footnote that will render differently with each output format.
  In HTML footnotes will render in the location they were defined in, but can be styled differently.
  In PDF they become proper footnotes at the bottom of the page and in EPUB they become popups by default.

* `BlockSequence` is a generic sequence of blocks without any semantics attached.


### Links and References

Most links and references are also a `SpanContainer` (for the link text), but a few are not.
Span containers that are not links or references are listed in the next section.

The `Link` types differ from the `Reference` types in that the former are fully resolved, "ready to render",
whereas the latter still need to be resolved based on the surrounding content or other documents.

`Reference` nodes only appear in the AST before the AST transformation step, 
where they will be either translated to a corresponding `Link` node or an `Invalid` node in case of errors.

For details on this functionality from a markup author's perspective, see [Navigation]. 


Fully resolved `Link` nodes:

* `SpanLink` is a link associated to a sequence of spans, which may be text or images.

* `FootnoteLink` is a link to a footnote in the same document.
  The link text here is usually just a label or number, e.g. `[4]`.


`Reference` nodes that need to be resolved during AST rewriting:

* `PathReference` refers to a different document or section by a relative path, resolves to `SpanLink`.

* `LinkIdReference` refers either to the id of a URL defined elsewhere or directly to a section-id,
  resolves to `SpanLink`.

* `ImageIdReference` refers to the id of an image URL defined elsewhere, resolves to `Image`.

* `FootnoteReference` refers to a footnote in the same document, resolves to `FootnoteLink`.
  Supports special label functionality like auto-numbering.

* `ContextReference` encapsulates a reference in HOCON syntax, e.g. `${cursor.currentDocument.title}`.
  Resolves to whatever type the `Config` instance holds for the specified key.


### Span Containers

* `Paragraph` is the basic span container type. 
  Each block element without explicit markup that indicates a special type like a list or a header 
  is represented by this node type.

* `Header` is a header element with an associated level.

* `Title` represents the first header of the document.

* `CodeBlock` represents a block of code and the name of the language.
  When Laika's built-in syntax highlighter is active and recognizes this language,
  the content will be a sequence of `CodeSpan` nodes with associated categories.
  Otherwise a single `CodeSpan` node will hold the entire content.

* `Emphasized`, `Strong`, `Deleted` and `Inserted` all contain a sequence of spans with the
  corresponding associated semantics as indicated by the markup.

* `SpanSequence` is a generic sequence of spans without any semantics attached.


### Text Containers

* `Text` is the node type representing unformatted text.

* `Literal` is a literal span type.

* `LiteralBlock` is a literal block type that usually renders with whitespace preserved.

* `CodeSpan` is a span of text associated to a code category. 
  Used in a `CodeBlock` where Laika's internal syntax highlighter has been applied. 

* `Comment` is a comment in a markup document (not a comment in a code block).
  It is not a `Hidden` node as renderers for output formats that support comment syntax render this node.


Other Elements
--------------

This section lists the block and span elements that are not containers and the special elements representing templates.

### Block Elements

Most block elements are either [Span Containers] or [Block Containers], but a few are neither:

* The `Table` element represents a table structure with the help of its related types 
  `Row`, `Cell`, `TableHead`, `TableBody`, `Caption` and `Columns`.
  
  Laika supports three table types out of the box: The grid table and simple table in reStructuredText
  and the table from GitHubFlavor for Markdown.
  All three types parse to the same model structure, 
  but some of the simpler table types might only use a subset of the available node types.
  
* The `LinkDefinition` is a `Hidden` element that represents a link to an external or internal target mapped
  to an id that can be referenced by other elements.
   
  Both, the Markdown and reStructuredText parsers produce such an element, albeit with different syntax.
  In Markdown the syntax is `[id]: http://foo.com/` for example.
  
  All renderers are supposed to ignore this element and it normally gets removed during the AST transformation phase.
   
* The `PageBreak` element can be used to explicitly request a page break at the position of the node.
  Currently all renderers apart from the one for PDF ignore this element.


### Span Elements

Most span elements are either [Span Containers] or [Text Containers], but a few are neither:

* The `Image` element is a `Span` element that points to an image resource which may be internal or external.
  It supports optional size and title attributes.
   
  If you want to use an image as a block-level element you can wrap it in a `Paragraph`. 

* The `LineBreak` represents an explicit line break in inline content.


### Template Spans

Templates get parsed into an AST in a similar way as markup documents, but the model is much simpler.
The most important element types are listed below.

* A `TemplateString` is a `TextContainer` that holds a raw template element in the target format (e.g. HTML).

* A `TemplateElement` is a wrapper around a `Block` or `Span` element from a markup AST.

  This allows the use of directives or other techniques that produce markup node types inside templates,
  so that the same directive can be used in template and markup documents.

* A `TemplateSpanSequence` is a generic sequence of `TemplateSpan` elements.

* The `TemplateRoot` node is the root element of every template, similar to how `RootElement` is the root of
  all markup documents. 

* `EmbeddedRoot` is the node holding the merged content from the associated markup document.
  Usually one node of this type gets produced by every AST transformation.


AST Element Companions
----------------------

Laika includes base traits called `BlockContainerCompanion` and `SpanContainerCompanion` 
for adding companion objects that provide convenient shortcuts for creating containers.

They allow to shorten constructor invocations for simple use cases, e.g. `Paragraph(Seq(Text("hello")))`
can be changed to `Paragraph("hello")`. 
They also add an `empty` constructor and a vararg constructor for passing the content.

If you create a custom element type you can use these base traits to get all these shortcuts with a few lines:

```scala mdoc
import laika.ast._

case class MyElement(content: Seq[Block], options: Options = NoOpt) extends Block
    with BlockContainer {
  type Self = MyElement
  def withContent(newContent: Seq[Block]): MyElement = copy(content = newContent)
  def withOptions(options: Options): MyElement       = copy(options = options)
}

object MyElement extends BlockContainerCompanion {
  type ContainerType = MyElement
  override protected def createBlockContainer (blocks: Seq[Block]): ContainerType = 
    MyElement(blocks)
}
```


Document Trees
--------------

So far we dealt with `Element` types which are used to represent the content of a single markup document or template.

When a transformation of inputs from an entire directory gets processed, the content gets
assembled into a `DocumentTree`, consisting of nested `DocumentTree` nodes and leaf `Document` nodes.

### The Document Type

This is the signature of the `Document` class:

```scala:reset
import laika.ast.{Path, RootElement, Element, Config, TreePosition, DocumentStructure, TreeContent}

case class Document (
  path: Path,
  content: RootElement,
  fragments: Map[String, Element] = Map.empty,
  config: Config = Config.empty,
  position: TreePosition = TreePosition.orphan
) extends DocumentStructure with TreeContent
```

* The `path` property holds the absolute, virtual path of the document inside the tree.
  It is virtual as no content in Laika has to originate from the file system.
  See [Virtual Tree Abstraction] for details.
  
* The `content` property holds the parsed content of the markup document.
  Its type `RootElement` is one of the types listed under [Block Containers] above.
  
* The `fragment` property holds a map of fragments, which are named body elements that are not supposed
  to be rendered with the main content.
  It may be used for populating template elements like sidebars and footers.
  See [Document Fragments] for details.
  
* The `config` property holds the configuration parsed from an optional HOCON header in the markup document.
  It is an empty instance if there is no such header.
  See [Laika's HOCON API] for details.

* The `position` property represents the position of the document in a tree.
  It can be used for functionality like auto-numbering.
  It is not populated if the transformation deals with a single document only.

The `DocumentStructure` mixin provides additional methods as shortcuts for selecting content from the document:

* `title: Option[SpanSequence]` provides the title of the document, either from the configuration header or
  the first header in the markup. It is empty when none of the two are defined.
  
* `sections: Seq[SectionInfo]` provides the hierarchical structure of sections, obtained from the headers
  inside the document and their assigned levels.
  

### The DocumentTree Type

In a multi-input transformation all `Document` instances get assembled 
into a recursive structure of `DocumentTree` instances.
  
* Like with documents, the `path: Path` property holds the absolute, virtual path of the document inside the tree.
  For the root tree it will be `Root`.
  
* The `content: Seq[TreeContent]` property holds the child trees and documents of this tree.
  Its type `TreeContent` is a trait implemented by both `Document` and `DocumentTree`.
  
* The `titleDocument: Option[TitleDocument]` property holds an optional title document for this tree.
  A tree often represents a logical structure like a chapter.
  It is used as a section headline in auto-generated site navigation 
  as well as the first content of this tree in linearized e-book output (EPUB or PDF).
  See [Title Documents] for details.
  
* The `templates: Seq[TemplateDocument]` property holds the AST of all templates parsed inside this tree
  (excluding templates from child trees).
  The AST nodes it can hold are described in [Template Spans] above.
  See [Creating Templates] for more details on the template engine.
  
* The `config: Config` property holds the configuration parsed from an optional HOCON document named `directory.conf`.
  It is an empty instance if there is no file.
  It can also be populated programmatically in cases where the content is generated and not loaded from the file system.
  See [Laika's HOCON API] for details.

* The `position: TreePosition` property represents the position of the document in a tree.
  It can be used for functionality like auto-numbering.
  It is not populated if the transformation deals with a single document only.
  
The API provides additional methods as shortcuts for selecting content from the tree:

* `selectSubtree`, `selectTemplate` and `selectDocument` all accept a `RelativePath` argument to select
  content from the tree structure, including content from sub-trees.
  
* `allDocuments: Seq[Document]` selects all documents contained in this tree, fetched recursively, depth-first.
  This is in contrast to the `content` property which selects documents and sub-trees on the current level.
 
* The `runtimeMessage` method selects any `RuntimeMessage` matching the specified filter.
  Useful for reporting errors and warnings in the parsed result.
  

Cursors
-------

During an AST transformation the rewrite rule might require access to more than just the AST node passed to it.
When it is a rule for resolving references or creating a table of contents, it needs access to the AST of other
documents, too.

The `Cursor` type provides this access, but in contrast to the recursive `DocumentTree` which is a classic tree
structure, it represents the tree from the perspective of the current document, with methods to navigate to
parents, siblings or the root tree.

An instance of `DocumentCursor` is passed to rewrite rules for AST transformations (see [AST Rewriting], 
and to directive implementations that request access to it (see [Implementing Directives]).

Let's look at some of its properties:

* `target: Document` represents the document this cursor points to.

* `parent: TreeCursor` represents the parent tree this document belongs to.
  A `TreeCursor` is similar to a `DocumentCursor`, just that its target is a `DocumentTree` instead. 

* `root: RootCursor` represents the root of the entire tree.

* `previousDocument: Option[DocumentCursor]` and `nextDocument: Option[DocumentCursor]` represent its siblings.
  For a cursor pointing to the first document in a sub-tree (chapter), `previousDocument` will be empty, 
  for the last document `nextDocument` will be empty.
  
* `flattenedSiblings.previousDocument` and `flattenedSiblings.nextDocument` are alternatives that look beyond
  the current sub-tree and flatten the hierarchy into a single list in depth-first traversal.


AST Transformation Phases
-------------------------

In most transformations the AST moves through three different phases between parsing and rendering:

1) The first shape will be the AST produced by the parsers for text markup or templates.
   Since parsers do not have access to the surrounding nodes or the configuration,
   some parsers for elements like links or navigation structures need to insert temporary node types.
   
2) After parsing of all participating documents and templates completes,
   the first AST transformation is performed.
   It resolves links, variables, footnotes, builds the document's section structure or generates a table of contents.
   These transformations are defined in rewrite rules.
   The library contains a basic set of rules internally for linking and navigation,
   but users can provide additional, custom rules.
   See [AST Rewriting] for details.
   
3) Finally the resolved AST representing the markup document is applied to the AST of the template.
   It is merely the insertion of one AST at a particular node in another AST.
   
The result obtained from step 3 is then passed to renderers.
When rendering the same content to multiple output formats, the steps 1 and 2 are always only executed once.
Only step 3 has to be repeated for each output format, as each format comes with its own templates.
