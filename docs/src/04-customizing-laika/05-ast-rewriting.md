
AST Rewriting
=============

The document tree in a Laika transformation is a generic representation of the document 
that does not contain any specific semantic or technical coupling to a concrete input or output format. 
This allows to add custom processing logic only operating on the document structure itself, 
so it can be used with all supported input and output formats unchanged.

Rewriting a tree means traversing it and replacing or removing some of its nodes.
A tree model is immutable, so rewriting always creates a new tree, 
while reusing unmodified branches for efficiency.

Several of Laika's built-in features are implemented as rewrite rules internally,
like the mechanism for resolving internal references or footnote auto-numbering.
But it also designed to be a hook for processing on the application level.


How Rewrite Rules Work
----------------------

A rewrite rule has the type `PartialFunction[T, RewriteAction[T]]`. 
Laika offers a type alias `RewriteRule[T]` for convenience.

The rules are as follows:

* If the function is not defined for a particular element or it returns `Retain` the old element is kept in the tree.

* If the function returns `Replace(newElement)` this element is used in place of the old one.

* If the function returns `Remove` the old element is removed from the tree.

* Processing happens depth-first (bottom-up), 
  so all nodes getting passed to this function already had their children getting processed.

* The tree is immutable, therefore new instances are returned when rewriting.
  
The following sections show the three ways to apply such a rule.


Applying a Rewrite Rule
-----------------------

The mechanism is slightly different, depending on whether you are using the sbt
plugin or Laika embedded in an application. In the latter case you have two
choices, one for hooking into a full transformation, the other for operating
on nodes obtained by a separate parse operation. All three options are described below.


### Using the sbt Plugin

The following example of an sbt build file shows how to turn each `Emphasized` node
into a `Strong` node while processing everything else with default rules:

```scala
import laika.ast._

laikaExtensions += laikaSpanRewriteRule { 
  case Emphasized(content, opts) => Replace(Strong(content, opts))
}
```


### Using the Transformer API

When using the library API and all you want to do is to perform a full transformation 
from some input text to some output format, 
the Transformer API offers a hook to do this in one go, as a step in the transformation process.

Again we replace all `Emphasized` nodes with `Strong` nodes:

```scala
val transformer = Transformer
  .from(ReStructuredText)
  .to(HTML)
  .usingSpanRule {
    case Emphasized(content, opts) => Replace(Strong(content, opts))
  }.build
```


### Working with the Tree Model

The final option is to use the `rewrite` method on individual documents or AST nodes.

The types `DocumentTree` and `Document` come with a `rewrite` method as well as most of the node types 
that can occur within a document (all nodes that mix in `RewritableContainer`).

Obtaining `Document` instances is usually achieved by splitting the parsing and rendering operations
instead of using a full transformer. This is described in detail in [Separate Parsing and Rendering].

Once again we are turning all `Emphasized` nodes in the text to `Strong` nodes for our example:

```scala
val doc: Document = ??? // obtained through the Parser API

val newDoc = doc.rewrite(RewriteRules.forSpans {
  case Emphasized(content, opts) => Replace(Strong(content, opts))
})
```

For a slightly more advanced example, let's assume you only want to replace `Emphasized` nodes inside headers. 
To accomplish this you need to nest a rewrite operation inside another one:

```scala
val newDoc = doc.rewrite(RewriteRules.forBlocks {
  case h: Header => Replace(h.rewriteSpans {
    case Emphasized(content, opts) => Replace(Strong(content, opts))
  })
})
```
