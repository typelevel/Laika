
Document Tree Rewriting
=======================     

The document tree in a Laika transformation is a generic representation
of the document that does not contain any specific semantic or technical coupling to a concrete
input or output format. This allows to add custom processing logic only
operating on the tree itself, so it can be used with all supported input and output
formats unchanged.

Rewriting a tree means traversing it and replacing or removing some of its nodes.
A tree model is immutable, so rewriting always creates a new tree, while
reusing unmodified branches for efficiency.

The purpose of this facility is to provide a way to support basic tree rewriting
needs without introducing additional dependencies. If you have more sophisticated
requirements, you can alternatively use [Kiama], a language processing library that
also supports rewriting. According to their documentation Kiama should be compatible
with Laika as they support tree models consisting of `Product` instances
(which all case classes are). 



How Rewrite Rules Work
----------------------

A rewrite rule has the type `PartialFunction[Element,Option[Element]]`. Laika offers
a type alias `RewriteRule` for convenience.

The partial function expects an `Element` and returns an `Option[Element]`.
The rules are as follows:

* If the function is not defined for a particular element the old element is kept in the tree.

* If the function returns `Some(Element)` this element is used in place of the old one.

* If the function returns `None` the old element is removed from the tree.

* Processing happens depth-first (bottom-up), so all nodes getting passed to this function
  already had their children getting processed.

* The tree is immutable, so new instances are returned when rewriting, but unmodified
  branches are reused.
  
* Therefore, if the rule does not affect any child node, the rule will simply return
  the old root node. 

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

    import LaikaKeys._
    import laika.tree.Elements._
    
    // ... your standard build stuff
    
    LaikaPlugin.defaults
    
    rewriteRules in Laika += rewriteRule { 
      case Emphasized(content, opts) => Some(Strong(content, opts))
    }


### Using the Transform API

When using Laika embedded and all you want to do is to perform a full transformation 
from some input text to some output format, the Transform API offers a hook to do this 
in one go, as a step in the transformation process.

Again we replace all `Emphasized` nodes with `Strong` nodes:

    Transform from ReStructuredText to HTML usingRule {
      case Emphasized(content, opts) => Some(Strong(content, opts))
    } fromFile "hello.rst" toFile "hello.html"


### Working with the Tree Model

The final option is for splitting the parse and render operations
and working with the tree model directly between these operations.
For obtaining a document tree see [Separate Parsing and Rendering].

Often a rule gets applied to the whole `Document` instance, but other container
types support rewriting, too, so you can also apply it to elements like `Paragraph` or `ListItem`.

Once again we are turning all `Emphasized` nodes in the text to `Strong` nodes:

    val doc: Document = ... // obtained through the Parse API
    
    val newDoc = doc rewrite {
      case Emphasized(content, opts) => Some(Strong(content, opts))
    }

For a slightly more advanced example, let's assume you only want to replace `Emphasized`
nodes inside headers. To accomplish this you need to nest a rewrite operation
inside another one:

    val newDoc = doc rewrite {
      case h: Header => Some(h rewrite {
        case Emphasized(content, opts) => Some(Strong(content, opts))
      })
    }


[Kiama]: http://code.google.com/p/kiama/wiki/UserManual
  