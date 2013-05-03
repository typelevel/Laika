
Document Tree Rewriting
=======================     
  
In a similar way like the API allows to adjust the rendering for individual
node types, it also allows to rewrite the model itself, transforming parts
of the tree to create a new tree model.

A tree model is immutable, so rewriting always creates a new tree, while
reusing unmodified branches for efficiency.

There are two ways to perform such a rewriting:

* Through the `ElementTraversal.rewrite` method directly on the tree model instance itself.
  The `Document` root element as well as all other container types mix in this trait.

* Through the Transform API

The latter is convenient if you otherwise have no need to separate the parse and 
render operation and just want to add a transformation before rendering.

The purpose of this facility is to provide a way to support basic tree rewriting
needs without introducing additional dependencies. If you have more sophisticated
requirements, you can alternatively use [Kiama], a language processing library that
also supports rewriting. According to their documentation Kiama should be compatible
with Laika as they support tree models consisting of `Product` instances
(which all case classes are). 


Using the rewrite Method
------------------------

Often this method gets called on the `Document` root element, but other container
types support it, too, so you can also call it on elements like `Paragraph` or `ListItem`.

This is the signature of the rewrite method:

    def rewrite (rule: PartialFunction[Element,Option[Element]]): Self

Self is a type parameter which is always the node class that mixes in the `ElementTraversal`
trait, meaning you always get back an instance of the same class you call the method on. 
The rule is a partial function that expects an `Element` and returns an `Option[Element]`.
The rules are as follows:

* If the function is not defined for a particular element the old element is kept in the tree.

* If the function returns `Some(Element)` this element is used in place of the old one.

* If the function returns `None` the old element is removed from the tree.

* Processing happens depth-first (bottom-up), so all nodes getting passed to this function
  already had their children getting processed.

* The tree is immutable, so new instances are returned when rewriting, but unmodified
  branches are reused.
  
* Therefore, if the rule does not affect any node, the method will simply return `this`. 

For a very simple example, let's assume you want to "downgrade" all `Strong` nodes in 
the text to `Emphasized` nodes:

    val doc: Document = ...
    
    val newDoc = doc rewrite {
      case Strong(content, options) => Some(Emphasized(content, options))
    }

For a slightly more advanced example, let's assume you only want to downgrade `Strong`
nodes inside headers. To accomplish this you need to nest a rewrite operation
inside another one:

    val newDoc = doc rewrite {
      case h: Header => Some(h rewrite {
        case Strong(content, options) => Some(Emphasized(content, options))
      })
    }


Using the Transform API
-----------------------

The examples in the previous example show how to rewrite the tree if you have
access to a `Document` or other container element instance. When all you want
to do is to perform a full transformation from some input text to some output
format, this would require to split the parse and render operations to get hold
of the `Document` instance. Therefore the Transform API offers a hook to do
this in one go, as a step in the transformation process.

The rules and principles for defining a rewrite rule are the same as for using
the `rewrite` method directly, as described in the previous section.

To use the same example, this is how you can replace all `Strong` nodes with
`Emphasized` nodes:

    Transform from Markdown to HTML usingRule {
      case Strong(content, options) => Some(Emphasized(content, options))
    } fromFile "hello.md" toFile "hello.html"



[Kiama]: http://code.google.com/p/kiama/wiki/UserManual
  