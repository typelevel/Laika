
Extending reStructuredText
==========================

This chapter describes the API for defining a custom directive or text role
that is compatible with the reStructuredText specification.


Comparison with Laika Directives
-------------------------------- 

Extensions defined in the way described in this chapter could still be used 
when parsing the markup documents with a different reStructuredText implementation,
as they are fully compatible with the original specification.

If this is not a requirement you may alternatively use the Laika variant
of directives. This would give you the following advantages:

* The syntax definition is simpler, while offering the same flexibility
* The directive may be used in other parsers, too, like in the Markdown parser
* The directive may also be used in templates

For details on these alternative directive types see 
[Directives][directive.md:Directives].


Feature Overview
----------------

In contrast to Markdown reStructuredText has been designed to be extensible.
The way these extension mechanisms are defined means that in most cases there
is no need to provide custom low-level parser logic, but instead use the Laika
API to specify the expected format based on existing building blocks the parser
already knows how to deal with and then provide one or more functions to convert 
and validate the parsed result and provide a tree element to be inserted into the document
as a block or span element.

The following types of extension points exist:

* Block Directives - an extension hook for adding new block level elements to
  reStructuredText markup. 
  For details see the [specification entry for directives][rst directives]. 

* Substitution Definitions - an extension hook for adding new span level elements to
  reStructuredText markup that can be used by substitution references (like ``|replaceMe|``). 
  For details see the [specification entry for substitution definitions][rst substitutions]. 
 
* Interpreted Text Roles - an extension hook for adding new dynamic span level elements to
  reStructuredText markup. In contrast to substitution definitions the implementation of a text
  role uses the text from the occurrences in the markup referring to the role as input.
  For details see the [specification entry for interpreted text roles][rst text roles]. 

[rst directives]:    http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#directives
[rst substitutions]: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-definitions
[rst text roles]:    http://docutils.sourceforge.net/docs/ref/rst/directives.html#custom-interpreted-text-roles

The Laika APIs for all of these extension types are explained in the sections below.

The design of these APIs did not aim to mimic the API of the original Python reference implementation.
Instead the goal was to create an API that is idiomatic Scala, fully typesafe and as concise as possible.
Yet it should be flexible enough to semantically support the options of the Python directives and text roles, 
so that ideally most existing Python directives could theoretically get ported to Laika.

The style of the API is somewhat similar to the [Play JSON API] and its combinators. 
It is inspired by the concepts outlined by Sadek Drobi in his 
[document about functional builders][func-builders].


[Play JSON API]: http://www.playframework.com/documentation/2.1.1/ScalaJsonCombinators

[func-builders]: https://gist.github.com/sadache/3646092


Implementing a Directive
------------------------

A directive can be used to introduce new syntax for custom block or span elements.
Entry points are the `BlockDirective` and `SpanDirective` objects. The Python reference parser does
not make this distinction on the API level, but does this internally based on the context a 
directive is parsed in. Since Laika APIs are typesafe, the distinction is necessary since
block level and span level directives create different types of document tree nodes.
A `SpanDirective` can only be used in a substitution definition which can then be used
within flow elements. A `BlockDirective` can be used directly in any location other block
level content like paragraphs or lists can be used.
 
A directive may consist of any combination of arguments, fields and body elements:
 
    .. myDirective:: arg1 arg2
      :field1: value1
      :field2: value2
 
      This is the body of the directive. It may consist of any standard or 
      custom block-level and inline markup.
 
  
In the example above `arg1` and `arg2` are arguments, `field1` and `field2` are fields,
and followed by body elements after a blank line. If there are no arguments or fields
the blank line may be omitted.
 
For each of these directive elements, the API offers a method to specify whether the
element is required or optional, and an optional function to convert or validate the
parsed value.
 
 
### Basic Example

Consider the following simple example of a directive with just one argument and
a body:
 
    .. note:: This is the title
   
       This is the body of the note.
 
 
The implementation of this directive could look like this:
 
    case class Note (title: String, 
                     content: Seq[Block], 
                     options: Options = NoOpt) extends Block 
                                               with BlockContainer[Note]
 
    val rst = ReStructuredText withBlockDirectives
      BlockDirective("note") {
        (argument(withWS = true) ~ blockContent)(Note(_,_))
      }
 
    Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"
 
 
The `argument()` method specifies a required argument of type `String` (since no conversion
function was supplied). We need to set the `withWS` flag to true as an argument cannot have
whitespace per default. The `blockContent` method specifies standard block content (any block-level
elements that are supported in normal blocks, too) which results in a parsed value of type
`Seq[Block]`. Finally you need to provide a function that accepts the results of the specified
directive elements as parameters (of the corresponding type). Here we created a case class
with a matching signature (ignoring the optional third argument). For a block directive
the final result has to be of type `Block` which the `Note` class satisfies. Finally the directive 
gets registered with the `ReStructuredText` parser.
 
 
### Adding Converters and Validators 
 
If any conversion or validation is required on the individual parts of the directive they can
be passed to the corresponding function:

    def nonNegativeInt (value: String) =
      try {
        val num = value.toInt
        Either.cond(num >= 0, num, "not a positive int: " + num)
      }
      catch {
        case e: NumberFormatException => Left("not a number: " + value)
      }
 
    case class Message (severity: Int, 
                        content: Seq[Block],
                        options: Options = NoOpt) extends Block 
                                                  with BlockContainer[Message]
 
    val rst = ReStructuredText withBlockDirectives
      BlockDirective("message") {
        (argument(nonNegativeInt) ~ blockContent)(Message(_,_))
      }


The function has to provide an `Either[String, T]` as a result. A `Left` result will be interpreted
as an error by the parser with the string being used as the message and an instance of `InvalidBlock`
containing the validator message and the raw source of the directive will be inserted into the document
tree. In this case the final function (`Message`) will never be invoked. A `Right` result will be
used as an argument to the final function. Note how this case class now expects an `Int` as the first
parameter.
 
 
### Optional Elements

Finally arguments and fields can also be optional. In case they are missing, the directive is still
considered valid and `None` will be passed to your function:
 
    case class Message (severity: Option[Int], 
                        content: Seq[Block],
                        options: Options = NoOpt) extends Block 
                                                  with BlockContainer[Message]
  
    val rst = ReStructuredText withBlockDirectives
      BlockDirective("message") {
        (optArgument(nonNegativeInt) ~ blockContent)(Message(_,_))
      }
    
 
The argument may be missing, but if it is present it has to pass the specified validator.
 
In case of multiple arguments, the order you specify them is also the order in which they
are parsed from the directive markup, with the only exception being that required arguments
will always be parsed before optional ones, and arguments with whitespace need to come last.



Implementing a Text Role
------------------------

Text roles are the extension mechanism for inline elements of reStructuredText.
For details see the [specification entry for interpreted text roles][rst text roles].

Entry point for creating a new role is the `TextRole` object. It allows to specify the following
aspects that define a text role:
 
* The name with which it can be referred to by both, a span of interpreted text and a role
  directive to further customize it.
 
* The default value, that should get passed to the role function in case it is used
  directly in interpreted text without customization through a role directive.
 
* The role directive that specifies how the role can be customized. The options
  for role directives are almost identical to regular directives, the only difference
  being that role directives do not support arguments, only fields and body elements.
 
* The actual role function. It gets invoked for each occurrence of interpreted text
  that refers to this role, either directly by name or to the name of a role directive
  that customized this role. The first argument is either the default value
  or the result of the role directive, the second is the actual text of the interpreted 
  text span. The return value of the role function is the actual `Span` instance
  that the original interpreted text should be replaced with.
 
A role directive may consist of any combination of fields and body elements:
 
    .. role:: ticket(link)
      :base-url: http://www.company.com/tickets/
 
  
In the example above `ticket` is the name of the customized role, `link` the name
of the base role and `base-url` the value that overrides the default defined in the
base role.
 
Before such a role directive can be used, an implementation has to be provided
for the base role with the name `link`. For more details on implementing directives
see the previous section.
 
The implementation of the `link` text role could look like this:
 
    val rst = ReStructuredText withTextRoles (
      TextRole("link", "http://www.company.com/main/")(field("base-url")) {
        (base, text) => Link(List(Text(text)), base + text)
      }
    )
   
    Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"   

 
We specify the name of the role to be `link`, and the default value the URL provided as the
second argument. The second parameter list specifies the role directive implementation,
in this case only consisting of a call to `field("base-url")` which specifies a required 
field of type `String` (since no conversion function was supplied). The type of the result
of the directive has to match the type of the default value.
Finally the role function is defined that accepts two arguments. The first is the base
url, either the default in case the base role is used directly, or the value specified
with the `base-url` field in a customized role. The second is the actual text from the
interpreted text span. In this case we use these values to create an instance of `Link`,
a tree node from the default document tree. Finally the directive gets registered with 
the `ReStructuredText` parser.
  
If you need to define more fields or body content they can be added with the `~` combinator
just like with normal directives. Likewise you can specify validators and converters for 
fields and body values like documented above.
 
Our example role can then be used in the following ways:
 
Using the base role directly:
   
    For details read our :link:`documentation`.
  
This would result in the following HTML:
 
    For details read our <a href="http://www.company.com/main/documentation">documentation</a>.

 
Using the customized role called `ticket`: 
 
    For details see ticket :ticket:`344`.
 
This would result in the following HTML:
 
    For details see ticket <a href="http://www.company.com/ticket/344">344</a>.
   


Registering Extensions with the sbt Plugin
------------------------------------------

All previous examples showed how to register directives or text roles with the Laika API.
This section shows how to register the same elements with the sbt plugin.

Block Directives:

    val directive = BlockDirective("name") {
      // implementation producing a `Block` element
    }
    
    rstBlockDirectives += directive
    
    
Span Directives:

    val directive = SpanDirective("name") {
      // implementation producing a `Span` element
    }
    
    rstSpanDirectives += directive


Text Roles:

    val role = TextRole("name", "default") {
      // implementation producing a `Span` element
    }
    
    rstTextRoles += role
    
