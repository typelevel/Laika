
Supported Markup
================

The current release supports Markdown and reStructuredText. Both implementations stay as close
as possible to the original descriptions and specifications of these formats. The markup
gets parsed into a generic document tree model shared between all parsers. This means
that there are some adjustments in terminology (e.g. a bullet list in reStructuredText
is called an "unordered list" in the Markdown syntax description). It also means that
there are some tree element types which are not produced by the Markdown parser, like
table, footnotes or citations, as reStructuredText is much more feature-rich than
the original Markdown syntax. The unified model allows to create renderers that
do not have to deal with any of the specifics of an individual markup syntax.



Markdown
--------

Laika supports Markdown primarily due to its popularity.
It has several advantages, mainly that it is very easy to learn, very lightweight, and produces
documents which are fairly easy to read, even when you don't know anything about Markdown.

However, Markdown also has a few issues. First, there is no real specification,
only a page with a syntax description that leaves many questions unanswered. Secondly, its feature
set is fairly limited, lacking functionality for more advanced usage like technical 
documentation or entire books. As a consequence
extensions have been added to almost every Markdown implementation without much agreement between 
them. Finally, the Markdown syntax itself tries to circumvent the reduced set of features through
recommending the embedding of verbatim HTML elements for any formatting needs not supported by Markdown, 
which is not the best approach for a library like Laika that aims to support other output formats
than just HTML.


### Getting Started

Even if you do not know much about Markdown formally, it is very likely that you have already used it.
This document is not going to provide a syntax overview and refers to the official documentation instead.
It will primarily focus on anything specific to the way Laika handles Markdown.

To get an overview over Markdown syntax, these documents may be used:

* For a description of the syntax see the [official syntax documentation][markdown docs].

* For trying out small snippets of Markdown and checking how the various existing Markdown implementations
  handle them, the [Babelmark] tool can be used.  

* For using Markdown in Laika, the [Transformation Basics] page should answer most of your questions.

* For the special treatment of verbatim HTML in Laika, see the following section.

Laika tries to follow the official syntax documentation. In cases where it is silent on how
to handle certain edge cases, the Babelmark tool has been consulted and usually the approach
the majority of available parses have chosen has been picked for Laika, too. There is currently only
one (known) minor exception: 

* Laika does not detect a header if it is not preceded by a blank line. Supporting this would be
  disadvantageous for three reasons: it goes against Markdown's design goal of promoting
  readability, it would slow down the parser and it would open the doors for accidental headers.
  According to Babelmark, there are at least two parsers (Pandoc, Python-Markdown) that agree.
   

[markdown docs]: http://daringfireball.net/projects/markdown/syntax

[Babelmark]: http://johnmacfarlane.net/babelmark2/

[Transformation Basics]: basics.html


### Verbatim HTML

Finally there is one major difference to standard Markdown: the parsing of verbatim HTML elements
is not enabled by default, but it can be switched on if required. This decision is based on the 
following problems with verbatim HTML:

* It goes against Markdown's own design goal of great readability and being easy for anyone
  to pick up. Markdown syntax basics can be understood in minutes, by non-technical people, 
  but producing well-formed and valid HTML is not trivial if you do not regularly deal with HTML.
  
* It goes against Laika's design goal of decoupling parsers and renderers. 
  Markdown was designed with only HTML output in mind, so this mismatch is natural.
  
* It would not be safe to use without additional filtering when used in web applications
  that offer user input. 
  
This design decision has the following consequences for the Laika library:

* The Markdown parser, by default, treats HTML tags as normal text input and converts
  tags to HTML entities when rendering, so that they would show up as part of the
  rendered text nodes in the final HTML document. How to enable HTML parsing is shown 
  further below.
  
* The elements produced by the Markdown HTML parsers are not part of the standard
  Laika document tree model. Instead they are kept in a Markdown-specific trait `VerbatimHTMLElements`.
  
* As a consequence, built-in renderers like that for HTML do not know these nodes. How to add
  the required renderer extensions will be shown below, too.
  
This all sounds more complicated than it actually is. To enable verbatim HTML elements
you have to change this standard expression:

    Transform from Markdown to HTML
    
to

    Transform from (Markdown withVerbatimHTML) to (HTML using VerbatimHTML)
    
This installs both, the required parser and renderer extensions.

But, as explained above, when using this in a web application it is strongly recommended
to add whitelist filtering. Per default Laika renders all HTML tags out as is, including
orphaned opening and closing tags (without matching tag) and tags like `<script>`.
Whitelist functionality can be quickly added with a few lines of code using
[Document Tree Rewriting].

[Document Tree Rewriting]: tree-rewriting.html    
   
    
    
reStructuredText
----------------

The reStructuredText project is part of Python's Docutils project. It is considerably more feature-rich
than Markdown, with support for tables, citations and footnotes. It is also extensible through
directives and text roles, allowing the creation of custom block and span types without touching the
original parser. Finally it is more strictly defined than Markdown, with a detailed specification
and clearly defined markup recognition rules.

On the other hand, adoption is not nearly as high as for Markdown, and some of the syntax is more
verbose and less intuitive or legible than Markdown.

  
  
### Getting Started

This document is not going to provide a syntax overview and refers to the official documentation instead.

To get started with reStructuredText, these resources may be used:

* The [official markup specification][rst spec].

* An [online tool][rst tool] for trying out small snippets of reStructuredText.  

* [Transformation Basics] for using reStructuredText in Laika.

* The following sections below for an overview on how implement extensions for reStructuredText


[rst home]: http://docutils.sourceforge.net/rst.html
[rst spec]: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
[rst tool]: http://www.tele3.cz/jbar/rest/rest.html  



### Extending reStructuredText

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



### Implementing a Directive

The API for directive setup did not aim to mimic the API of the original Python reference implementation.
Instead the goal was to create an API that is idiomatic Scala, fully typesafe and as concise as possible.
Yet it should be flexible enough to semantically support the options of the Python directives, so that
ideally most existing Python directives could theoretically get ported to Laika.
 
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
 
    This is the body of the directive. It may consist of any standard or custom
    block-level and inline markup.
 
  
In the example above `arg1` and `arg2` are arguments, `field1` and `field2` are fields,
and followed by body elements after a blank line. If there are no arguments or fields
the blank line may be omitted.
 
For each of these directive elements, the API offers a method to specify whether the
element is required or optional, and an optional function to convert or validate the
parsed value.
 
Consider the following simple example of a directive with just one argument and
a body:
 
  .. note:: This is the title
   
     This is the body of the note.
 
 
The implementation of this directive could look like this:
 
  case class Note (title: String, content: Seq[Block]) extends Block with BlockContainer[Note]
 
  val rst = ReStructuredText withBlockDirectives (
    BlockDirective("note") {
      (argument(withWS = true) ~ blockContent)(Note)
    }
  )                                              
 
  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"
 
 
The `argument()` method specifies a required argument of type `String` (since no conversion
function was supplied). We need to set the `withWS` flag to true as an argument cannot have
whitespace per default. The `blockContent` method specifies standard block content (any block-level
elements that are supported in normal blocks, too) which results in a parsed value of type
`Seq[Block]`. Finally you need to provide a function that accepts the results of the specified
directive elements as parameters (of the corresponding type). Here we created a case class
with a matching signature so can pass it directly as the target function. For a block directive
the final result has to be of type `Block` which the `Note` class satisfies. Finally the directive 
gets registered with the `ReStructuredText` parser.
 
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
 
  case class Message (severity: Int, content: Seq[Block]) 
                                     extends Block with BlockContainer[Message]
 
  val rst = ReStructuredText withBlockDirectives (
    BlockDirective("message") {
      (argument(nonNegativeInt) ~ blockContent)(Message)
    }
  )    
 
The function has to provide an `Either[String, T]` as a result. A `Left` result will be interpreted
as an error by the parser with the string being used as the message and an instance of `InvalidBlock`
containing the validator message and the raw source of the directive will be inserted into the document
tree. In this case the final function (`Message`) will never be invoked. A `Right` result will be
used as an argument to the final function. Note how this case class now expects an `Int` as the first
parameter.
 
Finally arguments and fields can also be optional. In case they are missing, the directive is still
considered valid and `None` will be passed to your function:
 
  case class Message (severity: Option[Int], content: Seq[Block]) 
                                extends Block with BlockContainer[Message]
  
  val rst = ReStructuredText withBlockDirectives (
    BlockDirective("message") {
      (optArgument(nonNegativeInt) ~ blockContent)(Message)
    }
  )    
 
The argument may be missing, but if it is present it has to pass the specified validator.
 
In case of multiple arguments, the order you specify them is also the order in which they
are parsed from the directive markup, with the only exception being that required arguments
will always be parsed before optional ones, and arguments with whitespace need to come last.



### Implementing a Text Role

Text roles are the extension mechanism for inline elements of reStructuredText.
For details see the [specification entry for interpreted text roles][rst text roles].

Like with the API for directive setup this API did not aim to mimic the API
of the original Python reference implementation, but instead to provide
the same functionality with a concise, type-safe API. 

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
   



  