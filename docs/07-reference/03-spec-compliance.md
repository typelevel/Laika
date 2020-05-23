
Spec Compliance
===============

This chapter gives a brief overview on how closely Laika adheres to the various formats it parses.
 

Markdown
--------

Laika sticks to the original [syntax description][markdown docs] and incorporates the [test suite]
from the PHP Markdown project which is a slightly expanded version of the original suite from John Gruber.

In cases where both the spec and the test suite are silent on how to handle certain edge cases, 
the [Babelmark] tool has been consulted and usually the approach the majority of available parsers have chosen 
has been picked for Laika, too.


### Test Suite

The testing approach in Laika is adapted to cater for the library's design goals, 
one of which is decoupling input and output formats.
Most existing, official test suites are providing inputs in text markup and output in HTML.
In cases where the output differs from Laika's built-in HTML renderer, the renderer is adjusted with
overrides just for the test.
This is acceptable because these subtle differences do not represent a semantic difference.
The only way to avoid these kind of "little cheats" would be to have a separate HTML renderer
for each supported text markup format. 
But this would be undesirable, in particular in cases where users mix documents with different markup formats
in the same input directory.

[test suite]: https://github.com/michelf/mdtest   

[markdown docs]: http://daringfireball.net/projects/markdown/syntax

[Babelmark]: http://johnmacfarlane.net/babelmark2/


### Verbatim HTML

One major difference to standard Markdown is that the parsing of verbatim HTML elements is not enabled by default, 
as Laika discourages the coupling of input and output formats, but it can be switched on if required. 

See [Raw Content] for examples on how to enable verbatim HTML in the sbt plugin or the library API.

When this support is switched on, it follows the original spec, including the support for text markup and HTML
syntax being interspersed in the input. 


### GitHub Flavored Markdown

Laika supports the syntax of GitHubFlavored Markdown through an `ExtensionBundle` that must be enabled explicitly.
These are the parsers this extension adds to standard Markdown:

* strikethrough ([spec][gfm strike])
* auto-links (urls and email addresses - [spec][gfm autolinks])
* fenced code blocks ([spec][gfm fences])
* tables ([spec][gfm tables])

[gfm strike]:    https://github.github.com/gfm/#strikethrough-extension-
[gfm autolinks]: https://github.github.com/gfm/#autolinks-extension-
[gfm fences]:    https://github.github.com/gfm/#fenced-code-blocks
[gfm tables]:    https://github.github.com/gfm/#tables-extension-


**Subtle Differences to the GitHub Specification**

* **Spec Alignment**: The Laika implementation is an extension of classic, standard Markdown, in a similar fashion as 
  GitHub Flavored Markdown had initially been defined. 
  However, GitHub's spec has since moved on and is now based on the [CommonMark] spec.
  This should not make a huge difference for the most common use cases as CommonMark stays pretty 
  close to classic Markdown and the syntax that has since moved to CommonMark (e.g. fenced code blocks) is included
  in Laika's extension.
  You'll probably only notice differences around any of the subtle lower-level ambiguities in Markdown's syntax.

* **Auto-Links**: The parsing of URIs in auto-links is based on the relevant RFCs and not on the rather informal 
  description in the GitHub spec. 
  This should not make any difference for the most common use cases. 
  The RFC based URI parser has been part of Laika for years (as reStructuredText natively supports auto-links)
  and its higher level of correctness justifies the bypassing of the informal description of GitHubs spec.
  
* **Fenced Code Blocks** need a preceding blank line to be recognized by Laika's parser for now. 
  This is due to temporary technical limitations which will be lifted before the 1.0 release. 
  It will require additional hooks for how extension parsers register with the host language 
  to allow to specify a line test that causes interruption of a paragraph.
  
* **Tables**: Since Laika is a tool that uses an internal AST that abstracts away the features of a specific output 
  format, it does not follow the exact syntax for HTML output as shown in the GitHub spec. 
  Specifically it does not render table cells using the deprecated `align` attributes. 
  Instead it renders the cells with classes (`alignLeft`, `alignCenter`, `alignRight` or none)
  so that the cells can get styled in CSS.


### CommonMark

Laika does not yet integrate the official CommonMark test suite.
This step is planned for a release shortly after the final 1.0 version is reached.

In practice the differences should be minor as CommonMark is a specification that builds on top of the original
Markdown spec plus some aspects of GitHub Flavor which Laika both supports. 
It mostly removes some ambiguity and adds detail to some of the under-specified features of classic Markdown.


reStructuredText
----------------

The reStructuredText project is part of Python's Docutils project. 
It is also more strictly defined than Markdown, with a detailed [specification][rst spec] 
and clearly defined markup recognition rules.

Apparently there is no official test suite for reStructuredText, therefore to add a realistic
test to the Laika test suite a full transformation of the reStructuredText specification itself
is integrated into Laika's test suite. 

[rst spec]: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html


### Supported Standard Directives

Directives are an extension mechanism of reStructuredText and the reference implementation supports a set
of standard [standard directives][std directives] out of the box.

[std directives]: http://docutils.sourceforge.net/docs/ref/rst/directives.html

Out of this set Laika supports the following:

 * Admonitions: `attention`, `caution`, `danger`, `error`, `hint`, `important`,
   `note`, `tip`, `warning` and the generic `admonition`
 * Document Structure: `contents`, `sectnum`, `header`, `footer`, `title`, `include`
 * Containers: `compound`, `container`, `topic`, `sidebar`, `rubric`, `table`, 
   `parsed-literal`, `code`
 * Quotations: `epigraph`, `highlights` and `pull-quote`
 * Images: `figure` and `image`
 * For Substitution Definitions: `replace`, `date`, `unicode`
 * Other: `meta`, `raw`
 
The following limitations apply to these directives:

 * `code` does currently not support syntax highlighting 
   (it allows to set the language though, so client-side highlighters can be integrated if required).
   
 * `sectnum` does currently not support the `prefix`, `suffix` and `start` options.
 
 * `include` does not support any of the options apart from the filename, due to the way document trees
   are traversed very differently in Laika.
  
 * `raw` does not support the `file` or `url` options. 
 
 * `unicode`: does not support the various trim options, as that would require modifying adjacent elements
   (and no other directive has this requirement, therefore API/implementation changes did not seem justified)
 
 * `date`: Uses the patterns of `java.text.SimpleDateFormat` instead of Python's `time.strftime` function.
 
 
### Supported Standard Text Roles

Text roles are a second extension mechanism for applying functionality to spans of text 
and the reference implementation supports a set of standard [standard text roles][std roles] out of the box.

[std roles]:      http://docutils.sourceforge.net/docs/ref/rst/roles.html 
 
Out of this set Laika supports the following:
 
 * `emphasis`
 * `strong`
 * `literal`
 * `subscript` (and `sub` alias)
 * `superscript` (and `sup` alias)
 * `title-reference` (and `title` alias) - the default role
 * `raw` (+ format option)


### Unsupported Extensions 
 
The following extensions are not supported:
 
 * `math`, `csv-table`, `list-table`, `target-notes` and `class` directives 
 * `math`, `pep-reference` and `rfc-reference` text roles
 
There are various reasons for excluding these extensions, some of them being rather technical.
For example, the `target-notes` and `class` directives would require processing beyond the directive itself, 
therefore would require new API.
Others, like the `pep-reference` text role, seemed too exotic to warrant inclusion in Laika.


### Raw Content Extensions

Two of the supported standard extensions, the `raw` directive and the `raw` text role, 
embed content in the target format in text markup.
Like with verbatim HTML for Markdown, these extensions are disabled by default, 
as Laika discourages the coupling of input and output formats, but it can be switched on if required. 

See [Raw Content] for examples on how to enable these extensions in the sbt plugin or the library API.


### Implementing a Custom Directive

Laika comes with a concise and typesafe DSL to declare custom directives and text roles.
It is fully documented in the scaladoc for @:api(laika.rst.ext.Directives) and @:api(laika.rst.ext.TextRoles).


HOCON
-----

Laika fully supports the official [HOCON specification], based on its own parsers 
(it does not wrap the Typesafe Config library).

HOCON in Laika is supported in various places:

- Directory configuration files
- Configuration headers in text markup documents and templates
- Attribute sections in directive declarations
- Substitution references compliant with the HOCON spec are supported anywhere in text markup and templates,
  including the syntax for optional references, e.g. `${?path}`.
  
For more information on how HOCON is used in the library see [Laika's HOCON API].

The only fairly minor exception in spec compliance is the deliberate decision not to support circular references.
Direct self references are supported (e.g. `path = ${path} [img, css]`) as they cover a very common use case,
but `a` referring to `b` referring to an earlier definition of `a` is not.
The cost of additional complexity in the implementation this would require seems disproportional to the little use 
supporting this edge case would provide. 
Convoluted circular references are also harder to read and error-prone when definitions move.

If you think this should be supported, please open a ticket and explain your reasoning and your use case.

[HOCON specification]: https://github.com/lightbend/config/blob/master/HOCON.md
