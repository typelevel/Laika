
Supported Markup
================

The initial release only supports Markdown. But at least two further formats are planned
for subsequent releases: restructuredText and "Markmore", the working title of a new
format of extended Markdown syntax, inspired by existing extensions like those from
GitHub, kramdown or Pandoc, with the overall goal of trying to marry the clarity and
simplicity of Markdown with the power of reStructuredText.

Nevertheless this document only covers the language available today, which is Markdown.


Markdown
--------

Laika supports Markdown as the first lightweight text markup language primarily due to its popularity.
It has several advantages, mainly that it is very easy to learn, very lightweight, and produces
documents which are fairly good to read, even when you don't know anything about Markdown. 
The popularity is probably largely based on these characteristics and is well deserved.

However, Markdown also has its problems. First, there is no real specification,
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
This document is not going to provide a syntax overview and refer to the official documentation instead.
It will primarily focus on anything specific to the way Laika handles Markdown.

To get an overview over Markdown syntax, these documents may be used:

* For a description of  [official syntax documentation][markdown docs].

* For trying out small snippets of Markdown and checking how the various existing Markdown implementations
  handle them, the [Babelmark] tool can be used.  

* For using Markdown in Laika, the [Transformation Basics] page should answer most of your questions.

* For the special treatment of verbatim HTML in Laika, see the following section.

Laika tries to follow the official syntax documentation. In cases where it is silent on how
to handle certain edge cases, the Babelmark tool has been consulted and usually the approach
the majority of available parses have chosen has been picked for Laika, too. There is currently only
one (known) minor exception: 

* Laika does not detect a header if it is not preceded by a blank line. Supporting this would be
  disadvantageous for three reasons: it feels to be against the Markdown design goal of promoting
  readability, it would slow down the parser as there'd need to be a lookahead to the next line
  for each line of a regular paragraph, and finally it'll open the doors for accidental headers.
  According to Babelmark, there are at least two parsers (Pandoc, Python-Markdown) that agree.
   

[markdown docs]: http://daringfireball.net/projects/markdown/syntax

[Babelmark]: http://johnmacfarlane.net/babelmark2/


### Verbatim HTML

Finally there is one major difference to standard Markdown: the parsing of verbatim HTML elements
is not enabled by default, but it can be configured if required. Before showing how to do that,
this section explains this design decision.

The problems with enabling verbatim HTML elements by default would be:

* It goes against Markdown's own design goal of great readability, and of being easy for anyone
  to pick up. Markdown syntax basics can be understood in minutes, by non-technical people, but producing well-formed
  and valid HTML is not trivial if you do not regularly deal with HTML.
  
* It goes against Laika's design goal of decoupling parsers and renderers. Allowing verbatim
  HTML elements really only makes sense if the output format is HTML, too. Once Laika adds
  output formats like PDF, it would be unclear what these renderers should do with these elements.
  Markdown was designed with only HTML output in mind, so this mismatch is natural.
  
* It would not be safe to use without additional filtering when used in web applications
  that offer user input. Before rendering some processing based on whitelists would need
  to be performed to avoid security vulnerabilities. Laika makes it easy to add such
  whitelist functionality through document tree rewriting.
  
This design decision has the following consequences for the Laika library:

* The Markdown parser, by default, treats HTML tags as normal text input and converts
  tags to HTML entities when rendering, so that they would show up as part of the
  rendered text nodes in the final HTML document. How to enable HTML parsing is shown 
  further below.
  
* The elements nodes produced by the Markdown HTML parsers are not part of the standard
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
   
    
  
  
  
  