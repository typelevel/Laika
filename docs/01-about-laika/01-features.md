
Features
========

from old output page:

The current release supports HTML, EPUB, PDF, XSL-FO and AST.
Rendering happens from a generic document tree model shared between all parsers,
so that no renderer implementation has to understand specifics about a concrete
markup syntax like Markdown or reStructuredText.

Customization of the output is possible on two levels, first most formats (except
for AST) can be styled with CSS. Secondly the rendering of specific nodes
can be overridden with a simple partial function as described in 
[Customizing Renderers][../customizing-laika/customize-rendering.md:Customizing Renderers].

Finally you can develop an entirely new renderer for a format not supported by Laika
out of the box. See chapter [Implementing a Renderer] for details.

================================================================================================

from old markup page:

The current release supports Markdown and reStructuredText. Both implementations stay as close
as possible to the original descriptions and specifications of these formats. 

The markup gets parsed into a generic document tree model shared between all parsers. This means
that there are some adjustments in terminology (e.g. a bullet list in reStructuredText
is called an "unordered list" in the Markdown syntax description). 

It also means that
there are some tree element types which are not produced by the Markdown parser, like
table, footnotes or citations, as reStructuredText is much more feature-rich than
the original Markdown syntax. 

The unified model allows to create renderers that
do not have to deal with any of the specifics of an individual markup syntax.

================================================================================================
