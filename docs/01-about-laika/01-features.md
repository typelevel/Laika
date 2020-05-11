
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
