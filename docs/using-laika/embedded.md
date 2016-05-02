
Using Laika Embedded
====================

Apart from using Laika within sbt, you can alternatively embed it into 
Scala applications. Laika is very lightweight, it does not
require the installation of any external tools and the core artifact
has only very few library dependencies.

The main and most commonly used operation is a full transformation
from input text written in a lightweight markup language like Markdown to 
output formats like HTML.
 
Alternatively the parse and render operations can be executed
separately, which is described in the chapter
[Separate Parsing and Rendering].

The following sections describe the options for full transformations
from text markup to output format.



Transforming a Single Document
------------------------------

Converting from Markdown to HTML using files as input and output:

    Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
    
Converting from ReStructuredText to PDF using files as input and output:

    Transform from ReStructuredText to 
      PDF fromFile "hello.md" toFile "hello.pdf"


Converting using Strings as input and output

    val input = "some *text* example"
    
    val result = Transform from Markdown to HTML fromString input toString


Other options are using `java.io.Reader/Writer` or streams. Of course they
can be combined in any way, like going from file as input to String as output.


### Character Encoding

Laika uses the same platform-dependent defaults for file encodings as the
IO classes in the Scala SDK. The most convenient way to specify an encoding
is via an implicit:

    implicit val codec:Codec = Codec.UTF8

This codec will then be used by the `fromFile` and `toFile` methods shown
in the examples above.


### Reusing Transformer Instances

You can also keep the created transformer instance to reuse it with different
inputs and outputs:

    val transform = Transform from ReStructuredText to HTML
    
    transform fromFile "hello.rst" toFile "hello.html"
    
    val result = transform fromString "some *text* example" toString

 
### Reusing a Parsed Document
    
All objects created by the API are reusable and immutable. For example,
using the same input for two different types of output could be coded like this:

    val doc = Transform from ReStructuredText to HTML fromFile "hello.rst"
    
    doc toFile "hello.html"
    
    val res = doc toString
    


### Pretty Print Output
    
Laika also supports a PrettyPrint renderer which can be useful for debugging.
It visualizes the document tree model while shortening longer text spans
into one line:

    val input = "some *text* example"
    
    Transform from Markdown to PrettyPrint fromString input toString
    
    res0: java.lang.String = Document - Blocks: 1
    . Paragraph - Spans: 3
    . . Text - 'some '
    . . Emphasized - Spans: 1
    . . . Text - 'text'
    . . Text - ' example'



Transforming an Entire Directory
--------------------------------

For transforming a directory you can specify the source and target 
directories separately:

    Transform from Markdown to 
      HTML fromDirectory "source" toDirectory "target"

The parser will pick up any document with a recognized suffix (`.md` or `.markdown`
for Markdown, `.rst` for reStructuredText).

When transforming to PDF, a directory of input files will get merged 
into a single PDF document:

    Transform from Markdown to 
      PDF fromDirectory "source" toFile "target.pdf"


### HTML Templates

The directories that contain the markup documents can optionally contain
templates that get applied to the parsed documents. A very basic template
may look like this:

    <html>
      <head>
        <title>{{document.title}}</title>
      </head>
      <body>
        <div class="content">
          {{document.content}}
        </div>
      </body>
    </html>
    
The two placeholders enclosed in double curly braces will be replaced with the
title and content of the parsed markup document. Everything else will be copied
to the final document unchanged.

Every directory can contain a file with the name `default.template.html` that
automatically gets applied to all documents in that directory and its subdirectories,
unless overridden in a subdirectory. There are also ways to manually declare a
template for a specific markup document only. 

For more details on the template engine, see the chapter [Templates].

For features like titles, sections, cross-linking, auto-numbering 
and tables of content, see the chapter [Document Structure].


### Merging Multiple Input Directories

Laika allows to merge multiple directories into a tree structure with 
a single root. This allows to keep reusable styles and templates separately,
a feature often called "themes" by other tools, but the support in Laika
is fully generic and the purpose of the merge can be very different from
classic themes.

When the merged directories contain subfolders with the same name, those
will be merged recursively. Only files with the same name in the same folder
are treated as errors.

Use the `fromDirectories` method to specify the directories to merge:

    Transform from Markdown to 
      HTML fromDirectories ("source","styles") toDirectory "target"


### Mixing Markdown and reStructuredText

Both text markup formats can be used within the same directory structure,
including cross-linking between the two formats. The parser to be used
is then determined by the file extension of each of the input files.
`.md` and `.markdown` files get parsed by the Markdown parser, `.rst`
files by the reStructuredText parser.

All you have to do is pass both parsers to the API:

    Transform from Markdown or ReStructuredText to 
      PDF fromDirectory "source" toFile "target.pdf"


### Parallel Execution

When transforming a large number of files you may want to run the operations
in parallel: 

    (Transform from Markdown to 
      HTML fromDirectory "source").inParallel toDirectory "target"

Note that we have to introduce parenthesis here, as `inParallel` is
a no-arg method breaking the pattern of the fluent API.

The actual transformation is a three phase process, the first (parsing) and
third (rendering) can run in parallel. For the second phase this is not possible,
as this is the document tree model rewrite step where things like cross references or
table of contents get processed that need access to more than just the current
document. But the rewrite phase is also the least expensive phase so that you
should still benefit from parallel execution.   


### Character Encoding

The mechanism is the same as for specifying an encoding for a single file.

Laika uses the same platform-dependent defaults for file encodings as the
IO classes in the Scala SDK. The most convenient way to specify an encoding
is via an implicit:

    implicit val codec:Codec = Codec.UTF8

This codec will then be used by the `fromDirectory`, `toDirectory` and
`withRootDirectory` methods shown in the examples above.


### Reusing Transformer Instances

Like with single file transformation you can (and usually should) keep 
the created transformer instance to reuse it with different
directories:

    val transform = Transform from ReStructuredText to HTML
    
    transform fromDirectory "source-1" toDirectory "target-1"
    
    transform fromDirectory "source-2" toDirectory "target-2"


### Error Reporting

Text markup parsers are usually very resilient. For any input they cannot
make sense of they fall back to rendering it back as raw text. Therefore
transformations rarely fail, but the output may not be exactly what you 
expected.

For some errors like unresolved link references or illegal directive
attributes, Laika inserts system message nodes into the tree. By default
these are ignored by all renderers, but you can explicitly enable
the rendering of message nodes for a specific message level.

In the HTML renderer messages are rendered as a span with the class
`system-message` and a second class for the level (`warning`, `error`, etc.),
so you can add CSS to easily identify these nodes in the page. This can be
useful for your own debugging purposes, but also for applications that allow
users to edit text with markup, giving them visual feedback for their mistakes.

The following example renders all message with the level `Warning` or higher:

    Transform from Markdown to 
      HTML.withMessageLevel(Warning) fromDirectory 
      "source" toDirectory "target"


