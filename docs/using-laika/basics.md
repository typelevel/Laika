
Transformation Basics
=====================


The main and most commonly used operation of Laika is a full transformation
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

For features like titles, sections, autonumbering and tables of content, see
the chapter [Document Structure].


### Directory Layout Conventions

When you organize your input and output directories under a common root
directory, with the directory containing the markup documents named
`source` and the output directory named `target`, you do not have
to specify input and output directories separately:

    Transform from ReStructuredText to HTML withRootDirectory "myDocs"


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
    
    transform fromDirectory "source-1" toDirectory "target-2"
    
    transform fromDirectory "source-1" toDirectory "target-2"

