
Configuration
=============

Laika has a rich set of features and keeps almost every module customizable or even replaceable, 
and as a consequence there is a lot to configure.

This chapter puts the focus on the basic configuration options that you are most likely to use
on a day-to-day basis.

If you want to dig deeper and tweak or extend the way Laika parses or renders,
see the intro sections to [Customizing Laika](../04-customizing-laika/01-overview.md)
and [Extending Laika](../05-extending-laika/01-overview.md) for an overview.


Basic Settings
--------------

These settings are available in the library API and the sbt plugin.


### Strict Mode

Strict mode switches off all extensions and only uses features defined in the spec of the markup languages.

This might be useful if you develop applications with user input for example, and want to ensure that
only standard markup features are exposed to users.

Markup extensions enabled by default are:

* Custom Directives - either the built-in [Standard Directives] 
  or user-defined as shown in [Implementing Directives].

* Support for HOCON configuration headers in markup files.

* Support for [Substitution Variables] in markup in the form of `${some.key}`.

To disable all these extensions you can use the `strict` flag:

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults.strict
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .strict
  .build
```

@:@

### Raw Content

Raw content is the term Laika uses for any markup syntax that allows the inclusion of content in the output format,
e.g. HTML.

This is disabled by default, even if it is part of the original text markup specification as in the 
case of Markdown.

This is to generally discourage to tie markup input to a concrete output format, 
as Laika supports multiple formats like HTML, EPUB and PDF and an API where users can easily add their own format.
Markup files containing raw HTML could not be used for those.

Secondly, when the markup originates from user input in a web application, 
it would not be safe to use without additional filtering. 
By default Laika does not filter any tags, not even `<script>` tags, 
but whitelist filtering can be added fairly easily through Laika's customization hooks like [AST Rewriting] 
or [Overriding Renderers].
 
You can enable verbatim HTML and other raw formats explicitly in the configuration:

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults.withRawContent
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withRawContent
  .build
```

@:@


### Character Encoding

The default encoding in Laika is UTF-8. 

When you need to work with different encodings you can override the default: 

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults.encoding(Codec.ISO8859)
```

@:choice(library)
```scala
implicit val codec:Codec = Codec.UTF8
```

This has to be in scope where you specify the input and ouput files for your transformer

@:@


Navigation
----------

These features have their dedicated [Navigation] chapter, including configuration examples,
therefore we'll just give a brief overview of available configuration options here and link to the relevant sections.

* [Global Link Definitions] help to avoid repetition by mapping URLs to an id in configuration and making
  them available for use with "native" markup link syntax (e.g. `link to [my-globally-defined-id]` in Markdown).

* [Linking to API Documentation] describes the `@:api` shortcut for defining links to API documentation,
  which requires setting the relevant base URLs in the configuration.
  
* [Disabling Validation] is a configuration option for excluding some directories/paths from
  validation, e.g. in cases where you know that a directory will be populated by some external tool.
  In its default setup, a Laika transformation fails when an internal link points to a document that does
  not exist in the input directory.

* E-books (EPUB or PDF) have additional configuration options for navigation:

    * [Book Navigation] allows to specify the depth of the auto-generated navigation.
    
    * The location of [Cover Images] can be configured, too.
    
    * And [Document Metadata] can be specified so that it will be converted to a format
      compatible with the corresponding readers.


Error Handling
--------------

Text markup parsers are usually very resilient. 
For any input they cannot make sense of they fall back to rendering it back as raw text.
If, for example you start an emphasized span with an `'*'`, but then never close the span,
it will simply be treated as a literal and will not trigger an error. 

But there are two groups of errors that do cause a transformation to fail.


### Error Types

* **Unrecoverable errors**: for example I/O errors where files cannot be read or directories cannot
  be written to, or invalid configuration, e.g. in HOCON configuration headers, which is also treated as fatal,
  since such a configuration section usually drives the behaviour or rendering of other nodes.

* **Invalid nodes**: these are "local" errors, which are confined to a single AST node.
  This may be a reference to a section, document or footnote that does not exist or a directive with
  invalid or missing attributes.

The handling of the second kind of error can be controlled via configuration.


### Default Behaviour

AST nodes that capture errors carry an error message, a severity
and a fallback node to use if configuration would allow to proceed with rendering.

Laika comes with two configuration options that control how these nodes are dealt with:

* `failOnMessages` controls the minimum severity that causes a transformation to fail, the default is `Error`.
* `renderMessages` controls the minimum severity that causes a message to be included in the rendered output,
  the default is `None`.

This means, by default the presence of one or more invalid nodes in the AST of a document 
causes the transformation to fail, with all their message collected in the returned error type.


### Visual Debugging

In some cases you may prefer to examine the errors in the rendered content, 
or you are processing user input where it is more helpful to display them in context than just as a list of
error messages.

You can achieve this by basically flipping the two default values in the configuration:

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .failOnMessages(MessageFilter.None)
  .renderMessages(MessageFilter.Error)
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .failOnMessages(MessageFilter.None)
  .renderMessages(MessageFilter.Error)
  .build
```

@:@

Now rendering proceeds even with invalid nodes and they will be rendered in the location of the document
they occurred in.
In HTML output these node are rendered as a span with the class `runtime-message` and a second class for the level 
(`warning`, `error`, etc.), so that their display can be controlled via CSS.


### The AST Renderer

If you need even more low-level insight, you can use the included AST renderer
to get a formatted output of the entire AST structure.

Longer plain text spans are truncated in this output format to put the emphasis on the structure of the document. 

@:choices(config)

@:choice(sbt)
Create a file with the content `some *text* example` in the input directory and run:

```scala
laikaAST
```

This task is a shortcut for `laikaGenerate ast`

@:choice(library)
```scala
val input = "some *text* example"

Transformer
  .from(Markdown)
  .to(AST)
  .build
  .transform(input)
```

@:@

The output for the small example above will have the following format:

```laika-ast
RootElement - Blocks: 1
. Paragraph - Spans: 3
. . Text - 'some '
. . Emphasized - Spans: 1
. . . Text - 'text'
. . Text - ' example'
```


User-Defined Variables
----------------------

Finally, the entire machinery for passing configuration around can be used by users, too.

You can define variables in any of the following scopes:

* Programmatically in global configuration

* Per directory with HOCON in a file named `directory.conf`

* Per markup document in a HOCON configuration header

This is an example for defining two variables globally: 

@:choices(config)

@:choice(sbt)
```scala
laikaConfig := LaikaConfig.defaults
  .withConfigValue("version.latest", "2.4.6")
  .withConfigValue("license", "Apache 2.0")
```

@:choice(library)
```scala
val transformer = Transformer
  .from(Markdown)
  .to(HTML)
  .using(GitHubFlavor)
  .withConfigValue("version.latest", "2.4.6")
  .withConfigValue("license", "Apache 2.0")
  .build
```
@:@

These values can then be accessed via [Substitution Variables] in templates or in markup files:

```laika-md
The latest release is ${version.latest}.
It's released under the ${license} license.
```

If you define them in a narrower scope and not globally, they won't be available outside of that scope.

There are two namespaces for variable keys which are used by the library, `laika.*` and `cursor.*`.
As long as you avoid these two namespaces, you can freely use any configuration keys.
