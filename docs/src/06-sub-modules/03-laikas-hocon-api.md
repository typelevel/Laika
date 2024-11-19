
Laika's HOCON API
=================

Laika comes with its own HOCON parser and the corresponding API is used throughout the document model.
It fully supports the [HOCON specification], albeit based on its own parser implementation.

[HOCON specification]: https://github.com/lightbend/config/blob/master/HOCON.md


Why Laika Uses HOCON
--------------------

There is a little irony in that Laika went the extra mile to introduce its own HOCON parser even though
this project actually does not recommend the use of external files for application configuration!


### Problems with Configuration Files

File-based application configuration was usually introduced with the promise of being able to relaunch the
application with different configurations without rebuilding. 
However, most real-world deployment processes involve a git commit of any configuration changes and a subsequent
rebuild on a CI server.
This way this mechanism never delivers on its initial promise, but instead introduces several problems:
the indirection of defining values in one place and accessing them in code and the stringly nature
of the configuration which tends to be error-prone compared to a more type-safe approach.

For this reason newer configuration libraries like Ciris by-pass the file approach entirely.
The motivation and some use cases are introduced in this [presentation][ciris-scalaxchange].

Likewise Laika encourages programmatic configuration approaches for all its global configuration options,
and in fact none of the code examples in the manual for this type of configuration show any HOCON-based approaches.

[ciris-scalaxchange]: https://skillsmatter.com/skillscasts/12650-ciris-functional-configurations


### Laika's Use Case for HOCON

So why is there are a custom HOCON parser in Laika then?
The reason is that its primary use case in the library is not around global configuration.
An input tree (usually obtained from one or more input directories in the file system) is a hierarchical
structure, and every sub-directory or individual document can override some configuration aspect,
as shown in [Where Laika Uses HOCON].
On top of that, Laika has an extension mechanism called directives (see [Implementing Directives] for details)
and for its syntax which often allows to add attributes to a directive declaration HOCON is a natural fit.

On such a fine-grained level, programmatic configuration is not practical, and HOCON's format is an attractive solution
thanks to its concise and flexible syntax and its support for substitution definitions where a declaration in a document
or directive attribute section can directly refer to something defined globally or in a parent scope.


### Why a New Parser?

After the question why HOCON has been chosen is answered, there is still a follow-up question about the need
to re-implement an entirely new parser.
One reason is that the original implementation (the [Typesafe Config] library) and the projects that wrap around it
or derive from it do not meet Laika's requirement for full referential transparency.
They might throw exceptions or perform side effects without exposing it in the method signature.
Another reason is that Laika extends the capability of what kind of data can be held in a configuration node.
It allows an AST node to be assigned to a key in a `Config` instance so that it can be referred to in markup or
templates.
One of the key features, the way how a template merges the content of its associated markup document,
is based on this extension, which allows the use of a substitution reference (`${cursor.currentDocument}`) for this task.

Finally, parsing is a central aspect of a lot of Laika's functionality anyway and it even comes with its own parser
combinators. 
If you examine the HOCON parser implementation you will notice that it's a very lightweight and small module
compared to the entire Laika code base.


[Typesafe Config]: https://github.com/lightbend/config


Where Laika Uses HOCON
----------------------

Multiple types in the document tree model have a `config` property holding the configuration for that scope:

* A `DocumentCursor` instance passed to directive implementations and custom rewrite rules.
  Probably the most likely instance you access a `config` property through.
* A `Document` instance, populated from the configuration header of the markup document
* A `TemplateDocument` instance, populated from the configuration header of the template document
* A `DocumentTree` instance, populated from the `directory.conf` file in the corresponding directory
* Each directive can optionally have a HOCON attribute block

Whenever the corresponding configuration header or file is missing, an empty `Config` instance will be used.

The `Config` instances will be populated with values supplied by the library in the `laika.*` and `cursor.*` namespaces.
The former holds configuration values and the latter navigation info for the current document.
An additional namespace `helium.*` will be used if you use the default theme, containing theme configuration
and some pre-built AST nodes.

Any user-supplied values will be available, too, and should live in any namespace other than the three reserved ones.


Reading from Config Instances
-----------------------------

Once you obtained access to a `Config` instance, either through one of the objects listed above 
or by [Creating a Config Instance] yourself, reading from it is quite straightforward.
You have to provide the key you want to read and the type you expect:

```scala mdoc:compile-only
import laika.api.config.Config

def config: Config = ???

val version = config.get[String]("project.version")
```

The type conversion is based on a matching `ConfigDecoder[T]` which must be in implicit scope.
It's a mechanism you might be familiar with from JSON libraries.

The returned value is of type `Either[ConfigError, T]`.
It will be a `Left` when either the type conversion fails or the key is not defined.

Laika comes with decoders for basic types like `Boolean`, `String`, `Int`, `Path` or `Date` 
and sequences and maps of them.
They are in the companion, therefore do not require any imports.

The `Path` decoder deals with the type representing Laika's virtual path.
It is convenient in that it resolves relative paths to absolute paths based on the origin of the definition.
See [Virtual Tree Abstraction] for details.

There are variants of the `get` method shown above: there is an overload that allows to pass a default value
as the second argument that will be used when there is no value for the specified key.
There is a `getOpt` variant that turns the result into `Either[ConfigError, Option[T]]` 
and there is a `hasKey` method to check for the existence of a key.

See @:api(laika.config.Config) for the full API documentation.


Creating Decoders and Encoders
------------------------------

Decoders are required for reading from a `Config` instance as shown in the previous section, the most common scenario.
Encoders are needed when you populate a `Config` instance programmatically as shown in [Programmatic Building].


### ConfigDecoder

A decoder for a simple type is quite straightforward, it usually piggy-backs on an existing decoder.
Let's assume you have a `Color` enum, with a constructor to create instances from a string:

```scala mdoc:silent
sealed trait Color {
  def name: String
}
// enum values omitted
object Color {
  def fromString (value: String): Option[Color] = ???
}
```

You can then flatMap on the string decoder to obtain a Color decoder:

```scala mdoc:silent
import laika.api.config._
import laika.api.config.ConfigError.DecodingFailed

implicit val colorDecoder: ConfigDecoder[Color] = 
  ConfigDecoder.string.flatMap { str =>
    Color.fromString(str)
      .toRight(DecodingFailed(s"Unsupported color name: $str"))
  }
```

Now let's assume you need a decoder for a case class with the following shape:

```scala mdoc:silent
case class Person (name: String, age: Int, city: Option[String])
```

For mapping a HOCON object to a Scala case class you would usually build on top of the `config` decoder,
which decodes a nested object into an instance that has the same API for querying values as the root.

```scala mdoc:silent
import laika.api.config._

implicit val decoder: ConfigDecoder[Person] = 
  ConfigDecoder.config.flatMap { config =>
    for {
      name <- config.get[String]("name")
      age  <- config.get[Int]("age")
      city <- config.getOpt[String]("city")
    } yield {
      Person(name, age, city)
    }
}
```


### ConfigEncoder

We are going to demonstrate how to write encoders for the same objects we used for our decoder examples.

We can encode our `Color` enumeration, assuming it has a `name` property, like this:

```scala mdoc:silent
implicit val colorEncoder: ConfigEncoder[Color] = ConfigEncoder.string.contramap(_.name)
``` 

For our Person case class we can use the convenient shortcuts in `ObjectBuilder`:

```scala mdoc:silent
implicit val encoder: ConfigEncoder[Person] = ConfigEncoder[Person] { person =>
  ConfigEncoder.ObjectBuilder.empty
    .withValue("name", person.name)
    .withValue("age", person.age)
    .withValue("city", person.city)
    .build
}
```

The builder deals with optional values by omitting the property altogether when it's empty.


### Automatic Derivation

Laika currently does not contain functionality for automatically deriving encoders or decoders for case classes.
One reason is that in contrast to JSON libraries the likeliness you need to map larger structures is much smaller.
Therefore, the amount of boilerplate is usually tolerable.

Secondly, the current (seemingly indefinite) transition period from Scala 2 to Scala 3 would require
to implement the derivation mechanism twice, since macro-based solutions are very different for those Scala 2 and 3.
There would be the option to introduce them as a feature for Scala 3 only, but then Laika's own code base
would not be able to benefit from them.

Thirdly, such functionality has not been requested even once so far.

For those reasons there are currently no plans to add this functionality.


Creating a Config Instance
--------------------------

The most common use cases for `Config` instances in Laika are read access.
But there may be scenarios where you want to create new instances yourself.
One would be when you create an entire `DocumentTree` programmatically instead of parsing from sources.
It is entirely possible in Laika to feed renderers for EPUB, PDF and HTML solely with content generated in-memory.
The other use case would be if you want to use Laika's HOCON parser completely independently from its other features.

There are two ways to create a `Config` instance. 
One is based on parsing HOCON string input with a `ConfigParser`, the other is to assemble configuration values
programmatically with a `ConfigBuilder`.
The former mostly exists for scenarios where you need to work with files, 
whereas the latter should be preferred for creating configuration data in-memory.


### Programmatic Building

A `ConfigBuilder` allows to assemble arbitrary values as long as they have a `ConfigEncoder` in scope.

```scala mdoc:compile-only
val config = ConfigBuilder.empty
  .withValue("laika.epub.coverImage", "/images/epub-cover.jpg")
  .withValue("laika.pdf.coverImage", "/images/pdf-cover.jpg")
  .build
```

The first parameter is the key to assign to the value, 
the second is the actual value which will be converted based on an implicit `ConfigEncoder` in scope.

Laika comes with encoders for basic types like `Boolean`, `String`, `Int`, `Path` or `Date` 
and sequences and maps of them. 
They are in the companion, therefore do not require any imports.

You can alternatively create your own encoder as shown above.

If you have a fallback instance, you can pass it to the constructor:

```scala mdoc:compile-only
def parentConfig: Config = ???

val config = ConfigBuilder.withFallback(parentConfig)
  .withValue("laika.epub.coverImage", "/images/epub-cover.jpg")
  .withValue("laika.pdf.coverImage", "/images/pdf-cover.jpg")
  .build
```

The fallback will be used for resolving any values not present in the current instance.

Finally, if you want to modify an existing `Config` instance of a particular `Document` instance
you can use the `modifyConfig` method:

```scala mdoc:compile-only
import laika.ast.Document

def doc: Document = ???

val finalDoc = doc.modifyConfig(_
  .withValue("laika.epub.coverImage", "/images/epub-cover.jpg")
  .withValue("laika.pdf.coverImage", "/images/pdf-cover.jpg")
)
```

This is more efficient than replacing the config and preserves the origin info in the existing config property which is essential for resolving relative paths defined in that configuration correctly.


### Parsing HOCON

The `ConfigParser` has a very simple API:

```scala mdoc:compile-only
def hoconInput: String = ???

val result: Either[ConfigError, Config] = ConfigParser
  .parse(hoconInput)
  .resolve()
```

The `parse` step creates an interim model of unresolved configuration values.
This is necessary because the HOCON format supports substitution references and the corresponding values do not need
to originate from the same instance.
The `resolve` step then finally creates a `Config` instance, resolving and validating all references.

If you have a fallback instance, you can pass it via `resolve`:

```scala mdoc:compile-only
def hoconInput: String = ???
def parentConfig: Config = ???

val result: Either[ConfigError, Config] = ConfigParser
  .parse(hoconInput)
  .resolve(fallback = parentConfig)
```

The fallback will be used for resolving any values not present in the current instance.

Finally, if you are building a `Config` instance that you want to assign to a `Document` instance in cases
where you build an entire tree programmatically, you also have to provide a correct `Origin` instance:

```scala mdoc:compile-only
import laika.ast.Document

def hoconInput: String = ???
def doc: Document = ???
val docOrigin: Origin = Origin(Origin.DocumentScope, doc.path) 

val result: Either[ConfigError, Document] = ConfigParser
  .parse(hoconInput)
  .resolve(origin = docOrigin)
  .map { config =>
    doc.withConfig(config)
  }
```

This is essential for resolving relative paths defined in that configuration correctly.

There is currently no API for conveniently reading HOCON from files.
You would need to do the file IO yourself before feeding the parser.
Should the HOCON parser become a standalone micro-lib, it would definitely get a `ConfigLoader`.
But within Laika's own usage all IO is performed by the logic in the `laika-io` module that also deals
with loading of markup files and templates.
