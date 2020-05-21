
Laika's HOCON API
=================

Laika comes with its own HOCON parser and the corresponding API is used throughout the document model.


Why Laika Uses HOCON
--------------------

TODO


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

Any user-supplied values will be available, too, and should live in any namespace other than the two reserved ones.


Reading from Config Instances
-----------------------------

Once you obtained access to a `Config` instance, either through one of the objects listed above 
or by [Creating a Config Instance] yourself, reading from it is quite straightforward. [TODO - ] 
You have to provide the key you want to read and the type you expect:

```scala
TODO
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
See [Virtual Path Abstraction] for details.

There are variants of the `get` method shown above: there is an overload that allows to pass a default value
as the second argument that will be used when there is no value for the specified key.
There is a `getOpt` variant that turns the result into `Either[ConfigError, Option[T]]` 
and there is a `hasKey` method to check for the existence of a key.

See @:api(laika.config.Config) for the full API documentation.


Creating Decoders and Encoders
------------------------------

Decoders are required for reading from a `Config` instance as shown in the previous section, the most common scenario.
Encoders are needed when you populate a `Config` instance programmatically as shown in [Using ConfigBuilder].


### ConfigDecoder

A decoder for a simple type is quite straightforward, it usually piggy-backs on an existing decoder.
Let's assume you have a `Color` enum, with a constructor to create instances from a string:

```scala
object Color {
  def fromString (value: String): Option[Color] = ???
}
```

You can then flatMap on the string decoder to obtain a Color decoder:

```scala
implicit val colorDecoder: ConfigDecoder[Color] = 
  ConfigDecoder.string.flatMap { str =>
    Color.fromString(str).toRight(DecodingError(s"Unsupported color name: $str"))
  }
```

Now let's assume you need a decoder for a case class with the following shape:

```scala
case class Person (name: String, age: Int, city: Option[String])
```

For mapping a HOCON object to a Scala case class you would usually build on top of the `config` decoder,
which decodes a nested object into an instance that has the same API for querying values as the root.

```scala
implicit val decoder: ConfigDecoder[Person] = ConfigDecoder.config.flatMap { config =>
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

```scala
implicit val colorEncoder: ConfigEncoder[Color] = ConfigEncoder.string.contramap(_.name)
``` 

For our Person case class we can use the convenient shortcuts in `ObjectBuilder`:

```scala
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
Therefore the amount of boilerplate is usually tolerable.

Secondly we are so close to a Scala 3 release that will make automatic derivation much easier,
that the step of building something on top of shapeless for Scala 2 (and paying the compile time tax)
or writing a low-level Scala-2-style macro does seem very unattractive.

Automatic derivation will be supported once the Laika code base moves to Scala 3.


Creating a Config Instance
--------------------------

The most common use cases for `Config` instance in Laika are read access.
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

```scala
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

```scala
val parentConfig: Config = ???

val config = ConfigBuilder.withFallback(parentConfig)
  .withValue("laika.epub.coverImage", "/images/epub-cover.jpg")
  .withValue("laika.pdf.coverImage", "/images/pdf-cover.jpg")
  .build
```

The fallback will be used for resolving any values not present in the current instance.

Finally, if you are building a `Config` instance that you want to assign to a `Document` instance in cases
where you build an entire tree programmatically, you also have to provide a correct `Origin` instance:

```scala
val doc: Document = ???
val docOrigin: Origin = Origin(Origin.DocumentScope, doc.path) 

val config = ConfigBuilder.withOrigin(docOrigin)
  .withValue("laika.epub.coverImage", "/images/epub-cover.jpg")
  .withValue("laika.pdf.coverImage", "/images/pdf-cover.jpg")
  .build
  
val finalDoc = doc.copy(config = config)
```

This is essential for resolving relative paths defined in that configuration correctly.


### Parsing HOCON

The `ConfigParser` has a very simple API:

```scala
val hoconInput: String = ???

val config: Config = ConfigParser
  .parse(hoconInput)
  .resolve()
```

The `parse` step creates an interim model of unresolved configuration values.
This is necessary because the HOCON format supports substitution references and the corresponding values do not need
to originate from the same instance.
The `resolve` step then finally creates a `Config` instance, resolving and validating all references.

If you have a fallback instance, you can pass it via `resolve`:

```scala
val hoconInput: String = ???
val parentConfig: Config = ???

val config: Config = ConfigParser
  .parse(hoconInput)
  .resolve(fallback = parentConfig)
```

The fallback will be used for resolving any values not present in the current instance.

Finally, if you are building a `Config` instance that you want to assign to a `Document` instance in cases
where you build an entire tree programmatically, you also have to provide a correct `Origin` instance:

```scala
val hoconInput: String = ???
val doc: Document = ???
val docOrigin: Origin = Origin(Origin.DocumentScope, doc.path) 

val config: Config = ConfigParser
  .parse(hoconInput)
  .resolve(origin = docOrigin)
  
val finalDoc = doc.copy(config = config)
```

This is essential for resolving relative paths defined in that configuration correctly.

There is currently no API for conveniently reading HOCON from files.
You would need to do the file IO yourself before feeding the parser.
Should the HOCON parser become a standalone micro-lib, it would definitely get a `ConfigLoader`.
But within Laika's own usage all IO is performed by the logic in the `laika-io` module that also deals
with loading of markup files and templates.
