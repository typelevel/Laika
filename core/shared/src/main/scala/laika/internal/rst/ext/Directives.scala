/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.internal.rst.ext

import laika.ast.*
import laika.internal.rst.bundle.RstExtension
import laika.parse.{ Parser, SourceFragment }
import laika.parse.builders.~
import laika.parse.markup.RecursiveParsers
import ExtensionParsers.Result

/** API for creating directives, the extension mechanism of reStructuredText.
  *
  * The API did not aim to mimic the API of the original Python reference implementation.
  * Instead the goal was to create an API that is idiomatic Scala, fully typesafe and as concise as possible.
  * Yet it should be flexible enough to semantically support the options of the Python directives, so that
  * ideally most existing Python directives could theoretically get ported to Laika.
  *
  * =Comparison with Laika Directives=
  *
  * Extensions defined in the way described in this chapter could still be used when parsing the markup documents
  * with a different reStructuredText implementation, as they are fully compatible with the original specification.
  *
  * If this is not a requirement you may alternatively use the Laika variant of directives.
  * This would give you the following advantages:
  *
  * - The syntax definition is simpler, while offering the same flexibility.
  * - The directive may be used in other parsers, too, like in the Markdown parser.
  * - The directive may also be used in templates.
  * For details on these alternative directive types see
  * [[http://typelevel.org/Laika/latest/05-extending-laika/03-implementing-directives.html]].
  *
  * =Implementing a Directive=
  *
  * Entry points are the `BlockDirective` and `SpanDirective` objects.
  * The Python reference parser does not make this distinction on the API level,
  * but does this internally based on the context a directive is parsed in.
  * Since Laika APIs are typesafe, the distinction is necessary
  * since block level and span level directives create different types of document tree nodes.
  * A `SpanDirective` can only be used in a substitution definition which can then be used within flow elements.
  * A `BlockDirective` can be used directly in any location other block level content like paragraphs
  * or lists can be used.
  *
  * A directive may consist of any combination of arguments, fields and body elements:
  *
  * {{{
  *  .. myDirective:: arg1 arg2
  *   :field1: value1
  *   :field2: value2
  *
  *   This is the body of the directive. It may consist of any standard or custom
  *   block-level and inline markup.
  * }}}
  *
  * In the example above `arg1` and `arg2` are arguments, `field1` and `field2` are fields,
  * and followed by body elements after a blank line. If there are no arguments or fields
  * the blank line may be omitted.
  * For the full specification, see
  * [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#directives]].
  *
  * For each of these directive elements, the API offers a method to specify whether the element is required or optional,
  * and an optional function to convert or validate the parsed value.
  *
  * =Basic Example=
  *
  * Consider the following simple example of a directive with just one argument and a body:
  *
  * {{{
  *  .. note:: This is the title
  *
  *   This is the body of the note.
  * }}}
  *
  * The implementation of this directive could look like this:
  *
  * {{{
  *  case class Note (title: String,
  *                   content: Seq[Block],
  *                   options: Options = Options.empty) extends Block
  *                                             with BlockContainer[Note]
  *
  *  object MyDirectives extends RstExtensionRegistry {
  *    val blockDirectives = Seq(
  *      BlockDirective("note") {
  *        (argument(withWS = true) ~ blockContent).map { case title ~ content => Note(title, content) }
  *      }
  *    )
  *    val spanDirectives = Nil
  *    val textRoles = Nil
  *  )
  *
  *  val transformer = Transformer
  *    .from(ReStructuredText)
  *    .to(HTML)
  *    .using(MyDirectives)
  *    .build
  * }}}
  *
  * The `argument()` method specifies a required argument of type `String` (since no conversion function was supplied).
  * We need to set the `withWS` flag to true as an argument cannot have whitespace per default.
  * The `blockContent` method specifies standard block content (any block-level elements
  * that are supported in normal blocks, too) which results in a parsed value of type `Seq[Block]`.
  * Finally you need to provide a function that accepts the results of the specified directive elements as parameters
  * (of the corresponding type).
  * Here we created a case class with a matching signature so can pass it directly as the target function.
  * For a block directive the final result has to be of type `Block` which the `Note` class satisfies.
  * Finally the directive gets registered with the `ReStructuredText` parser.
  *
  * =Adding Converters and Validators=
  *
  * If any conversion or validation is required on the individual parts of the directive they can
  * be passed to the corresponding function:
  *
  * {{{
  *  def nonNegativeInt (value: String) =
  *    try {
  *      val num = value.toInt
  *      Either.cond(num >= 0, num, s"not a positive int: \$num")
  *    }
  *    catch {
  *      case e: NumberFormatException => Left(s"not a number: \$value")
  *    }
  *
  *  case class Message (severity: Int,
  *                      content: Seq[Block],
  *                      options: Options = Options.empty) extends Block
  *                                                with BlockContainer[Message]
  *
  *  object MyDirectives extends RstExtensionRegistry {
  *    val blockDirectives = Seq(
  *      BlockDirective("message") {
  *        (argument(nonNegativeInt) ~ blockContent).map {
  *          case severity ~ content => Message(severity, content)
  *        }
  *      }
  *    )
  *    val spanDirectives = Nil
  *    val textRoles = Nil
  *  )
  * }}}
  *
  * The function has to provide an `Either[String, T]` as a result.
  * A `Left` result will be interpreted as an error by the parser with the string being used as the message
  * and an instance of `InvalidBlock` containing the validator message
  * and the raw source of the directive will be inserted into the document tree.
  * In this case the final function (`Message`) will never be invoked.
  * A `Right` result will be used as an argument to the final function.
  * Note how the case class now expects an `Int` as the first parameter.
  *
  * =Optional Elements=
  *
  * Finally arguments and fields can also be optional. In case they are missing, the directive is still
  * considered valid and `None` will be passed to your function:
  *
  * {{{
  *  case class Message (severity: Option[Int],
  *                      content: Seq[Block],
  *                      options: Options = Options.empty) extends Block
  *                                                with BlockContainer[Message]
  *
  *  object MyDirectives extends RstExtensionRegistry {
  *    val blockDirectives = Seq(
  *      BlockDirective("message") {
  *        (optArgument(nonNegativeInt) ~ blockContent).map {
  *          case severity ~ content => Message(severity.getOrElse(0), content)
  *        }
  *      }
  *    )
  *    val spanDirectives = Nil
  *    val textRoles = Nil
  *  }
  * }}}
  *
  * The argument may be missing, but if it is present it has to pass the specified validator.
  *
  * In case of multiple arguments, the order you specify them is also the order in which they
  * are parsed from the directive markup, with the only exception being that required arguments
  * will always be parsed before optional ones, and arguments with whitespace need to come last.
  *
  * @author Jens Halm
  */
private[rst] object Directives {

  /** API to implement by the actual directive parser.
    *
    * This allows directive parts to specify the expected elements within
    * the parsed directive. In contrast to Laika's directive syntax which
    * allows to have a single directive parser for any kind of directive implementation,
    * the one for ReStructuredText has a separate parser for each directive depending
    * on its configuration.
    */
  trait DirectiveParserBuilder {

    def parser: Parser[Vector[Part]]

    def argument(withWS: Boolean = false): (Key, DirectiveParserBuilder)
    def optArgument(withWS: Boolean = false): (Key, DirectiveParserBuilder)
    def field(name: String): (Key, DirectiveParserBuilder)
    def optField(name: String): (Key, DirectiveParserBuilder)
    def body: (Key, DirectiveParserBuilder)
  }

  /** Represents one part of a directive (an attribute or a body element).
    */
  case class Part(key: Key, content: SourceFragment)

  /** Represents a single part (argument, field or body) of a directive.
    */
  abstract class DirectivePart[+A] extends (ParsedDirective => Result[A]) { self =>

    def map[B](f: A => B): DirectivePart[B] = new DirectivePart[B] {
      def apply(p: ParsedDirective) = self(p) map f
    }

    def flatMap[B](f: A => Result[B]): DirectivePart[B] = new DirectivePart[B] {
      def apply(p: ParsedDirective) = self(p) flatMap f
    }

    def ~ [B](other: DirectivePart[B]): DirectivePart[A ~ B] = new DirectivePart[A ~ B] {

      def apply(p: ParsedDirective): Result[A ~ B] = for {
        a <- self.apply(p)
        b <- other.apply(p)
      } yield new ~(a, b)

    }

  }

  /** Represents a single part (argument, field or body) of a directive.
    */
  abstract class DirectivePartBuilder[+A]
      extends (DirectiveParserBuilder => (DirectiveParserBuilder, DirectivePart[A])) { self =>

    def map[B](f: A => B): DirectivePartBuilder[B] = new DirectivePartBuilder[B] {

      def apply(parser: DirectiveParserBuilder): (DirectiveParserBuilder, DirectivePart[B]) = {
        val (newParser, part) = self.apply(parser)
        (newParser, part.map(f))
      }

    }

    def evalMap[B](f: A => Either[String, B]): DirectivePartBuilder[B] =
      new DirectivePartBuilder[B] {

        def apply(parser: DirectiveParserBuilder): (DirectiveParserBuilder, DirectivePart[B]) = {
          val (newParser, part) = self.apply(parser)
          (newParser, part.flatMap(f))
        }

      }

    def ~ [B](other: DirectivePartBuilder[B]): DirectivePartBuilder[A ~ B] =
      new DirectivePartBuilder[A ~ B] {

        def apply(
            parser: DirectiveParserBuilder
        ): (DirectiveParserBuilder, DirectivePart[A ~ B]) = {
          val (parserA, partA) = self.apply(parser)
          val (parserB, partB) = other.apply(parserA)
          (parserB, partA ~ partB)
        }

      }

  }

  sealed trait Key

  object Key {
    case class Argument(group: Int, position: Int) extends Key
    case class Field(name: String)                 extends Key
    case object Body                               extends Key
  }

  /** Represents the parsed but unprocessed content of a directive.
    */
  case class ParsedDirective(
      parts: Seq[Part],
      recursiveBlocks: SourceFragment => Result[Seq[Block]],
      recursiveSpans: SourceFragment => Result[Seq[Span]]
  ) {
    def part(key: Key): Option[SourceFragment] = parts.find(_.key == key).map(_.content)
  }

  /** Provides functions for internal use in the implementation of directive
    * combinators to convert the string value obtained from a directive
    * attribute or body.
    */
  object Converters {

    def simple[T](
        f: SourceFragment => Either[String, T]
    ): (ParsedDirective, SourceFragment) => Either[String, T] =
      (_, value) => f(value)

    def spans: (ParsedDirective, SourceFragment) => Either[String, Seq[Span]] =
      (parsed, value) => parsed.recursiveSpans(value)

    def blocks: (ParsedDirective, SourceFragment) => Either[String, Seq[Block]] =
      (parsed, value) => parsed.recursiveBlocks(value)

  }

  /** The public user API for specifying the required and optional parts of a directive
    *  (arguments, fields or body) together with optional converter/validator functions.
    */
  object Parts {

    import Converters._

    private def requiredPart[T](
        build: DirectiveParserBuilder => (Key, DirectiveParserBuilder),
        converter: (ParsedDirective, SourceFragment) => Either[String, T]
    ) = new DirectivePartBuilder[T] {
      val base = part(build, converter)

      def apply(builder: DirectiveParserBuilder): (DirectiveParserBuilder, DirectivePart[T]) = {
        val (newBuilder, basePart) = base(builder)
        val reqPart                = basePart.flatMap(_.toRight("Missing directive part"))
        (newBuilder, reqPart)
      }

    }

    private def part[T](
        build: DirectiveParserBuilder => (Key, DirectiveParserBuilder),
        converter: (ParsedDirective, SourceFragment) => Either[String, T]
    ) = new DirectivePartBuilder[Option[T]] {

      def apply(
          builder: DirectiveParserBuilder
      ): (DirectiveParserBuilder, DirectivePart[Option[T]]) = {
        val (key, newBuilder) = build(builder)
        val part              = new DirectivePart[Option[T]] {
          def apply(parsed: ParsedDirective): Result[Option[T]] =
            parsed.part(key).map(converter(parsed, _)) match {
              case None                => Right(None)
              case Some(Left(error))   => Left(error)
              case Some(Right(result)) => Right(Some(result))
            }
        }
        (newBuilder, part)
      }

    }

    /** Specifies a required argument.
      *
      *  @param convert the function to use for converting and validating the parsed value
      *  @param withWS whether the argument supports whitespace characters (only one of these
      *  can exist in any single directive markup)
      *  @return a directive part that can be combined with further parts with the `~` operator
      */
    def argument[T](
        convert: SourceFragment => Either[String, T] = { (s: SourceFragment) => Right(s.input) },
        withWS: Boolean = false
    ): DirectivePartBuilder[T] = requiredPart(_.argument(withWS), simple(convert))

    /** Specifies an optional argument.
      *
      *  @param convert the function to use for converting and validating the parsed value
      *  if it is present
      *  @param withWS whether the argument supports whitespace characters (only one of these
      *  can exist in any single directive markup)
      *  @return a directive part that can be combined with further parts with the `~` operator
      */
    def optArgument[T](
        convert: SourceFragment => Either[String, T] = { (s: SourceFragment) => Right(s.input) },
        withWS: Boolean = false
    ): DirectivePartBuilder[Option[T]] = part(_.optArgument(withWS), simple(convert))

    /** Specifies a required named field.
      *
      *  @param name the name of the field as used in the directive markup (without the colons)
      *  @param convert the function to use for converting and validating the parsed value
      *  @return a directive part that can be combined with further parts with the `~` operator
      */
    def field[T](
        name: String,
        convert: SourceFragment => Either[String, T] = { (s: SourceFragment) => Right(s.input) }
    ): DirectivePartBuilder[T] =
      requiredPart(_.field(name), simple(convert))

    /** Specifies an optional named field.
      *
      *  @param name the name of the field as used in the directive markup (without the colons)
      *  @param convert the function to use for converting and validating the parsed value
      *  if it is present
      *  @return a directive part that can be combined with further parts with the `~` operator
      */
    def optField[T](
        name: String,
        convert: SourceFragment => Either[String, T] = { (s: SourceFragment) => Right(s.input) }
    ): DirectivePartBuilder[Option[T]] =
      part(_.optField(name), simple(convert))

    /** Specifies standard block-level content as the body of the directive.
      *
      *  @return a directive part that can be combined with further parts with the `~` operator
      */
    def blockContent: DirectivePartBuilder[Seq[Block]] = requiredPart(_.body, blocks)

    def spanContent: DirectivePartBuilder[Seq[Span]] = requiredPart(_.body, spans)

    def spanArgument: DirectivePartBuilder[Seq[Span]] =
      requiredPart(_.argument(withWS = true), spans)

    def optSpanArgument: DirectivePartBuilder[Option[Seq[Span]]] =
      part(_.optArgument(withWS = true), spans)

    /** Specifies that the body of the directive markup should get passed to the conversion function as a raw string.
      *
      *  @param f the function to use for converting and validating the parsed value
      *  @return a directive part that can be combined with further parts with the `~` operator
      */
    def content[T](f: SourceFragment => Either[String, T]): DirectivePartBuilder[T] =
      requiredPart(_.body, simple(f))

  }

  /** Represents a single directive implementation.
    */
  class Directive[E <: Element] private[Directives] (
      val name: String,
      val part: RecursiveParsers => DirectivePartBuilder[E]
  ) extends RstExtension[DirectivePartBuilder[E]]

  /** API entry point for setting up a span directive that can be used
    *  in substitution definitions.
    */
  object SpanDirective {

    /** Creates a new directive with the specified name and implementation.
      *  The `DirectivePart` can be created by using the methods of the `Parts`
      *  object.
      *
      *  @param name the name the directive can be used with in reStructuredText markup
      *  @param part the implementation of the directive that can be created by using the combinators of the `Parts` object
      *  @return a new directive that can be registered with the reStructuredText parser
      */
    def apply(name: String)(part: DirectivePartBuilder[Span]): Directive[Span] =
      new Directive(name.toLowerCase, _ => part)

    /** Creates a new directive with the specified name and implementation.
      *  The `DirectivePart` can be created by using the methods of the `Parts`
      *  object. In contrast to the `apply` function, this function allows to
      *  depend on the standard block and span parsers. This is necessary if
      *  the directive does both, require a custom parser for arguments or body
      *  and allow for nested directives in those parsers.
      *
      *  @param name the name the directive can be used with in reStructuredText markup
      *  @param part a function returning the implementation of the directive that can be created by using the combinators of the `Parts` object
      *  @return a new directive that can be registered with the reStructuredText parser
      */
    def recursive(name: String)(
        part: RecursiveParsers => DirectivePartBuilder[Span]
    ): Directive[Span] = new Directive(name.toLowerCase, part)

  }

  /** API entry point for setting up a block directive.
    */
  object BlockDirective {

    /** Creates a new directive with the specified name and implementation.
      *  The `DirectivePart` can be created by using the methods of the `Parts`
      *  object.
      *
      *  @param name the name the directive can be used with in reStructuredText markup
      *  @param part the implementation of the directive that can be created by using the combinators of the `Parts` object
      *  @return a new directive that can be registered with the reStructuredText parser
      */
    def apply(name: String)(part: DirectivePartBuilder[Block]): Directive[Block] =
      new Directive(name.toLowerCase, _ => part)

    /** Creates a new directive with the specified name and implementation.
      *  The `DirectivePart` can be created by using the methods of the `Parts`
      *  object. In contrast to the `apply` function, this function allows to
      *  depend on the standard block and span parsers. This is necessary if
      *  the directive does both, require a custom parser for arguments or body
      *  and allow for nested directives in those parsers.
      *
      *  @param name the name the directive can be used with in reStructuredText markup
      *  @param part a function returning the implementation of the directive that can be created by using the combinators of the `Parts` object
      *  @return a new directive that can be registered with the reStructuredText parser
      */
    def recursive(name: String)(
        part: RecursiveParsers => DirectivePartBuilder[Block]
    ): Directive[Block] = new Directive(name.toLowerCase, part)

  }

}
