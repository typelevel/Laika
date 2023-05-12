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

package laika.directive

import cats.{ Functor, Semigroupal }
import laika.ast.{ TemplateSpan, _ }
import laika.collection.TransitionalCollectionOps._
import laika.config.Origin.DirectiveScope
import laika.config._
import laika.parse.SourceFragment
import laika.parse.directive.DirectiveParsers.ParsedDirective
import laika.parse.hocon.ConfigResolver
import laika.parse.markup.{ RecursiveParsers, RecursiveSpanParsers }

import scala.reflect.ClassTag

/** The id for a directive part.
  */
sealed abstract class AttributeKey {
  def key: String
  def desc: String
}

object AttributeKey {

  /** Represents the string identifier of an attribute or body part of a directive.
    */
  case class Named(key: String) extends AttributeKey {
    def desc: String = s"attribute '$key'"
  }

  /** Represents an individual positional attribute at the specified index.
    */
  case class PositionalAt(pos: Int) extends AttributeKey {
    val key          = s"${Positional.key}.$pos"
    def desc: String = s"positional attribute at index $pos"
  }

  /** Represents all positional attributes.
    */
  case object Positional extends AttributeKey {
    val key                        = "__$$:positional:$$__"
    def desc: String               = "positional attributes"
    def at(pos: Int): AttributeKey = PositionalAt(pos)
  }

}

/** Provides the basic building blocks for Laika's Directive API.
  * This trait is not used directly, but instead its three sub-traits `Blocks`, `Spans` and `Templates`,
  * which represent the concrete implementations for the three directive types.
  */
trait BuilderContext[E <: Element] {

  /** The parser API in case a directive function needs to manually parse one of the directive parts.
    */
  type Parser

  protected def parse(parser: Parser, src: SourceFragment): Result[Seq[E]]

  type Result[+A] = Either[Seq[String], A]

  /** The content of a body element divided by separator directives.
    */
  case class Multipart[T](mainBody: Seq[E], children: Seq[T]) {
    def collect[U <: T: ClassTag]: Seq[U] = children.collect { case u: U => u }
  }

  /** The content of a directive part, either an attribute or the body. */
  sealed trait BodyContent

  object BodyContent {

    /** The content of a directive part in its raw, unparsed form. */
    case class Source(value: SourceFragment) extends BodyContent

    /** The parsed content of a directive part. */
    case class Parsed(value: Seq[E]) extends BodyContent
  }

  /** The content of a parsed directive with the HOCON attributes captured in a `Config` instance.
    */
  case class DirectiveContent(attributes: Config, body: Option[BodyContent])

  /** The context of a directive during execution.
    */
  case class DirectiveContext(
      content: DirectiveContent,
      parser: Parser,
      cursor: DocumentCursor,
      source: SourceFragment
  ) {

    val body: Option[BodyContent] = content.body

    def message(error: ConfigError): String = error match {
      case de: DecodingError => de.error // do not include key in message
      case other             => other.message
    }

    def attribute[T](
        id: AttributeKey,
        decoder: ConfigDecoder[T],
        inherit: Boolean
    ): Result[Option[T]] =
      content.attributes
        .getOpt[Traced[T]](id.key)(ConfigDecoder.tracedValue(decoder))
        .map(_.flatMap { traced =>
          if (traced.origin.scope == DirectiveScope || inherit) Some(traced.value)
          else None
        })
        .left.map(e => Seq(s"error converting ${id.desc}: ${message(e)}"))

  }

  /** Represents a single part (attribute or body) of a directive
    * or a combination of multiple parts.
    */
  abstract class DirectivePart[+A] extends (DirectiveContext => Result[A]) { self =>

    def map[B](f: A => B): DirectivePart[B] = new DirectivePart[B] {
      def apply(p: DirectiveContext) = self(p) map f
      def hasBody: Boolean           = self.hasBody
      def needsCursor: Boolean       = self.needsCursor
      def separators: Set[String]    = self.separators
    }

    def evalMap[B](f: A => Either[String, B]): DirectivePart[B] = new DirectivePart[B] {
      def apply(p: DirectiveContext) = self(p).flatMap(f(_).left.map(Seq(_)))
      def hasBody: Boolean           = self.hasBody
      def needsCursor: Boolean       = self.needsCursor
      def separators: Set[String]    = self.separators
    }

    /** Indicates whether the directive supports a body section
      * after the directive name and attribute section.
      */
    def hasBody: Boolean

    /** Indicates whether this part or any of its sub-parts requests access
      * to a cursor.
      * This information is used to determine the default phase a directive
      * should be resolved in.
      */
    def needsCursor: Boolean

    /** The names of the separator directives accepted by this directive part.
      */
    def separators: Set[String]

  }

  object DirectivePart {

    implicit final val directivePartInstances
        : Functor[DirectivePart] with Semigroupal[DirectivePart] =
      new Functor[DirectivePart] with Semigroupal[DirectivePart] {
        override def map[A, B](fa: DirectivePart[A])(f: A => B): DirectivePart[B] = fa.map(f)

        override def product[A, B](
            fa: DirectivePart[A],
            fb: DirectivePart[B]
        ): DirectivePart[(A, B)] = new DirectivePart[(A, B)] {

          def apply(p: DirectiveContext) = (fa(p), fb(p)) match {
            case (Right(a), Right(b)) => Right((a, b))
            case (Left(a), Left(b))   => Left(a ++ b)
            case (Left(msg), _)       => Left(msg)
            case (_, Left(msg))       => Left(msg)
          }

          def hasBody: Boolean        = fa.hasBody || fb.hasBody
          def needsCursor: Boolean    = fa.needsCursor || fb.needsCursor
          def separators: Set[String] = fa.separators ++ fb.separators
        }

      }

  }

  private[laika] trait DirectiveProcessor {

    def typeName: String

    def parsedResult: ParsedDirective

    def name: String = parsedResult.name

    def process[T](
        cursor: DocumentCursor,
        factory: Option[DirectiveContent => Result[T]]
    ): Result[T] = {

      val originPath = cursor.templatePath.getOrElse(cursor.path)
      val origin     = Origin(DirectiveScope, originPath)

      def directiveOrMsg: Result[DirectiveContent => Result[T]] =
        factory.toRight(Seq(s"No $typeName directive registered with name: $name"))

      def attributes: Result[Config] = ConfigResolver
        .resolve(parsedResult.attributes, origin, cursor.config, Map.empty)
        .map(new ObjectConfig(_, origin, cursor.config))
        .left.map(e => Seq(e.message))

      val body = parsedResult.body.map(BodyContent.Source.apply)

      for {
        dir   <- directiveOrMsg
        attrs <- attributes
        res   <- dir(DirectiveContent(attrs, body))
      } yield res

    }

  }

  private[laika] trait DirectiveInstanceBase extends DirectiveProcessor {

    def options: Options

    def directive: Option[Directive]

    def parser: Parser

    def source: SourceFragment

    def createInvalidElement(message: String): E

    def resolve(cursor: DocumentCursor): E = {

      val factory: Option[DirectiveContent => Result[E]] = directive.map { dir => content =>
        dir(DirectiveContext(content, parser, cursor, source))
      }

      process(cursor, factory)
        .fold(
          messages =>
            createInvalidElement(
              s"One or more errors processing directive '$name': "
                + messages.mkString(", ")
            ),
          identity
        )
        .mergeOptions(options)
        .asInstanceOf[E]
    }

    def runsIn(phase: RewritePhase): Boolean = directive.fold(true)(_.runsIn(phase))
  }

  private[laika] trait SeparatorInstanceBase extends DirectiveProcessor {

    val typeName: String = "separator"

    def resolve[T](
        context: DirectiveContext,
        body: Seq[E],
        directive: Option[SeparatorDirective[T]]
    ): Result[T] = {

      val factory: Option[DirectiveContent => Result[T]] = directive.map { dir => content =>
        dir(context.copy(content = content.copy(body = Some(BodyContent.Parsed(body)))))
      }

      process(context.cursor, factory)

    }

    def runsIn(phase: RewritePhase): Boolean = true
  }

  /** Provides combinators to describe the expected structure of a specific directive.
    */
  trait Combinators {

    private def getRawBody(context: DirectiveContext): Option[Result[String]] =
      context.body.map {
        case BodyContent.Source(value) => Right(value.input)
        case BodyContent.Parsed(_)     =>
          Left(Seq(s"unable to retrieve raw body from pre-parsed content"))
      }

    private def getParsedBody(context: DirectiveContext): Option[Result[Seq[E]]] =
      context.body.map {
        case BodyContent.Source(value) => parse(context.parser, value)
        case BodyContent.Parsed(value) => Right(value)
      }

    private def getParsedBody[T](
        parser: Parser => laika.parse.Parser[T]
    )(context: DirectiveContext): Option[Result[T]] =
      context.body.map {
        case BodyContent.Source(value) =>
          parser(context.parser).parse(value).toEither.left.map(Seq(_))
        case BodyContent.Parsed(_)     =>
          Left(Seq(s"unable to use custom body parser with pre-parsed content"))
      }

    private def bodyPart[T](accessor: DirectiveContext => Option[Result[T]]) =
      new DirectivePart[T] {

        def apply(context: DirectiveContext): Result[T] =
          accessor(context).getOrElse(Left(Seq(s"required body is missing")))

        def hasBody: Boolean        = true
        def needsCursor: Boolean    = false
        def separators: Set[String] = Set.empty
      }

    class PositionalAttributes[T](decoder: ConfigDecoder[T]) extends DirectivePart[Seq[T]] {

      def apply(context: DirectiveContext): Result[Seq[T]] =
        context.attribute(AttributeKey.Positional, ConfigDecoder.seq(decoder), inherit = false).map(
          _.getOrElse(Nil)
        )

      def as[U](implicit decoder: ConfigDecoder[U]): PositionalAttributes[U] =
        new PositionalAttributes(decoder)

      def hasBody: Boolean             = false
      def needsCursor: Boolean         = false
      def separators: Set[String]      = Set.empty
      def widen: DirectivePart[Seq[T]] = this
    }

    class AttributePart[T](
        key: AttributeKey,
        decoder: ConfigDecoder[T],
        isInherited: Boolean,
        requiredMsg: => String
    ) extends DirectivePart[T] {

      def apply(context: DirectiveContext): Result[T] =
        context.attribute(key, decoder, isInherited).flatMap(_.toRight(Seq(requiredMsg)))

      def as[U](implicit decoder: ConfigDecoder[U]): AttributePart[U] =
        new AttributePart(key, decoder, isInherited, requiredMsg)

      def inherited: AttributePart[T] = new AttributePart(key, decoder, true, requiredMsg)

      def optional: DirectivePart[Option[T]] = new DirectivePart[Option[T]] {

        def apply(context: DirectiveContext): Result[Option[T]] =
          context.attribute(key, decoder, isInherited)

        def hasBody: Boolean        = false
        def needsCursor: Boolean    = false
        def separators: Set[String] = Set.empty
      }

      def hasBody: Boolean        = false
      def needsCursor: Boolean    = false
      def separators: Set[String] = Set.empty
      def widen: DirectivePart[T] = this
    }

    class SeparatedBodyPart[T](directives: Seq[SeparatorDirective[T]])
        extends DirectivePart[Multipart[T]] {

      def apply(context: DirectiveContext): Result[Multipart[T]] =
        getParsedBody(context).getOrElse(Left(Seq(s"required body is missing"))).flatMap(
          toMultipart(context)
        )

      def hasBody: Boolean     = true
      def needsCursor: Boolean = false

      override def separators: Set[String] = directives.map(_.name).toSet

      def toMultipart(context: DirectiveContext)(elements: Seq[E]): Result[Multipart[T]] = {

        def splitNextBodyPart(remaining: Seq[E]): (Seq[E], Seq[E]) =
          remaining.span(!_.isInstanceOf[SeparatorInstanceBase])

        def processSeparators(
            remaining: Seq[E],
            acc: Seq[Result[(String, T)]]
        ): Seq[Result[(String, T)]] = {
          remaining.headOption
            .collect { case i: SeparatorInstanceBase => i }
            .fold(acc) { instance =>
              val name                 = instance.parsedResult.name
              val (body, newRemaining) = splitNextBodyPart(remaining.tail)
              val nextSeparator        =
                instance.resolve(context, body, directives.find(_.name == name)).map((name, _))
              processSeparators(
                newRemaining,
                acc :+ nextSeparator
                  .left.map(errs =>
                    Seq(
                      s"One or more errors processing separator directive '$name': ${errs.mkString(", ")}"
                    )
                  )
              )
            }
        }

        val (mainBody, remaining)                             = splitNextBodyPart(elements)
        val separators: Seq[Either[Seq[String], (String, T)]] = processSeparators(remaining, Nil)

        val (errors, valid) =
          (separators.collect { case Left(e) => e }, separators.collect { case Right(v) => v })
        val nameCounts      = valid.groupBy(_._1).mapValuesStrict(_.length).withDefaultValue(0)
        val cntErrors       = directives.flatMap { dir =>
          val cnt = nameCounts(dir.name)
          (if (cnt > dir.max)
             Seq(
               s"too many occurrences of separator directive '${dir.name}': expected max: ${dir.max}, actual: $cnt"
             )
           else Nil) ++
            (if (cnt < dir.min)
               Seq(
                 s"too few occurrences of separator directive '${dir.name}': expected min: ${dir.min}, actual: $cnt"
               )
             else Nil)
        }
        if (errors.isEmpty && cntErrors.isEmpty) Right(Multipart(mainBody, valid.map(_._2)))
        else Left(Seq((errors.flatten ++ cntErrors).mkString(", ")))
      }

    }

    private def part[T](f: DirectiveContext => Result[T]) = new DirectivePart[T] {
      def apply(p: DirectiveContext) = f(p)
      def hasBody: Boolean           = false
      def needsCursor: Boolean       = false
      def separators: Set[String]    = Set.empty
    }

    /** Specifies a required attribute from the positional attribute section of the directive.
      *
      * @param position the position within the attribute list
      * @return a directive part that can be combined with further parts
      */
    def attribute(position: Int): AttributePart[ConfigValue] = new AttributePart(
      AttributeKey.Positional.at(position),
      ConfigDecoder.configValue,
      false,
      s"required positional attribute at index $position is missing"
    )

    /** Specifies a required attribute from the HOCON section of the directive.
      *
      * @param key the key that must be used in markup or templates
      * @return a directive part that can be combined with further parts
      */
    def attribute(key: String): AttributePart[ConfigValue] = new AttributePart(
      AttributeKey.Named(key),
      ConfigDecoder.configValue,
      false,
      s"required attribute '$key' is missing"
    )

    /** A combinator that captures all positional attributes in a directive declaration.
      *
      * This is useful when the positional attributes represent a flexible, comma-separated list of values.
      * Using `as` on the directive decodes '''all''' attributes as the same type. To decode with different
      * types, use the combinators for individual positional attributes, e.g. `attribute(0)`.
      */
    def positionalAttributes: PositionalAttributes[ConfigValue] =
      new PositionalAttributes[ConfigValue](ConfigDecoder.configValue)

    /** A combinator that captures all attributes in a directive declaration.
      *
      * This is useful when a directive implementation allows the use of any arbitrary attribute name,
      * but leaves the burden of validation to the implementor of the directive.
      * This part does not provide automatic error handling for missing required attributes for example.
      */
    def allAttributes: DirectivePart[Config] = part(c => Right(c.content.attributes))

    /** Specifies a required body part parsed as spans or blocks, depending on the type of directive.
      *
      * @return a directive part that can be combined with further parts
      */
    def parsedBody: DirectivePart[Seq[E]] = bodyPart(getParsedBody)

    /** Specifies a required body part with a custom parser.
      *
      * The provided parser factory function has to accept a parameter for an instance providing
      * access to the default parser for blocks and spans with all user and theme extensions installed.
      *
      * This is useful for situations where some custom parsing logic has to be combined with
      * the standard block/span parsing rules.
      *
      * This is a fairly rare requirement, and most likely used the zero-param `parsedBody` method
      * will suffice in most cases.
      *
      * @return a directive part that can be combined with further parts
      */
    def parsedBody[T](parser: Parser => laika.parse.Parser[T]): DirectivePart[T] = bodyPart(
      getParsedBody(parser)
    )

    /** Specifies a required body part.
      *
      * @return a directive part that can be combined with further parts
      */
    def rawBody: DirectivePart[String] = bodyPart(getRawBody)

    /** Specifies a required body part divided by separator directives.
      *
      * It is recommended that all separators extend a sealed trait, if the directive
      * supports more than one separator kind. The separators need to be immediate
      * children in the body element of the parent directive.
      *
      * @param separators all separator directives accepted as children of this directive.
      * @return a directive part that can be combined with further parts
      */
    def separatedBody[T](separators: Seq[SeparatorDirective[T]]): DirectivePart[Multipart[T]] =
      new SeparatedBodyPart(separators)

    /** Specifies an empty directive that does not accept any attributes or
      * body elements.
      *
      * @param result the fixed result each empty directive will produce
      * @return a directive part that usually won't be combined with other parts
      */
    def empty[T](result: T): DirectivePart[T] = part(_ => Right(result))

    /** Indicates that access to the document cursor is required.
      * This may be required if the directive relies on information from the document structure,
      * its title or the parent tree it is contained in.
      */
    def cursor: DirectivePart[DocumentCursor] = new DirectivePart[DocumentCursor] {
      def apply(p: DirectiveContext) = Right(p.cursor)
      def hasBody: Boolean           = false
      def needsCursor: Boolean       = true
      def separators: Set[String]    = Set.empty
    }

    /** Indicates that access to the source of the directive is required.
      * This may be required if the directive needs to produce instances of `InvalidElement` for error scenarios,
      * which requires passing the source.
      *
      * This should normally be a rare requirement, as it is more convenient to use `evalMap` on the directive
      * builder and pass simple strings describing any error and let the library insert the corresponding source.
      */
    def source: DirectivePart[SourceFragment] = part(c => Right(c.source))

  }

  /** Represents a directive, its name and its (combined) parts.
    */
  class Directive private[directive] (val name: String, part: DirectivePart[E]) {
    def apply(context: DirectiveContext): Result[E] = part(context)
    def hasBody: Boolean                            = part.hasBody
    def separators: Set[String]                     = part.separators

    def runsIn(phase: RewritePhase): Boolean = phase match {
      case RewritePhase.Render(_) => true
      case _                      => !part.needsCursor
    }

  }

  /** Represents a separator directive, its name and its (combined) parts.
    * It also allows to specify requirements for the minimum and maximum number of occurrences allowed for this directive.
    * The default is unbounded, with 0 or more instances allowed.
    */
  class SeparatorDirective[+T] private[directive] (
      val name: String,
      part: DirectivePart[T],
      val min: Int = 0,
      val max: Int = Int.MaxValue
  ) {
    def apply(context: DirectiveContext): Result[T] = part(context)
  }

  /** Creates a new directive with the specified name and part specification.
    *
    * When the result of the directive is a `Left`, the directive will produce
    * an invalid AST element with the string as the error message.
    */
  def eval(name: String)(part: DirectivePart[Either[String, E]]): Directive =
    new Directive(name, part.evalMap(identity))

  /** Creates a new directive with the specified name and part specification.
    */
  def create(name: String)(part: DirectivePart[E]): Directive = new Directive(name, part)

  /** Creates a new separator directive with the specified name and part specification.
    */
  def separator[T](name: String, min: Int = 0, max: Int = Int.MaxValue)(
      part: DirectivePart[T]
  ): SeparatorDirective[T] = new SeparatorDirective(name, part, min, max)

  /** Turns a collection of directives into a map,
    * using the name of the directive as the key.
    */
  def toMap(directives: Iterable[Directive]): Map[String, Directive] =
    directives.map(dir => (dir.name, dir)).toMap

  /**  Provides the basic building blocks for defining directives, Laika's extension
    *  mechanism for creating custom tags for both, templates or text markup.
    *
    *  This object is used as part of the concrete objects  `Blocks.dsl`,
    *  `Spans.dsl` and `Templates.dsl` respectively.
    *
    *  It contains several simple combinators that allow to specify the expected
    *  attributes and body elements of the directive, optional converters for these
    *  elements and the function responsible for producing the final node element.
    *
    *  In contrast to custom tag hooks in other template engines the result of
    *  a directive is not a string. In the same way as markup documents get
    *  transformed into a tree of elements before rendering, a directive produces
    *  a node of the tree to render. As a result, the directive can be used
    *  independent from the output format.
    *
    *  Entry points of the API are the `Templates`, `Blocks` and `Spans` objects for the
    *  three different directive types.
    *
    *  A directive may consist of any combination of attributes and body elements:
    *
    *  {{{
    *  @:myDirective { arg1 = value1, arg2 = value2 }
    *
    *  This is the body of the directive. It may consist of any standard or custom
    *  block-level and inline markup.
    *
    *  @:@
    *  }}}
    *
    *  In the example above `arg1` and `arg2` are attributes, followed by a body element
    *  enclosed in curly braces.
    *
    *  For each of these directive elements, the API offers a combinator to specify whether the
    *  element is required or optional, and an optional function to convert.
    *
    *  Consider the following simple example of a directive with just one argument and
    *  a body, for specifying a specially formatted inline note:
    *
    *  {{{
    *  @:note { This is the title }
    *
    *  This is the body of the note.
    *
    *  @:@
    *  }}}
    *
    *  The implementation of this directive could look like this:
    *
    *  {{{
    *  case class Note (title: String, content: Seq[Block], options: Options = NoOpt)
    *                                                      extends Block with BlockContainer[Note]
    *
    *  object MyDirectives extends DirectiveRegistry {
    *    val blockDirectives = Seq(
    *      Blocks.create("note") {
    *        (defaultAttribute.as[String], parsedBody).mapN(Note(_,_))
    *      }
    *    )
    *    val spanDirectives = Seq()
    *  }
    *
    *  val transformer = Transformer.from(Markdown).to(HTML).using(MyDirectives)
    *  }}}
    *
    *  The `defaultAttribute` combinator specifies a required attribute of type `String`
    *  and without a name. The `parsedBody` combinator specifies standard block content (any block
    *  elements that are supported in normal markup, too) which results in a parsed value of type
    *  `Seq[Block]`.
    *
    *  Finally you need to provide a function that accepts the results of the specified
    *  directive elements as parameters (of the corresponding type). Here we created a case class
    *  with a matching signature so can pass it directly as the target function. For a block directive
    *  the final result has to be of type `Block` which the `Note` class satisfies. Finally the directive
    *  gets registered with the `Markdown` parser. It can be registered for a `reStructuredText` parser,
    *  too, without any changes.
    *
    *  If any conversion of attributes is required it can be performed with the `as[T]` method:
    *
    *  {{{
    *  case class Message (severity: Int,
    *                      content: Seq[Block],
    *                      options: Options = NoOpt) extends Block
    *                                                with BlockContainer[Message]
    *
    *  val blockDirectives = Seq(
    *    Blocks.create("message") {
    *      (defaultAttribute.as[Int], blockContent).mapN(Message(_,_))
    *    }
    *  )
    *  }}}
    *
    *  In the example above the built-in `Int` decoder gets passed to the `defaultAttribute`
    *  combinator, but you can easily create and use your own instances of `ConfigDecoder[T]`.
    *
    *  If required attributes or bodies are missing or any type conversion fails,
    *  an instance of `InvalidBlock` containing the error message and the raw source of the directive
    *  will be inserted into the document tree. In this case the final function (`Message`) will never be invoked.
    *
    *  Finally attributes can also be optional. In case they are missing, the directive is still
    *  considered valid and `None` will be passed to your function:
    *
    *  {{{
    *  case class Message (severity: Int,
    *                      content: Seq[Block],
    *                      options: Options = NoOpt) extends Block
    *                                                with BlockContainer[Message]
    *
    *  val blockDirectives = Seq(
    *    Blocks.create("message") {
    *      (defaultAttribute.as[Int].optional, blockContent).mapN {
    *        (severity, content) => Message(severity.getOrElse(0), content)
    *      }
    *    }
    *  )
    *  }}}
    *
    *  The attribute may be missing, but if it is present it has to pass the specified validator.
    */
  object dsl extends Combinators

}

/** The API for declaring directives that can be used
  *  as inline elements in markup documents.
  */
object Spans extends BuilderContext[Span] {

  type Parser = RecursiveSpanParsers

  protected def parse(parser: Parser, src: SourceFragment): Result[Seq[Span]] =
    parser.recursiveSpans.parse(src).toEither.left.map(Seq(_))

  case class DirectiveInstance(
      directive: Option[Directive],
      parsedResult: ParsedDirective,
      parser: RecursiveSpanParsers,
      source: SourceFragment,
      rewriteRules: RewriteRules = RewriteRules.empty,
      options: Options = NoOpt
  ) extends SpanResolver with RewritableContainer with DirectiveInstanceBase {
    type Self = DirectiveInstance
    val typeName: String                                 = "span"
    def withOptions(options: Options): DirectiveInstance = copy(options = options)

    def rewriteChildren(rules: RewriteRules): DirectiveInstance =
      copy(rewriteRules = rewriteRules ++ rules)

    override def resolve(cursor: DocumentCursor): Span =
      rewriteRules.rewriteSpan(super.resolve(cursor))

    def createInvalidElement(message: String): Span = InvalidSpan(message, source)

    lazy val unresolvedMessage: String =
      s"Unresolved span directive instance with name '${directive.fold("<unknown>")(_.name)}'"

  }

  case class SeparatorInstance(
      parsedResult: ParsedDirective,
      source: SourceFragment,
      options: Options = NoOpt
  ) extends Span with SeparatorInstanceBase with SpanResolver {
    type Self = SeparatorInstance
    def withOptions(options: Options): SeparatorInstance = copy(options = options)

    def resolve(cursor: DocumentCursor): Span =
      InvalidSpan(s"Orphaned separator directive with name '${parsedResult.name}'", source)

    lazy val unresolvedMessage: String =
      s"Unresolved separator directive instance with name '${parsedResult.name}'"

  }

}

/** The API for declaring directives that can be used as block elements in markup documents.
  */
object Blocks extends BuilderContext[Block] {

  type Parser = RecursiveParsers

  protected def parse(parser: Parser, src: SourceFragment): Result[Seq[Block]] =
    parser.recursiveBlocks.parse(src).toEither.left.map(Seq(_))

  case class DirectiveInstance(
      directive: Option[Directive],
      parsedResult: ParsedDirective,
      parser: RecursiveParsers,
      source: SourceFragment,
      rewriteRules: RewriteRules = RewriteRules.empty,
      options: Options = NoOpt
  ) extends BlockResolver with RewritableContainer with DirectiveInstanceBase {
    type Self = DirectiveInstance
    val typeName: String                                 = "block"
    def withOptions(options: Options): DirectiveInstance = copy(options = options)

    def rewriteChildren(rules: RewriteRules): DirectiveInstance =
      copy(rewriteRules = rewriteRules ++ rules)

    override def resolve(cursor: DocumentCursor): Block =
      rewriteRules.rewriteBlock(super.resolve(cursor))

    def createInvalidElement(message: String): Block = InvalidBlock(message, source)

    lazy val unresolvedMessage: String =
      s"Unresolved block directive instance with name '${directive.fold("<unknown>")(_.name)}'"

  }

  case class SeparatorInstance(
      parsedResult: ParsedDirective,
      source: SourceFragment,
      options: Options = NoOpt
  ) extends Block with SeparatorInstanceBase with BlockResolver {
    type Self = SeparatorInstance
    def withOptions(options: Options): SeparatorInstance = copy(options = options)

    def resolve(cursor: DocumentCursor): Block =
      InvalidBlock(s"Orphaned separator directive with name '${parsedResult.name}'", source)

    lazy val unresolvedMessage: String =
      s"Unresolved separator directive instance with name '${parsedResult.name}'"

  }

}

/** The API for declaring directives that can be used in templates.
  */
object Templates extends BuilderContext[TemplateSpan] {

  type Parser = RecursiveSpanParsers // TODO - specialize to TemplateSpan?

  protected def parse(parser: Parser, src: SourceFragment): Result[Seq[TemplateSpan]] =
    parser.recursiveSpans.parse(src).map {
      _.collect {
        case s: TemplateSpan => s
        case Text(s, opt)    => TemplateString(s, opt) // TODO - might get extracted
      }
    }.toEither.left.map(Seq(_))

  case class DirectiveInstance(
      directive: Option[Directive],
      parsedResult: ParsedDirective,
      parser: RecursiveSpanParsers,
      source: SourceFragment,
      options: Options = NoOpt
  ) extends SpanResolver with TemplateSpan with DirectiveInstanceBase {
    type Self = DirectiveInstance
    val typeName: String                                 = "template"
    def withOptions(options: Options): DirectiveInstance = copy(options = options)

    def createInvalidElement(message: String): TemplateSpan = TemplateElement(
      InvalidSpan(message, source)
    )

    lazy val unresolvedMessage: String =
      s"Unresolved template directive instance with name '${directive.fold("<unknown>")(_.name)}'"

  }

  case class SeparatorInstance(
      parsedResult: ParsedDirective,
      source: SourceFragment,
      options: Options = NoOpt
  ) extends TemplateSpan with SeparatorInstanceBase with SpanResolver {
    type Self = SeparatorInstance
    def withOptions(options: Options): SeparatorInstance = copy(options = options)

    def resolve(cursor: DocumentCursor): TemplateSpan =
      TemplateElement(
        InvalidSpan(s"Orphaned separator directive with name '${parsedResult.name}'", source)
      )

    lazy val unresolvedMessage: String =
      s"Unresolved separator directive instance with name '${parsedResult.name}'"

  }

}

/** The API for declaring directives that can be used in links.
  */
object Links {

  /** A directive that knows how to take a string identifier and turn it into
    * a span link.
    */
  trait Directive {

    /** The name of the directive, as it is used in text markup. */
    def name: String

    /** Turns the link identifier as specified by the user in text markup into a span link.
      * If the link id is invalid this method should return a left with a textual description
      * of the problem.
      */
    def apply(linkId: String, cursor: DocumentCursor): Either[String, SpanLink]

    /** Turns the link directive into a regular span directive. */
    def asSpanDirective: Spans.Directive = Spans.eval(name) {
      import Spans.dsl._
      import cats.implicits._
      (attribute(0).as[String], cursor).mapN(apply)
    }

  }

  /** Creates a new link directive with the specified name and implementation.
    *
    * The specified function receives the string used in the directive instance in text markup.
    */
  def create(directiveName: String)(f: (String, DocumentCursor) => SpanLink): Directive =
    eval(directiveName)((s, c) => Right(f(s, c)))

  /** Creates a new link directive with the specified name and implementation.
    *
    * The specified function receives the string used in the directive instance in text markup.
    * When the result of the function call is a `Left`, the directive will produce
    * an invalid AST element with the string as the error message.
    */
  def eval(
      directiveName: String
  )(f: (String, DocumentCursor) => Either[String, SpanLink]): Directive = new Directive {
    override def name = directiveName

    override def apply(linkId: String, cursor: DocumentCursor): Either[String, SpanLink] =
      f(linkId, cursor).map(_.withStyles(directiveName))

  }

}
