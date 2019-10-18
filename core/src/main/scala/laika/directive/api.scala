/*
 * Copyright 2013-2018 the original author or authors.
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

import laika.api.config.{Config, ObjectConfig}
import laika.ast.{TemplateSpan, _}
import laika.collection.TransitionalCollectionOps._
import laika.parse.directive.DirectiveParsers.ParsedDirective
import laika.parse.hocon.ConfigResolver
import laika.parse.hocon.HoconParsers.Origin
import laika.parse.{Failure, Success}

import scala.reflect.ClassTag

/** The id for a directive part.
  */
sealed abstract class PartId {
  def key: String
  def desc: String
}

object PartId {

  /** Represents the string identifier of an attribute or body part
    *  of a directive.
    */
  case class Named (key: String) extends PartId {
    def desc: String = s"attribute with name '$key'"
  }

  implicit def stringToId (str: String): PartId = Named(str)

  /** Represents an unnamed attribute or body part
    *  of a directive.
    */
  case object Default extends PartId {
    val key = "__$$default$$__"
    def desc: String = s"default attribute"
  }

}

sealed abstract class Key (keyType: String) {
  def id: PartId
  def desc: String = id.desc
}

case class Attribute (id: PartId) extends Key("attribute")

case object Body extends Key("body") {
  val id: PartId = PartId.Default
}


/** Provides the basic building blocks for
  * Laika's Directive API. This trait
  * is not used directly, but instead its
  * three sub-traits `Blocks`, `Spans` and `Templates`,
  * which represent the concrete implementations
  * for the three directive types.
  */
trait BuilderContext[E <: Element] {

  /** The parser API in case a directive function
    * needs to manually parse one of the directive
    * parts.
    */
  type Parser <: String => Seq[E]
  
  
  type Result[+A] = Either[Seq[String], A]

  /** The content of a body element divided by separator directives.
    */
  case class Multipart[T](mainBody: Seq[E], children: Seq[T]) {
    def collect[U <: T : ClassTag]: Seq[U] = children.collect { case u:U => u }
  }

  /** The content of a directive part, either an attribute or the body. */
  sealed trait BodyContent
  object BodyContent {
    /** The content of a directive part in its raw, unparsed form. */
    case class Source(value: String) extends BodyContent
    /** The parsed content of a directive part. */
    case class Parsed(value: Seq[E]) extends BodyContent
  }
  
  case class DirectiveContent (attributes: Config, body: Option[BodyContent])

  /** The context of a directive during execution.
    */
  case class DirectiveContext (content: DirectiveContent, parser: Parser, cursor: DocumentCursor) {

    def part (key: Key): Option[BodyContent] = key match {
      case Body => content.body
      case Attribute(id) => content.attributes.get[String](id.key).toOption.map(BodyContent.Source) // TODO - 0.12 - error handling
    }

  }

  /** Represents a single part (attribute or body) of a directive
    * or a combination of multiple parts.
    */
  abstract class DirectivePart[+A] extends (DirectiveContext => Result[A]) { self =>

    def map [B](f: A => B): DirectivePart[B] = new DirectivePart[B] {
      def apply (p: DirectiveContext) = self(p) map f
      def hasBody: Boolean = self.hasBody
      def separators: Set[String] = self.separators
    }

    def ~ [B] (other: DirectivePart[B]): DirectivePart[A ~ B] = new DirectivePart[A ~ B] {
      def apply (p: DirectiveContext) = (self(p), other(p)) match {
        case (Right(a), Right(b)) => Right(new ~(a, b))
        case (Left(a), Left(b)) => Left(a ++ b)
        case (Left(msg), _) => Left(msg)
        case (_, Left(msg)) => Left(msg)
      }
      def hasBody: Boolean = other.hasBody || self.hasBody
      def separators: Set[String] = other.separators ++ self.separators
    }

    /** Indicates whether the directive supports a body section
      * after the directive name and attribute section.
      */
    def hasBody: Boolean

    /** The names of the separator directives accepted by this directive part.
      */
    def separators: Set[String]

  }
  
  private[laika] trait DirectiveProcessor {

    def typeName: String

    def parsedResult: ParsedDirective

    def name: String = parsedResult.name

    def process[T] (cursor: DocumentCursor, factory: Option[DirectiveContent => Result[T]]): Result[T] = {

      def directiveOrMsg: Result[DirectiveContent => Result[T]] =
        factory.toRight(Seq(s"No $typeName directive registered with name: $name"))
      
      def attributes: Result[Config] = Right(ConfigResolver
        .resolve(parsedResult.attributes, cursor.config))
        .map(new ObjectConfig(_, Origin(cursor.path), cursor.config))
      
      val body = parsedResult.body.map(BodyContent.Source)

      for {
        dir   <- directiveOrMsg
        attrs <- attributes
        res   <- dir(DirectiveContent(attrs, body))
      } yield res

    }
    
  }

  private[laika] trait DirectiveInstanceBase extends DirectiveProcessor {
    
    def directive: Option[Directive]
    
    def parser: Parser

    def createInvalidElement (message: String): E

    def resolve (cursor: DocumentCursor): E = {

      val factory: Option[DirectiveContent => Result[E]] = directive.map { dir =>
        content => dir(DirectiveContext(content, parser, cursor))
      }

      process(cursor, factory).fold(messages => createInvalidElement(s"One or more errors processing directive '$name': "
        + messages.mkString(", ")), identity)
    }

  }

  private[laika] trait SeparatorInstanceBase extends DirectiveProcessor {

    val typeName: String = "separator"
    
    def resolve[T] (context: DirectiveContext, body: Seq[E], directive: Option[SeparatorDirective[T]]): Result[T] = {

      val factory: Option[DirectiveContent => Result[T]] = directive.map { dir =>
        content => dir(context.copy(content = content.copy(body = Some(BodyContent.Parsed(body)))))
      }

      process(context.cursor, factory)
      
    }
    
  }

  trait IdBuilders {

    val Default = PartId.Default

    implicit def stringToId (str: String): PartId = PartId.stringToId(str)

  }

  type Converter[T] = (Parser, String) => Result[T]

  /** Provides various converter functions that can be used with the directive
    * combinators to convert the string value obtained from a directive attribute or body.
    */
  trait Converters {

    val string: Converter[String] = (_, input) => Right(input)

    val parsed: Converter[Seq[E]] = (parser, input) => Right(parser(input))

    val int: Converter[Int] = (_, input) => toInt(input, _ => true)

    val positiveInt: Converter[Int] = (_, input) => toInt(input, _ > 0, "not a positive integer")

    val nonNegativeInt: Converter[Int] = (_, input) => toInt(input, _ >= 0, "not a non-negative integer")

    private def toInt (input: String, predicate: Int => Boolean, msg: String = ""): Result[Int] = {
      try {
        val i = input.trim.toInt
        if (predicate(i)) Right(i) else Left(Seq(s"$msg: $i"))
      } catch {
        case e: NumberFormatException => Left(Seq(s"not an integer: $input"))
      }
    }

  }

  /** Provides various combinators to describe the expected
    * format of a specific directive.
    */
  trait Combinators {
    
    private def convert [T] (context: DirectiveContext, key: Key, converter: Converter[T]): Option[Result[T]] = 
      context.part(key).map {
        case BodyContent.Source(value) => converter(context.parser, value)
        case BodyContent.Parsed(_) => Left(Seq(s"error converting ${key.desc}: expected raw string element"))
      }

    private def convertBody (context: DirectiveContext): Option[Result[Seq[E]]] =
      context.part(Body).map {
        case BodyContent.Source(value) => dsl.parsed(context.parser, value)
        case BodyContent.Parsed(value) => Right(value)
      }

    private def bodyPart [T] (converter: Converter[T]) = new DirectivePart[T] {
      def apply (context: DirectiveContext) = 
        convert(context, Body, converter).getOrElse(Left(Seq(s"required body is missing")))
      def hasBody: Boolean = true
      def separators: Set[String] = Set.empty
    }

    private def parsedBodyPart = new DirectivePart[Seq[E]] {
      def apply (context: DirectiveContext) = convertBody(context).getOrElse(Left(Seq(s"required body is missing")))
      def hasBody: Boolean = true
      def separators: Set[String] = Set.empty
    }

    class AttributePart [T] (key: Key, converter: Converter[T], msg: => String) extends DirectivePart[T] {
      def apply (context: DirectiveContext) = convert(context, key, converter).getOrElse(Left(Seq(msg)))

      def optional: DirectivePart[Option[T]] = new DirectivePart[Option[T]] {
        def apply (context: DirectiveContext) = convert(context, key, converter) match {
          case Some(Right(value)) => Right(Some(value))
          case Some(Left(msg))    => Left(Seq(s"error converting ${key.desc}: " + msg.mkString(", ")))
          case None               => Right(None)
        }
        def hasBody: Boolean = false
        def separators: Set[String] = Set.empty
      }
      def hasBody: Boolean = false
      def separators: Set[String] = Set.empty
    }
    
    class SeparatedBodyPart[T] (directives: Seq[SeparatorDirective[T]]) extends DirectivePart[Multipart[T]] {
      
      def apply (context: DirectiveContext): Result[Multipart[T]] = 
        convertBody(context).getOrElse(Left(Seq(s"required body is missing"))).flatMap(toMultipart(context))
      
      def hasBody: Boolean = true
      
      override def separators: Set[String] = directives.map(_.name).toSet
      
      def toMultipart (context: DirectiveContext)(elements: Seq[E]): Result[Multipart[T]] = {
        
        def splitNextBodyPart(remaining: Seq[E]): (Seq[E], Seq[E]) = remaining.span(!_.isInstanceOf[SeparatorInstanceBase])
        
        def processSeparators(remaining: Seq[E], acc: Seq[Result[(String,T)]]): Seq[Result[(String, T)]] = {
          remaining.headOption
            .collect { case i: SeparatorInstanceBase => i }
            .fold(acc) { instance =>
              val name = instance.parsedResult.name
              val (body, newRemaining) = splitNextBodyPart(remaining.tail)
              val nextSeparator = instance.resolve(context, body, directives.find(_.name == name)).map((name, _))
              processSeparators(newRemaining, acc :+ nextSeparator
                .left.map(errs => Seq(s"One or more errors processing separator directive '$name': ${errs.mkString(", ")}")))
            }
        }
        
        val (mainBody, remaining) = splitNextBodyPart(elements)
        val separators: Seq[Either[Seq[String], (String, T)]] = processSeparators(remaining, Nil)
        
        val (errors, valid) = (separators.collect{case Left(e) => e}, separators.collect{case Right(v) => v})
        val nameCounts = valid.groupBy(_._1).mapValuesStrict(_.length).withDefaultValue(0)
        val cntErrors = directives.flatMap { dir =>
          val cnt = nameCounts(dir.name)
          (if (cnt > dir.max) Seq(s"too many occurrences of separator directive '${dir.name}': expected max: ${dir.max}, actual: $cnt") else Nil) ++ 
            (if (cnt < dir.min) Seq(s"too few occurrences of separator directive '${dir.name}': expected min: ${dir.min}, actual: $cnt") else Nil)
        }
        if (errors.isEmpty && cntErrors.isEmpty) Right(Multipart(mainBody, valid.map(_._2)))
        else Left(Seq((errors.flatten ++ cntErrors).mkString(", ")))
      }
      
    }

    private def part [T](f: DirectiveContext => Result[T]) = new DirectivePart[T] {
      def apply (p: DirectiveContext) = f(p)
      def hasBody: Boolean = false
      def separators: Set[String] = Set.empty
    }

    /** Specifies a required attribute.
      *
      * @param id        the identifier that must be used in markup or templates
      * @param converter the function to use for converting and validating the parsed value
      * @return a directive part that can be combined with further parts with the `~` operator
      */
    def attribute [T](id: PartId, converter: Converter[T] = dsl.string): AttributePart[T]
    = new AttributePart(Attribute(id), converter, s"required ${Attribute(id).desc} is missing")

    /** Specifies a required body part.
      *
      * @return a directive part that can be combined with further parts with the `~` operator
      */
    def body: DirectivePart[Seq[E]] = parsedBodyPart
    
    /** Specifies a required body part.
      *
      * @param converter the function to use for converting and validating the parsed value
      * @return a directive part that can be combined with further parts with the `~` operator
      */
    def body [T](converter: Converter[T]): DirectivePart[T] = bodyPart(converter)

    /** Specifies a required body part divided by separator directives.
      * 
      * It is recommended that all separators extend a sealed trait, if the directive
      * supports more than one separator kind. The separators need to be immediate
      * children in the body element of the parent directive.
      *
      * @param separators all separator directives accepted as children of this directive.
      * @return a directive part that can be combined with further parts with the `~` operator
      */
    def separatedBody[T] (separators: Seq[SeparatorDirective[T]]): DirectivePart[Multipart[T]] =
      new SeparatedBodyPart(separators)

    /** Specifies an empty directive that does not accept any attributes or
      * body elements.
      *
      * @param result the fixed result each empty directive will produce
      * @return a directive part that usually won't be combined with other parts
      */
    def empty [T] (result: T): DirectivePart[T] = part(_ => Right(result))

    /** Indicates that access to the parser responsible for this directive
      * is needed, in case the directive implementation has to manually
      * parse parts or all of its result.
      *
      * The advantage of using the parser provided by the runtime versus
      * creating your own is only this provided parser can now all other
      * registered extensions in case your directive content may contain
      * other directives.
      */
    def parser: DirectivePart[Parser] = part(c => Right(c.parser))

    /** Indicates that access to the document cursor is required.
      * This may be required if the directive relies on information
      * from the document structure, its title or the parent tree
      * it is contained in.
      *
      * Use of this function causes the directive to be processed in a later
      * rewrite step as the document cursor is not yet fully populated in
      * the initial rewrite step. But this is an implementation detail
      * you normally do not need to deal with.
      */
    def cursor: DirectivePart[DocumentCursor] = part(c => Right(c.cursor))

  }

  /** Represents a directive, its name and its (combined) parts.
    */
  class Directive private[directive] (val name: String, part: DirectivePart[E]) {
    def apply (context: DirectiveContext): Result[E] = part(context)
    def hasBody: Boolean = part.hasBody
    def separators: Set[String] = part.separators
  }

  /** Represents a separator directive, its name and its (combined) parts.
    * It also allows to specify requirements for the minimum and maximum number of occurrences allowed for this directive.
    * The default is unbounded, with 0 or more instances allowed.
    */
  class SeparatorDirective[+T] private[directive] (val name: String, part: DirectivePart[T], val min: Int = 0, val max: Int = Int.MaxValue) {
    def apply (context: DirectiveContext): Result[T] = part(context)
  }

  /** Creates a new directive with the specified name and part specification.
    */
  def create (name: String)(part: DirectivePart[E]): Directive = new Directive(name, part)

  /** Creates a new separator directive with the specified name and part specification.
    */
  def separator[T] (name: String, min: Int = 0, max: Int = Int.MaxValue)(part: DirectivePart[T]): SeparatorDirective[T] = new SeparatorDirective(name, part, min, max)

  /** Turns a collection of directives into a map,
    *  using the name of the directive as the key.
    */
  def toMap (directives: Iterable[Directive]): Map[String, Directive] = directives map (dir => (dir.name, dir)) toMap

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
    *  A directive may consist of any combination of arguments, fields and body elements:
    *
    *  {{{
    *  @:myDirective arg1=value1 arg2=value2: {
    *    This is the body of the directive. It may consist of any standard or custom
    *    block-level and inline markup.
    *  }
    *  }}}
    *
    *  In the example above `arg1` and `arg2` are arguments, ollowed by a body element
    *  enclosed in curly braces.
    *
    *  For each of these directive elements, the API offers a combinator to specify whether the
    *  element is required or optional, and an optional function to convert or validate the
    *  parsed value.
    *
    *  Consider the following simple example of a directive with just one argument and
    *  a body, for specifying a specially formatted inline note:
    *
    *  {{{
    *  @:note This is the title: { This is the body of the note. }
    *  }}}
    *
    *  The implementation of this directive could look like this:
    *
    *  {{{
    *  case class Note (title: String, content: Seq[Span], options: Options = NoOpt)
    *                                                      extends Span with SpanContainer[Note]
    *
    *  object MyDirectives extends DirectiveRegistry {
    *    val spanDirectives = Seq(
    *      Spans.create("note") {
    *        (attribute(Default) ~ body).map { 
    *          case title ~ content => Note(title, content) 
    *        }
    *      }
    *    )
    *    val blockDirectives = Seq()
    *  }
    *
    *  Transformer.from(Markdown).to(HTML).using(MyDirectives) fromFile "hello.md" toFile "hello.html"
    *  }}}
    *
    *  The `attribute(Default)` combinator specifies a required attribue of type `String` (since no conversion
    *  function was supplied) and without a name (indicated by passing the `Default` object instead of a string
    *  name). The `body` combinator specifies standard inline content (any span
    *  elements that are supported in normal inline markup, too) which results in a parsed value of type
    *  `Seq[Span]`.
    *
    *  Finally you need to provide a function that accepts the results of the specified
    *  directive elements as parameters (of the corresponding type). Here we created a case class
    *  with a matching signature so can pass it directly as the target function. For a span directive
    *  the final result has to be of type `Span` which the `Note` class satisfies. Finally the directive
    *  gets registered with the `Markdown` parser. It can be registered for a `reStructuredText` parser,
    *  too, without any changes.
    *
    *  If any conversion or validation is required on the individual parts of the directive they can
    *  be passed to the corresponding function:
    *
    *  {{{
    *  case class Message (severity: Int,
    *                      content: Seq[Block],
    *                      options: Options = NoOpt) extends Block
    *                                                with BlockContainer[Message]
    *
    *  val blockDirectives = Seq(
    *    Blocks.create("message") {
    *      (attribute(Default, positiveInt) ~ blockContent).map { 
    *        case severity ~ content => Message(severity, content) 
    *      }
    *    }
    *  )
    *  }}}
    *
    *  In the example above the built-in `positiveInt` converter gets passed to the `attribute`
    *  combinator, but you can easily create and use your own functions.
    *  The function has to accept a string argument and return a `Result[T]`.
    *
    *  The `Failure` subtype of `Result` will be interpreted as an error by the interpreter with the string being used as the message
    *  and an instance of `InvalidBlock` containing the validator message and the raw source of the directive
    *  will be inserted into the document tree. In this case the final function (`Message`) will never be invoked.
    *
    *  The `Success` subtype of `Result` will be used as an argument to the final function. Note how the case class now expects
    *  an `Int` as the first parameter.
    *
    *  Finally attributes and body elements can also be optional. In case they are missing, the directive is still
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
    *      (attribute(Default, positiveInt).optional ~ blockContent).map { 
    *        case severity ~ content => Message(severity.getOrElse(0), content) 
    *      }
    *    }
    *  )
    *  }}}
    *
    *  The attribute may be missing, but if it is present it has to pass the specified validator.
    */
  object dsl extends Combinators with Converters with IdBuilders

}

/** The API for declaring directives that can be used
  *  as inline elements in markup documents.
  */
object Spans extends BuilderContext[Span] {

  trait Parser extends (String => Seq[Span]) {
    def apply (source: String): Seq[Span]
  }

  case class DirectiveInstance (directive: Option[Directive],
                                parsedResult: ParsedDirective,
                                recursiveParser: String => List[Span],
                                source: String,
                                options: Options = NoOpt) extends SpanResolver with DirectiveInstanceBase {
    type Self = DirectiveInstance
    val typeName: String = "span"
    val parser: Parser = new Parser {
      def apply (source: String): Seq[Span] = recursiveParser(source)
    }
    def withOptions (options: Options): DirectiveInstance = copy(options = options)
    def createInvalidElement (message: String): Span = InvalidElement(message, "@"+source).asSpan
  }

  case class SeparatorInstance (parsedResult: ParsedDirective,
                                source: String,
                                options: Options = NoOpt) extends Span with SeparatorInstanceBase with SpanResolver {
    type Self = SeparatorInstance
    def withOptions (options: Options): SeparatorInstance = copy(options = options)
    def resolve (cursor: DocumentCursor): Span =
      InvalidElement(s"Orphaned separator directive with name '${parsedResult.name}'", "@" + source).asSpan
  }
  
}

/** The API for declaring directives that can be used
  *  as block elements in markup documents.
  */
object Blocks extends BuilderContext[Block] {

  trait Parser extends (String => Seq[Block]) {
    def apply (source: String): Seq[Block]
    def parseInline (source: String): Seq[Span]
  }

  case class DirectiveInstance (directive: Option[Directive],
                                parsedResult: ParsedDirective,
                                recursiveBlockParser: String => Seq[Block],
                                recursiveSpanParser: String => Seq[Span],
                                source: String,
                                options: Options = NoOpt) extends BlockResolver with DirectiveInstanceBase {
    type Self = DirectiveInstance
    val typeName: String = "block"
    val parser: Parser = new Parser {
      def apply (source: String): Seq[Block] = recursiveBlockParser(source)
      def parseInline (source: String): Seq[Span] = recursiveSpanParser(source)
    }
    def withOptions (options: Options): DirectiveInstance = copy(options = options)
    def createInvalidElement (message: String): Block = InvalidElement(message, s"@$source").asBlock
  }
  
  case class SeparatorInstance (parsedResult: ParsedDirective,
                                source: String,
                                options: Options = NoOpt) extends Block with SeparatorInstanceBase with BlockResolver {
    type Self = SeparatorInstance
    def withOptions (options: Options): SeparatorInstance = copy(options = options)
    def resolve (cursor: DocumentCursor): Block =
      InvalidElement(s"Orphaned separator directive with name '${parsedResult.name}'", "@" + source).asBlock
  }

}

/** The API for declaring directives that can be used
  *  in templates.
  */
object Templates extends BuilderContext[TemplateSpan] {

  trait Parser extends (String => Seq[TemplateSpan]) {
    def apply (source: String): Seq[TemplateSpan]
  }

  case class DirectiveInstance (directive: Option[Directive],
                                parsedResult: ParsedDirective,
                                recursiveParser: laika.parse.Parser[List[TemplateSpan]],
                                source: String,
                                options: Options = NoOpt) extends SpanResolver with TemplateSpan with DirectiveInstanceBase {
    type Self = DirectiveInstance
    val typeName: String = "template"
    val parser: Parser = new Parser {
      def apply(source: String): Seq[TemplateSpan] = recursiveParser.parse(source) match {
        case Success(spans, _)  => spans
        case Failure(msg, next) => List(InvalidElement(msg.message(next), source).asTemplateSpan)
      }
    }
    def withOptions (options: Options): DirectiveInstance = copy(options = options)
    def createInvalidElement (message: String): TemplateSpan = InvalidElement(message, "@" + source).asTemplateSpan
  }

  case class SeparatorInstance (parsedResult: ParsedDirective,
                                source: String,
                                options: Options = NoOpt) extends TemplateSpan with SeparatorInstanceBase with SpanResolver {
    type Self = SeparatorInstance
    def withOptions (options: Options): SeparatorInstance = copy(options = options)
    def resolve (cursor: DocumentCursor): TemplateSpan = 
      InvalidElement(s"Orphaned separator directive with name '${parsedResult.name}'", "@" + source).asTemplateSpan
  }

}
