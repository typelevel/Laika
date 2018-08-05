/*
 * Copyright 2013-2016 the original author or authors.
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

import laika.util.Builders._
import laika.ast._
import laika.rewrite.DocumentCursor
import laika.util.~

/** API for creating directives, the Laika's extension mechanism for creating
 *  custom tags for both, templates or text markup.
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
 *        (attribute(Default) ~ body(Default)) (Note(_,_))
 *      }
 *    )
 *    val blockDirectives = Seq()
 *  }
 *
 *  Transform from Markdown to HTML using MyDirectives fromFile "hello.md" toFile "hello.html"
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
 *      (attribute(Default, positiveInt) ~ blockContent) (Message(_,_))
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
 *      (attribute(Default, positiveInt).optional ~ blockContent) (Message(_,_))
 *    }
 *  )    
 *  }}}
 * 
 *  The attribute may be missing, but if it is present it has to pass the specified validator.
 * 
 *  @author Jens Halm
 */
object Directives {

  
  /** Represents the result (or combined results) 
   *  of processing one or more parts of a directive.
   */
  sealed abstract class Result[+A] {
    
    def get: A
    
    def map [B](f: A => B): Result[B]
    
    def flatMap [B](f: A => Result[B]): Result[B]
    
    def ~ [B](result: Result[B]): Result[A ~ B]
    
  }
  
  /** Represents the successful computation
   *  of a directive part.
   */
  case class Success[+A] (a: A) extends Result[A] {
    
    def get: A = a
    
    def map [B](f: A => B): Result[B] = Success(f(get))
    
    def flatMap [B](f: A => Result[B]): Result[B] = f(a)
    
    def ~ [B](result: Result[B]): Result[A ~ B] = result match {
      case Success(value) => Success(new ~(a, value))
      case Failure(msg)   => Failure(msg)
    }
    
  }
  
  /** Represents the failed computation
   *  of a directive part. This failure type
   *  can be combined with other failures
   *  to "collect" all error messages, in contrast
   *  to the `Either` type for example, which always
   *  only carries one `Left` value through a chain
   *  of computations.
   */
  case class Failure (messages: Seq[String]) extends Result[Nothing] {
    
    def get: Nothing = throw new RuntimeException("no result as processing failed")
    
    def map [B](f: Nothing => B): Result[B] = this
    
    def flatMap [B](f: Nothing => Result[B]): Result[B] = this
    
    def ~ [B](result: Result[B]): Result[Nothing ~ B] = result match {
      case Success(value)     => this
      case Failure(otherMsg)  => Failure(messages ++ otherMsg)
    }
    
  }
  
  object Failure {
    def apply (msg: String) = new Failure(Seq(msg))
  }
  
  sealed abstract class Id {
    def desc (keyType: String): String
  }
  
  /** Represents the string identifier of an attribute or body part
   *  of a directive.
   */
  case class Named (name: String) extends Id {
    def desc (keyType: String): String = s"$keyType with name '$name'"
  }
  
  /** Represents an unnamed attribute or body part
   *  of a directive.
   */
  case object Default extends Id {
    def desc (keyType: String): String = s"default $keyType"
  }
  
  implicit def stringToId (str: String): Id = Named(str)
  

  sealed abstract class Key (keyType: String) {
    def id: Id
    def desc: String = id.desc(keyType)
  }
  
  case class Attribute (id: Id) extends Key("attribute")

  case class Body (id: Id) extends Key("body")
  
  
  
  /** Provides the basic building blocks for 
   *  Laika's Directive API. This trait
   *  is not used directly, but instead its
   *  three subtraits `Blocks`, `Spans` and `Templates`,
   *  which represent the concrete implementations
   *  for the three directive types.
   */
  trait BuilderContext[E <: Element] {

    /** The parser API in case a directive function
     *  needs to manually parse one of the directive
     *  parts.
     */
    type Parser <: (String => Seq[E])
    
    /** The context of a directive during
     *  execution.
     */
    trait DirectiveContext {
        
      def part (key: Key): Option[String]
      
      def cursor: Option[DocumentCursor]
      
      def parser: Parser
  
    }
    
    /** Represents a single part (attribute or body) of a directive
     *  or a combination of multiple parts.
     */
    abstract class DirectivePart[+A] extends (DirectiveContext => Result[A]) { self =>
      
      def map [B](f: A => B): DirectivePart[B] = new DirectivePart[B] { 
        def apply (p: DirectiveContext) = self(p) map f 
        def requiresContext = self.requiresContext
      }
      
      def requiresContext: Boolean
      
      /** Indicates that this directive part is optional,
       *  turning the result into an Option value.
       *  If the part is present it still has to validate
       *  successfully.
       */
      def optional: DirectivePart[Option[A]] = map (Some(_))
      
    }
    
    /** Type class required for using the generic `Builders` API with directives.
     */
    implicit object CanBuildDirectivePart extends CanBuild[DirectivePart] {
      
      def apply [A,B](ma: DirectivePart[A], mb: DirectivePart[B]): DirectivePart[A~B] = new DirectivePart[A~B] {
        def apply (p: DirectiveContext) = ma(p) ~ mb(p)
        def requiresContext = ma.requiresContext || mb.requiresContext
      }
      
      def map [A,B](m: DirectivePart[A], f: A => B) = m map f
      
    }
    
    type Converter[T] = (Parser, String) => Result[T]
    
    /** Provides various converter functions
     *  that can be used with the directive
     *  combinators to convert the string value
     *  obtained from a directive attribute or
     *  body. 
     */
    object Converters {
      
      val string: Converter[String] = (_, input) => Success(input)
      
      val parsed: Converter[Seq[E]] = (parser, input) => Success(parser(input))
      
      val int: Converter[Int] = (_, input) => toInt(input, _ => true)

      val positiveInt: Converter[Int] = (_, input) => toInt(input, _ > 0, "not a positive integer")

      val nonNegativeInt: Converter[Int] = (_, input) => toInt(input, _ >= 0, "not a non-negative integer")
      
      private def toInt (input: String, predicate: Int => Boolean, msg: String = ""): Result[Int] = { 
        try { 
          val i = input.trim.toInt
          if (predicate(i)) Success(i) else Failure(s"$msg: $i")
        } catch { 
          case e: NumberFormatException => Failure(s"not an integer: $input")
        }
    }
      
    }
   
    /** Provides various combinators to describe the expected
     *  format of a specific directive.
     * 
     */
    object Combinators {
      
      private def requiredPart [T] (key: Key, converter: Converter[T], msg: => String) = new DirectivePart[T] {
      
        val requiresContext = false
        
        def convert (context: DirectiveContext) = context.part(key).map(s => converter(context.parser, s))
        
        def apply (context: DirectiveContext) = convert(context).getOrElse(Failure(Seq(msg)))
        
        override def optional = new DirectivePart[Option[T]] {
          val requiresContext = false
          def apply (context: DirectiveContext) = convert(context) match {
            case Some(Success(value)) => Success(Some(value))
            case Some(Failure(msg))   => Failure(s"error converting ${key.desc}: " + msg.mkString(", "))
            case None                 => Success(None)
          }
        }
        
      }
      
      private def part [T](f: DirectiveContext => Result[T], reqContext: Boolean = false) = new DirectivePart[T] {
        def apply (p: DirectiveContext) = f(p)
        val requiresContext = reqContext
      }
      
      /** Specifies a required attribute. 
       * 
       *  @param id the identifier that must be used in markup or templates
       *  @param converter the function to use for converting and validating the parsed value
       *  @return a directive part that can be combined with further parts with the `~` operator
       */
      def attribute [T](id: Id, converter: Converter[T] = Converters.string): DirectivePart[T] 
          = requiredPart(Attribute(id), converter, s"required ${Attribute(id).desc} is missing") 
      
      /** Specifies a required body part. 
       * 
       *  @param id the identifier that must be used in markup or templates
       *  @param converter the function to use for converting and validating the parsed value
       *  @return a directive part that can be combined with further parts with the `~` operator
       */
      def body [T](id: Id, converter: Converter[T] = Converters.parsed): DirectivePart[T] 
          = requiredPart(Body(id), converter, s"required ${Body(id).desc} is missing")
          
      /** Specifies an empty directive that does not accept any attributes or 
       *  body elements.
       *     
       *  @param result the fixed result each empty directive will produce   
       *  @return a directive part that usually won't be combined with other parts
       */    
      def empty [T] (result: T): DirectivePart[T] = part(_ => Success(result))
      
      /** Indicates that access to the parser responsible for this directive
       *  is needed, in case the directive implementation has to manually
       *  parse parts or all of its result.
       * 
       *  The advantage of using the parser provided by the runtime versus
       *  creating your own is only this provided parser can now all other
       *  registered extensions in case your directive content may contain
       *  other directives.
       */
      def parser: DirectivePart[Parser] = part(c => Success(c.parser))

      /** Indicates that access to the document cursor is required.
       *  This may be required if the directive relies on information
       *  from the document structure, its title or the parent tree
       *  it is contained in.
       * 
       *  Use of this function causes the directive to be processed in a later
       *  rewrite step as the document cursor is not yet fully populated in
       *  the initial rewrite step. But this is an implementation detail
       *  you normally do not need to deal with.
       */
      def cursor: DirectivePart[DocumentCursor] 
          = part(_.cursor map (Success(_)) getOrElse Failure("DocumentCursor not available yet"), reqContext = true)
      
    }
  
    /** Represents a directive, its name and its (combined) parts.
     */
    class Directive private[Directives] (val name: String, part: DirectivePart[E]) {
      def apply (context: DirectiveContext): Result[E] = part(context)
      def requiresContext: Boolean = part.requiresContext
    }

    /** Creates a new directive with the specified name
     *  and part specification.
     */
    def create (name: String)(part: DirectivePart[E]): Directive = new Directive(name, part)

    /** Turns a collection of directives into a map,
     *  using the name of the directive as the key.
     */
    def toMap (directives: Traversable[Directive]): Map[String, Directive] = directives map (dir => (dir.name, dir)) toMap
    
  }
  
  /** The API for declaring directives that can be used
   *  as inline elements in markup documents.
   */
  object Spans extends BuilderContext[Span] {
    
    trait Parser extends (String => Seq[Span]) {
      def apply (source: String): Seq[Span]
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
    
  }
  
  /** The API for declaring directives that can be used
   *  in templates.
   */
  object Templates extends BuilderContext[TemplateSpan] {

    trait Parser extends (String => Seq[TemplateSpan]) {
      def apply (source: String): Seq[TemplateSpan]
    }
    
  }
  
  
}
