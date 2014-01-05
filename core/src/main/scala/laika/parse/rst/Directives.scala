/*
 * Copyright 2013 the original author or authors.
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

package laika.parse.rst

import laika.tree.Elements._
import laika.util.Builders._

/** API for creating directives, the extension mechanism of reStructuredText.
 *  The API did not aim to mimic the API of the original Python reference implementation.
 *  Instead the goal was to create an API that is idiomatic Scala, fully typesafe and as concise as possible.
 *  Yet it should be flexible enough to semantically support the options of the Python directives, so that
 *  ideally most existing Python directives could theoretically get ported to Laika.
 * 
 *  Entry points are the `BlockDirective` and `SpanDirective` objects. The Python reference parser does
 *  not make this distinction on the API level, but does this internally based on the context a 
 *  directive is parsed in. Since Laika APIs are typesafe, the distinction is necessary since
 *  block level and span level directives create different types of document tree nodes.
 *  A `SpanDirective` can only be used in a substitution definition which can then be used
 *  within flow elements. A `BlockDirective` can be used directly in any location other block
 *  level content like paragraphs or lists can be used.
 * 
 *  A directive may consist of any combination of arguments, fields and body elements:
 * 
 *  {{{
 *  .. myDirective:: arg1 arg2
 *   :field1: value1
 *   :field2: value2
 * 
 *   This is the body of the directive. It may consist of any standard or custom
 *   block-level and inline markup.
 *  }}}
 * 
 *  In the example above `arg1` and `arg2` are arguments, `field1` and `field2` are fields,
 *  and followed by body elements after a blank line. If there are no arguments or fields
 *  the blank line may be omitted. For the full specification, see 
 *  [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#directives]].
 * 
 *  For each of these directive elements, the API offers a method to specify whether the
 *  element is required or optional, and an optional function to convert or validate the
 *  parsed value.
 * 
 *  Consider the following simple example of a directive with just one argument and
 *  a body:
 * 
 *  {{{
 *  .. note:: This is the title
 *  
 *   This is the body of the note.
 *  }}}
 * 
 *  The implementation of this directive could look like this:
 * 
 *  {{{
 *  case class Note (title: String, 
 *                   content: Seq[Block], 
 *                   options: Options = NoOpt) extends Block 
 *                                             with BlockContainer[Note]
 *
 *  val rst = ReStructuredText withBlockDirectives
 *    BlockDirective("note") {
 *      (argument(withWS = true) ~ blockContent)(Note(_,_))
 *    }                                        
 *
 *  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"
 *  }}}
 * 
 *  The `argument()` method specifies a required argument of type `String` (since no conversion
 *  function was supplied). We need to set the `withWS` flag to true as an argument cannot have
 *  whitespace per default. The `blockContent` method specifies standard block content (any block-level
 *  elements that are supported in normal blocks, too) which results in a parsed value of type
 *  `Seq[Block]`. Finally you need to provide a function that accepts the results of the specified
 *  directive elements as parameters (of the corresponding type). Here we created a case class
 *  with a matching signature so can pass it directly as the target function. For a block directive
 *  the final result has to be of type `Block` which the `Note` class satisfies. Finally the directive 
 *  gets registered with the `ReStructuredText` parser.
 * 
 *  If any conversion or validation is required on the individual parts of the directive they can
 *  be passed to the corresponding function:
 * 
 *  {{{
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
 *                      options: Options = NoOpt) extends Block 
 *                                                with BlockContainer[Message]
 * 
 *  val rst = ReStructuredText withBlockDirectives (
 *    BlockDirective("message") {
 *      (argument(nonNegativeInt) ~ blockContent)(Message(_,_))
 *    }
 *  )    
 *  }}}
 * 
 *  The function has to provide an `Either[String, T]` as a result. A `Left` result will be interpreted
 *  as an error by the parser with the string being used as the message and an instance of `InvalidBlock`
 *  containing the validator message and the raw source of the directive will be inserted into the document
 *  tree. In this case the final function (`Message`) will never be invoked. A `Right` result will be
 *  used as an argument to the final function. Note how the case class now expects an `Int` as the first
 *  parameter.
 * 
 *  Finally arguments and fields can also be optional. In case they are missing, the directive is still
 *  considered valid and `None` will be passed to your function:
 * 
 *  {{{
 *  case class Message (severity: Option[Int], 
 *                      content: Seq[Block], 
 *                      options: Options = NoOpt) extends Block 
 *                                                with BlockContainer[Message]
 * 
 *  val rst = ReStructuredText withBlockDirectives (
 *    BlockDirective("message") {
 *      (optArgument(nonNegativeInt) ~ blockContent)(Message(_,_))
 *    }
 *  )    
 *  }}}
 * 
 *  The argument may be missing, but if it is present it has to pass the specified validator.
 * 
 *  In case of multiple arguments, the order you specify them is also the order in which they
 *  are parsed from the directive markup, with the only exception being that required arguments
 *  will always be parsed before optional ones, and arguments with whitespace need to come last.
 *   
 *  @author Jens Halm
 */
object Directives {

  
  /** API to implement by the actual directive parser.
   *  The methods of this trait correspond to the methods of the `Parts` object,
   *  only differing in return type. 
   */
  trait DirectiveParser {
      
    def argument [T](convert: String => Either[String,T] = {s:String => Right(s)}, 
                     withWS: Boolean = false): Result[T]
    
    def optArgument [T](convert: String => Either[String,T] = {s:String => Right(s)}, 
                     withWS: Boolean = false): Result[Option[T]]
    
    def field [T](name: String, convert: String => Either[String,T]): Result[T]
    
    def optField [T](name: String, convert: String => Either[String,T]): Result[Option[T]]
    
    def blockContent: Result[Seq[Block]]

    def spanContent: Result[Seq[Span]]
    
    def content [T](f: String => Either[String,T]): Result[T]
    
  }
  
  /** Represents a single part (argument, field or body) of a directive.
   */
  abstract class DirectivePart[+A] extends (DirectiveParser => Result[A]) { self =>
    
    def map [B](f: A => B) = new DirectivePart[B] { 
      def apply (p: DirectiveParser) = self(p) map f 
    }
    
  }

  /** Type class required for using the generic `Builders` API with directives.
   */
  implicit object CanBuildDirectivePart extends CanBuild[DirectivePart] {
    
    def apply [A,B](ma: DirectivePart[A], mb: DirectivePart[B]) = new DirectivePart[A~B] {
      def apply (p: DirectiveParser) = {
        val a = ma(p)
        val b = mb(p)
        new Result(new ~(a.get,b.get))
      }
    }
  
    def map [A,B](m: DirectivePart[A], f: A => B) = m map f
    
  }
 
  /** The public user API for specifying the required and optional parts of a directive
   *  (arguments, fields or body) together with optional converter/validator functions.
   */
  object Parts {
    
    private def part [T](f: DirectiveParser => Result[T]) = new DirectivePart[T] {
      def apply (p: DirectiveParser) = f(p)
    }
    
    /** Specifies a required argument. 
     * 
     *  @param convert the function to use for converting and validating the parsed value
     *  @param withWS whether the argument supports whitespace characters (only one of these 
     *  can exist in any single directive markup)
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def argument [T](convert: String => Either[String,T] = { s:String => Right(s) }, 
                     withWS: Boolean = false) = part(_.argument(convert, withWS)) 
      
    /** Specifies an optional argument. 
     * 
     *  @param convert the function to use for converting and validating the parsed value 
     *  if it is present
     *  @param withWS whether the argument supports whitespace characters (only one of these 
     *  can exist in any single directive markup)
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def optArgument [T](convert: String => Either[String,T] = { s:String => Right(s) }, 
                        withWS: Boolean = false) = part(_.optArgument(convert, withWS)) 

    /** Specifies a required named field. 
     * 
     *  @param name the name of the field as used in the directive markup (without the colons)
     *  @param convert the function to use for converting and validating the parsed value
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def field [T](name: String, 
                  convert: String => Either[String,T] = { s:String => Right(s) }): DirectivePart[T] = 
                    part(_.field(name, convert))
    
    /** Specifies an optional named field. 
     * 
     *  @param name the name of the field as used in the directive markup (without the colons)
     *  @param convert the function to use for converting and validating the parsed value
     *  if it is present
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def optField [T](name: String, 
                     convert: String => Either[String,T] = { s:String => Right(s) }): DirectivePart[Option[T]] = 
                     part(_.optField(name, convert))
    
    /** Specifies standard block-level content as the body of the directive.
     * 
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def blockContent: DirectivePart[Seq[Block]] = part(_.blockContent)

    def spanContent: DirectivePart[Seq[Span]] = part(_.spanContent)
    
    /** Specifies that the body of the directive markup should get passed to the conversion function as a raw string.
     * 
     *  @param convert the function to use for converting and validating the parsed value
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def content [T](f: String => Either[String,T]): DirectivePart[T] = part(_.content(f))
    
  }

  /** Represents a single directive implementation.
   */
  class Directive [E <: Element] private[Directives] (val name: String, val part: BlockParsers with InlineParsers => DirectivePart[E])

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
    def apply (name: String)(part: DirectivePart[Span]) = new Directive(name.toLowerCase, _ => part)
    
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
    def recursive (name: String)(part: BlockParsers with InlineParsers => DirectivePart[Span]) = new Directive(name.toLowerCase, part)
    
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
    def apply (name: String)(part: DirectivePart[Block]) = new Directive(name.toLowerCase, _ => part)
    
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
    def recursive (name: String)(part: BlockParsers with InlineParsers => DirectivePart[Block]) = new Directive(name.toLowerCase, part)
    
  }

  
}
