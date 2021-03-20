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

package laika.rst.ext

import laika.ast._
import laika.parse.{Parser, SourceFragment}
import laika.parse.markup.RecursiveParsers
import laika.rst.bundle.RstExtension
import laika.rst.ext.Directives._
import laika.rst.ext.ExtensionParsers.Result

/** API for creating interpreted text roles, the extension mechanism for inline elements of reStructuredText.
  *
  * The API did not aim to mimic the API of the original Python reference implementation.
  * Instead the goal was to create an API that is idiomatic Scala, fully typesafe and as concise as possible.
  * Yet it should be flexible enough to semantically support the options of the Python text roles, so that
  * ideally most existing Python text roles could theoretically get ported to Laika.
  *
  * =Implementing a Directive=
  *
  * Entry point for creating a new role is the `TextRole` object. It allows to specify the following
  * aspects that define a text role:
  *
  *  - The name with which it can be referred to by both, a span of interpreted text and a role
  * directive to further customize it.
  *
  *  - The default value, that should get passed to the role function in case it is used
  * directly in interpreted text without customization through a role directive.
  *
  *  - The role directive that specifies how the role can be customized. The options
  * for role directives are almost identical to regular directives, the only difference
  * being that role directives do not support arguments, only fields and body elements.
  *
  *  - The actual role function. It gets invoked for each occurrence of interpreted text
  * that refers to this role, either directly by name or to the name of a role directive
  * that customized this role. The first argument is either the default value
  * or the result of the role directive, the second is the actual text of the interpreted 
  * text span. The return value of the role function is the actual `Span` instance
  * that the original interpreted text should be replaced with.
  *
  * =Basic Example=
  * 
  * A role directive may consist of any combination of fields and body elements:
  *
  * {{{
  *  .. role:: ticket(link)
  *   :base-url: http://www.company.com/tickets/
  * }}}
  *
  * In the example above `ticket` is the name of the customized role, `link` the name
  * of the base role and `base-url` the value that overrides the default defined in the
  * base role. For the specification details on role directives see 
  * [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#custom-interpreted-text-roles]].
  *
  * Before such a role directive can be used, an implementation has to be provided
  * for the base role with the name `link`. For more details on implementing directives
  * see [[laika.rst.ext.Directives]].
  *
  * The implementation of the `link` text role could look like this:
  *
  * {{{
  *  val textRole = TextRole("link", "http://www.company.com/main/")(field("base-url")) {
  *    (base, text) => Link(List(Text(text)), base + text)
  *  }
  *
  *  object MyDirectives extends RstExtensionRegistry {
  *    val textRoles = Seq(textRole)
  *    val spanDirectives = Seq()
  *    val blockDirectives = Seq()
  *  }
  *
  *  val transformer = Transformer
  *    .from(ReStructuredText)
  *    .to(HTML)
  *    .using(MyDirectives)
  *    .build
  * }}}
  *
  * We specify the name of the role to be `link`, and the default value the URL provided as the
  * second argument. The second parameter list specifies the role directive implementation,
  * in this case only consisting of a call to `field("base-url")` which specifies a required 
  * field of type `String` (since no conversion function was supplied). The type of the result
  * of the directive has to match the type of the default value.
  * Finally the role function is defined that accepts two arguments. The first is the base
  * url, either the default in case the base role is used directly, or the value specified
  * with the `base-url` field in a customized role. The second is the actual text from the
  * interpreted text span. Finally the directive gets registered with the `ReStructuredText`
  * parser.
  *
  * If you need to define more fields or body content they can be added with the `~` combinator
  * just like with normal directives. Likewise you can specify validators and converters for 
  * fields and body values like documented in [[laika.rst.ext.Directives]].
  *
  * =Using the Text Role in Markup=
  * 
  * Our example role can then be used in the following ways:
  *
  * Using the base role directly:
  *
  * {{{
  *  For details read our :link:`documentation`.
  * }}}
  *
  * This would result in the following HTML:
  *
  * {{{
  *  For details read our <a href="http://www.company.com/main/documentation">documentation</a>.
  * }}}
  *
  * Using the customized role called `ticket`: 
  *
  * {{{
  *  For details see ticket :ticket:`344`.
  * }}}
  *
  * This would result in the following HTML:
  *
  * {{{
  *  For details see ticket <a href="http://www.company.com/ticket/344">344</a>.
  * }}}
  *
  * @author Jens Halm
  */
object TextRoles {

  /** API to implement by the actual directive parser.
    *
    * This allows directive parts to specify the expected elements within
    * the parsed directive. In contrast to Laika's directive syntax which 
    * allows to have a single directive parser for any kind of directive implementation,
    * the one for ReStructuredText has a separate parser for each directive depending
    * on its configuration.
    * 
    * The API for text roles is a subset of the API for block and span directives.
    */
  trait RoleDirectiveParserBuilder {

    def parser: Parser[Vector[Part]]

    def field (name: String): (Key, RoleDirectiveParserBuilder)

    def optField (name: String): (Key, RoleDirectiveParserBuilder)

    def body: (Key, RoleDirectiveParserBuilder)

  }

  /** Represents a single part (argument, field or body) of a text role.
    */
  abstract class RoleDirectivePartBuilder[+A] extends (RoleDirectiveParserBuilder => (RoleDirectiveParserBuilder, RoleDirectivePart[A])) { self =>

    def map [B](f: A => B): RoleDirectivePartBuilder[B] = new RoleDirectivePartBuilder[B] {
      def apply (parser: RoleDirectiveParserBuilder): (RoleDirectiveParserBuilder, RoleDirectivePart[B]) = {
        val (newParser, part) = self.apply(parser)
        (newParser, part.map(f))
      }
    }

    def ~ [B] (other: RoleDirectivePartBuilder[B]): RoleDirectivePartBuilder[A ~ B] = new RoleDirectivePartBuilder[A ~ B] {
      def apply (parser: RoleDirectiveParserBuilder): (RoleDirectiveParserBuilder, RoleDirectivePart[A ~ B]) = {
        val (parserA, partA) = self.apply(parser)
        val (parserB, partB) = other.apply(parserA)
        (parserB, partA ~ partB)
      }
    }

  }
  
  /** Represents a single part (field or body) of a directive.
   */
  abstract class RoleDirectivePart[+A] extends (ParsedDirective => Result[A]) { self =>
    
    def map [B](f: A => B): RoleDirectivePart[B] = new RoleDirectivePart[B] { 
      def apply (p: ParsedDirective) = self(p) map f 
    }

    def flatMap [B](f: A => Result[B]): RoleDirectivePart[B] = new RoleDirectivePart[B] {
      def apply (p: ParsedDirective) = self(p) flatMap f
    }
    
    def ~ [B] (other: RoleDirectivePart[B]): RoleDirectivePart[A~B] = new RoleDirectivePart[A~B] {
      def apply (p: ParsedDirective) = for {
        a <- self.apply(p)
        b <- other.apply(p)
      } yield new ~(a, b)
    }
    
  }
 
  /** The public user API for specifying the required and optional parts of a directive
   *  (fields or body) together with optional converter/validator functions.
   */
  object Parts {
    
    import Directives.Converters._
    
    private def requiredPart [T] (build: RoleDirectiveParserBuilder => (Key, RoleDirectiveParserBuilder),
                                  converter: (ParsedDirective, SourceFragment) => Either[String, T]) = new RoleDirectivePartBuilder[T] {
      val base = part(build, converter)
      def apply (builder: RoleDirectiveParserBuilder): (RoleDirectiveParserBuilder, RoleDirectivePart[T]) = {
        val (newBuilder, basePart) = base(builder)
        val reqPart = basePart.flatMap(_.toRight("Missing directive part"))
        (newBuilder, reqPart)
      }
    }

    private def part [T] (build: RoleDirectiveParserBuilder => (Key, RoleDirectiveParserBuilder),
                          converter: (ParsedDirective, SourceFragment) => Either[String, T]) = new RoleDirectivePartBuilder[Option[T]] {

      def apply (builder: RoleDirectiveParserBuilder): (RoleDirectiveParserBuilder, RoleDirectivePart[Option[T]]) = {
        val (key, newBuilder) = build(builder)
        val part = new RoleDirectivePart[Option[T]] {
          def apply (parsed: ParsedDirective): Result[Option[T]] = parsed.part(key).map(converter(parsed, _)) match {
            case None => Right(None)
            case Some(Left(error)) => Left(error)
            case Some(Right(result)) => Right(Some(result))
          }
        }
        (newBuilder, part)
      }

    }
    
    /** Specifies a required named field. 
     * 
     *  @param name the name of the field as used in the directive markup (without the colons)
     *  @param convert the function to use for converting and validating the parsed value
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def field [T](name: String, 
                  convert: SourceFragment => Either[String,T] = { (s:SourceFragment) => Right(s.input) }): RoleDirectivePartBuilder[T] = 
                    requiredPart(_.field(name), simple(convert))
    
    /** Specifies an optional named field. 
     * 
     *  @param name the name of the field as used in the directive markup (without the colons)
     *  @param convert the function to use for converting and validating the parsed value
     *  if it is present
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def optField [T](name: String, 
                     convert: SourceFragment => Either[String,T] = { (s:SourceFragment) => Right(s.input) }): RoleDirectivePartBuilder[Option[T]] = 
                     part(_.optField(name), simple(convert))
    
    /** Specifies standard block-level content as the body of the directive.
     * 
     *  @return a directive part that can be combined with further parts with the `~` operator
     */                 
    def blockContent: RoleDirectivePartBuilder[Seq[Block]] = requiredPart(_.body, blocks)
    
    def spanContent: RoleDirectivePartBuilder[Seq[Span]] = requiredPart(_.body, spans)
    
    /** Specifies that the body of the directive markup should get passed to the conversion function as a raw string.
     * 
     *  @param f the function to use for converting and validating the parsed value
     *  @return a directive part that can be combined with further parts with the `~` operator
     */
    def content [T](f: SourceFragment => Either[String,T]): RoleDirectivePartBuilder[T] = requiredPart(_.body, simple(f))
    
  }

  //type RoleDirectivePartBuilder[E] = RecursiveParsers => RoleDirectivePart[E]

  /** Represents a single text role implementation.
   */
  class TextRole private (val name: String,
                          val default: String => Span,
                          val part: RecursiveParsers => RoleDirectivePartBuilder[String => Span]) extends RstExtension[RoleDirectivePartBuilder[String => Span]]

  /** API entry point for setting up a text role that.
   */
  object TextRole {
    
    /** Creates a new text role that can be referred to by interpreted text with the specified name.
     *  The `DirectivePart` can be created by using the methods of the `Parts`
     *  object and specifies the functionality for users who customize a text role with a role directive.
     *  The `roleF` function is the final function that will be invoked with either the default value
     *  or the result of the role directive as the first argument (depending on whether the user used
     *  the default role or a customized one). The actual text of the interpreted text will be passed
     *  as the second argument. The return value of the role function is the actual `Span` instance
     *  that the original interpreted text should be replaced with.
     * 
     *  @param name the name the text role can be used with in interpreted text
     *  @param default the default value to pass to the role function in case the interpreted text
     *  is not referring to a role directive
     *  @param part the implementation of the role directive for customizing the text role 
     *  that can be created by using the combinators of the `Parts` object
     *  @param roleF the final role function that gets passed the result of the directive (or default
     *  value) and the actual text of the interpreted text span
     *  @return a new text role that can be registered with the reStructuredText parser
     */
    def apply [T] (name: String, default: T)(part: RoleDirectivePartBuilder[T])(roleF: (T, String) => Span): TextRole = 
      new TextRole(name.toLowerCase, str => roleF(default, str), _ => part map (res => (str: String) => roleF(res, str)))
    
    /** Creates a new text role that can be referred to by interpreted text with the specified name.
     *  The `DirectivePart` can be created by using the methods of the `Parts`
     *  object and specifies the functionality for users who customize a text role with a role directive.
     *  The `roleF` function is the final function that will be invoked with either the default value
     *  or the result of the role directive as the first argument (depending on whether the user used
     *  the default role or a customized one). The actual text of the interpreted text will be passed
     *  as the second argument. The return value of the role function is the actual `Span` instance
     *  that the original interpreted text should be replaced with.
     * 
     *  In contrast to the `apply` function, this function allows to 
     *  depend on the standard block and span parsers. This is necessary if
     *  the directive does both, require a custom parser for arguments or body 
     *  and allow for nested directives in those parsers.
     * 
     *  @param name the name the text role can be used with in interpreted text
     *  @param default the default value to pass to the role function in case the interpreted text
     *  is not referring to a role directive
     *  @param part a function returning the implementation of the role directive for customizing the text role 
     *  that can be created by using the combinators of the `Parts` object
     *  @param roleF the final role function that gets passed the result of the directive (or default
     *  value) and the actual text of the interpreted text span
     *  @return a new text role that can be registered with the reStructuredText parser
     */
    def recursive [T] (name: String, default: T)(part: RecursiveParsers => RoleDirectivePartBuilder[T])(roleF: (T, String) => Span): TextRole =
      new TextRole(name.toLowerCase, str => roleF(default, str), parsers => part(parsers) map (res => (str: String) => roleF(res, str)))
    
  }

  
}
