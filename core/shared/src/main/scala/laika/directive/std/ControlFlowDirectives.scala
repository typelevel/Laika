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

package laika.directive.std

import cats.syntax.all.*
import laika.api.bundle.TemplateDirectives
import laika.api.config.{ ConfigValue, Key }
import laika.ast.{ InvalidSpan, TemplateElement, TemplateScope, TemplateSpan, TemplateSpanSequence }
import laika.api.config.ConfigValue.*

import scala.annotation.tailrec

/** Provides the implementation for the standard control flow directives included in Laika.
  *
  * These include:
  *
  * - `for`: Accesses a value from the context and sets it as the reference context for its
  *   body elements, executing the body if the referenced value is non-empty and executing
  *   it multiple times when it is a collection.
  * - `if`: Accesses a value from the context and processes the body element only when
  *   it is a value recognized as true.
  *
  * For full documentation see the section about
  * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html#conditionals-and-loops Conditionals and Loops]]
  * in the manual.
  *
  * @author Jens Halm
  */
private[laika] object ControlFlowDirectives {

  /** Implementation of the `for` directive for templates.
    */
  lazy val templateFor: TemplateDirectives.Directive = TemplateDirectives.create("for") {

    import TemplateDirectives.dsl._

    val emptyValues    = Set[ConfigValue](StringValue(""), BooleanValue(false), NullValue)
    case class Empty(spans: Seq[TemplateSpan])
    val emptySeparator = TemplateDirectives.separator("empty", max = 1)(parsedBody.map(Empty.apply))

    (
      attribute(0).as[String],
      separatedBody(Seq(emptySeparator)),
      cursor,
      TemplateDirectives.dsl.source
    ).mapN { (ref, multipart, cursor, source) =>
      def contentScope(value: ConfigValue): TemplateSpan =
        TemplateScope(TemplateSpanSequence(multipart.mainBody), value, source)

      def fallback = multipart.children
        .headOption
        .map(e => TemplateSpanSequence(e.spans))
        .getOrElse(TemplateSpanSequence.empty)

      cursor.resolveReference(Key.parse(ref)) match {
        case Right(Some(o: ObjectValue))             => contentScope(o)
        case Right(Some(a: ArrayValue)) if a.isEmpty => fallback
        case Right(Some(a: ArrayValue)) => TemplateSpanSequence(a.values.map(contentScope))
        case Right(Some(simpleValue)) if emptyValues(simpleValue) => fallback
        case Right(Some(simpleValue))                             => contentScope(simpleValue)
        case Right(None)                                          => fallback
        case Left(error)                                          =>
          TemplateElement(
            InvalidSpan(s"Error retrieving reference '$ref': ${error.message}", source)
          )
      }
    }
  }

  /** Implementation of the `if` directive for templates.
    */
  lazy val templateIf: TemplateDirectives.Directive = TemplateDirectives.create("if") {

    import TemplateDirectives.dsl._

    val trueStrings = Set("true", "yes", "on", "enabled")

    sealed trait IfSeparator                                extends Product with Serializable
    case class ElseIf(ref: String, body: Seq[TemplateSpan]) extends IfSeparator
    case class Else(body: Seq[TemplateSpan])                extends IfSeparator

    val elseIfSep     = TemplateDirectives.separator("elseIf") {
      (attribute(0).as[String], parsedBody).mapN(ElseIf.apply)
    }
    val elseSep       = TemplateDirectives.separator("else", max = 1) {
      parsedBody.map(Else.apply)
    }
    val multipartBody = separatedBody(Seq(elseIfSep, elseSep))

    (attribute(0).as[String], multipartBody, cursor).mapN { (path, multipart, cursor) =>
      def fallback = multipart.children
        .collectFirst { case e: Else => e }
        .map(e => TemplateSpanSequence(e.body))
        .getOrElse(TemplateSpanSequence.empty)

      @tailrec
      def process(parts: Seq[ElseIf]): TemplateSpan =
        if (parts.isEmpty) fallback
        else
          cursor.resolveReference(Key.parse(parts.head.ref)) match {
            case Right(Some(BooleanValue(true))) => TemplateSpanSequence(parts.head.body)
            case Right(Some(StringValue(s))) if trueStrings(s) =>
              TemplateSpanSequence(parts.head.body)
            case Right(Some(a: ArrayValue)) if !a.isEmpty => TemplateSpanSequence(parts.head.body)
            case _                                        => process(parts.tail)
          }

      val alternatives = ElseIf(path, multipart.mainBody) +: multipart.collect[ElseIf]
      process(alternatives)
    }
  }

}
