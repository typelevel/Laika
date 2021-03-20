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

import cats.syntax.all._
import laika.ast.{DocumentCursor, InvalidSpan, TemplateElement, TemplateSpan, TemplateSpanSequence}
import laika.config.{ArrayValue, BooleanValue, ConfigValue, Key, NullValue, ObjectValue, StringValue}
import laika.directive.Templates
import laika.rewrite.TemplateRewriter

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
  * [[https://planet42.github.io/Laika/07-reference/01-standard-directives.html#conditionals-and-loops Conditionals and Loops]] 
  * in the manual.
  * 
  * @author Jens Halm
  */
object ControlFlowDirectives {

  /** Implementation of the `for` directive for templates.
    */
  lazy val templateFor: Templates.Directive = Templates.create("for") {

    import Templates.dsl._

    val emptyValues = Set[ConfigValue](StringValue(""), BooleanValue(false), NullValue)
    case class Empty (spans: Seq[TemplateSpan])
    val emptySeparator = Templates.separator("empty", max = 1)(parsedBody.map(Empty.apply))

    (attribute(0).as[String], separatedBody(Seq(emptySeparator)), cursor, Templates.dsl.source).mapN { (ref, multipart, cursor, source) =>

      def rewrite (spans: Seq[TemplateSpan], childCursor: DocumentCursor): TemplateSpanSequence =
        TemplateSpanSequence(spans).rewriteChildren(TemplateRewriter.rewriteRules(childCursor))

      def rewriteContent (value: ConfigValue): TemplateSpanSequence = rewrite(multipart.mainBody, cursor.withReferenceContext(value))

      def rewriteFallback = multipart.children.headOption.map(_.spans).map(rewrite(_, cursor)).getOrElse(TemplateSpanSequence.empty)

      cursor.resolveReference(Key.parse(ref)) match {
        case Right(Some(o: ObjectValue))             => rewriteContent(o)
        case Right(Some(a: ArrayValue)) if a.isEmpty => rewriteFallback
        case Right(Some(a: ArrayValue))              => TemplateSpanSequence(a.values.map(rewriteContent))
        case Right(Some(simpleValue)) if emptyValues(simpleValue) => rewriteFallback
        case Right(Some(simpleValue))                => rewriteContent(simpleValue)
        case Right(None)                             => rewriteFallback
        case Left(error)                             => TemplateElement(InvalidSpan(s"Error retrieving reference '$ref': ${error.message}", source))
      }
    }
  }

  /** Implementation of the `if` directive for templates.
    */
  lazy val templateIf: Templates.Directive = Templates.create("if") {

    import Templates.dsl._

    val trueStrings = Set("true","yes","on","enabled")

    sealed trait IfSeparator extends Product with Serializable
    case class ElseIf (ref: String, body: Seq[TemplateSpan]) extends IfSeparator
    case class Else (body: Seq[TemplateSpan]) extends IfSeparator

    val elseIfSep = Templates.separator("elseIf") {
      (attribute(0).as[String], parsedBody).mapN(ElseIf.apply)
    }
    val elseSep = Templates.separator("else", max = 1) {
      parsedBody.map(Else.apply)
    }
    val multipartBody = separatedBody(Seq(elseIfSep, elseSep))

    (attribute(0).as[String], multipartBody, cursor).mapN { (path, multipart, cursor) =>

      def rewrite (spans: Seq[TemplateSpan]): TemplateSpanSequence =
        TemplateSpanSequence(spans).rewriteChildren(TemplateRewriter.rewriteRules(cursor))

      def rewriteFallback = multipart.children
        .collectFirst { case e: Else => e }
        .map(_.body).map(rewrite)
        .getOrElse(TemplateSpanSequence.empty)

      @tailrec
      def process (parts: Seq[ElseIf]): TemplateSpanSequence =
        if (parts.isEmpty) rewriteFallback
        else cursor.resolveReference(Key.parse(parts.head.ref)) match {
          case Right(Some(BooleanValue(true)))               => rewrite(parts.head.body)
          case Right(Some(StringValue(s))) if trueStrings(s) => rewrite(parts.head.body)
          case Right(Some(a: ArrayValue)) if !a.isEmpty      => rewrite(parts.head.body)
          case _ => process(parts.tail)
        }

      val alternatives = ElseIf(path, multipart.mainBody) +: multipart.collect[ElseIf]
      process(alternatives)
    }
  }
  
}
