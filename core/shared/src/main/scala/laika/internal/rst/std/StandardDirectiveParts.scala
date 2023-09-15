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

package laika.internal.rst.std

import laika.ast.*
import laika.internal.rst.ext.Directives.DirectivePartBuilder
import laika.parse.builders.~
import laika.parse.implicits.*
import laika.parse.markup.RecursiveParsers
import laika.parse.text.{ CharGroup, TextParsers }
import laika.parse.{ GeneratedSource, SourceFragment }
import laika.internal.rst.BaseParsers.sizeAndUnit
import laika.internal.rst.ext.Directives.DirectivePartBuilder
import laika.internal.rst.ext.Directives.Parts.*

/** @author Jens Halm
  */
private[std] object StandardDirectiveParts {

  /** The name option which is supported by almost all reStructuredText directives.
    */
  val nameOpt: DirectivePartBuilder[Option[String]] = optField("name")

  /** The class option which is supported by almost all reStructuredText directives.
    */
  private val classOpt: DirectivePartBuilder[Option[String]] = optField("class")

  /** The standard class and name options supported by most directives,
    *  combined in the result into an Options instance.
    */
  val stdOpt: DirectivePartBuilder[Options] = (nameOpt ~ classOpt).map { case id ~ styles =>
    toOptions(id, styles)
  }

  /** Converts an optional id and an optional style parameter containing
    *  a space-delimited list of styles to an `Options` instance.
    */
  def toOptions(id: Option[String], styles: Option[String]): Options =
    Options(id, styles.map(_.split(" ").toSet).getOrElse(Set()))

  /** The image directive for span elements,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#image]] for details.
    */
  def image(p: RecursiveParsers): DirectivePartBuilder[Span] = {
    import TextParsers._

    def multilineURI(text: SourceFragment) = Right(
      text.input.split("\n").map(_.trim).mkString("\n").trim
    )

    val align = ("top" | "middle" | "bottom" | "left" | "center" | "right" |
      anyChars.flatMap(s => failure(s"illegal value for align: '$s'"))).map { a =>
      Styles(s"align-$a")
    }

    val scale =
      sizeAndUnit | (anyOf(CharGroup.digit) ^^ { amt => LengthUnit.percent(amt.toDouble) })

    (argument(multilineURI, withWS = true) ~
      optField("alt") ~
      optField("width", StandardDirectiveParsers.parseDirectivePart(sizeAndUnit, _)) ~
      optField("height", StandardDirectiveParsers.parseDirectivePart(sizeAndUnit, _)) ~
      optField("scale", StandardDirectiveParsers.parseDirectivePart(scale, _)) ~
      optField("align", StandardDirectiveParsers.parseDirectivePart(align, _)) ~
      optField("target", StandardDirectiveParsers.target(p)) ~
      stdOpt).map { case uri ~ alt ~ width ~ height ~ scale ~ align ~ target ~ opt =>
      val actualWidth  = scale.fold(width)(s => width.map(_.scale(s.amount)))
      val actualHeight = scale.fold(height)(s => height.map(_.scale(s.amount)))
      val alignOpt     = align.getOrElse(NoOpt)

      val img      = Image(Target.parse(uri), width = actualWidth, height = actualHeight, alt = alt)
      val resolver = ImageResolver(img, GeneratedSource)

      target
        .flatMap {
          case sc: SpanContainer =>
            // type inference not working properly here, but the cast is safe
            Some(
              sc.withContent(List(resolver.withOptions(opt))).withOptions(opt).asInstanceOf[Span]
            )
          case _                 => None
        }
        .getOrElse(resolver.withOptions(alignOpt + opt))
    }
  }

  case class ImageResolver(image: Image, source: SourceFragment) extends SpanResolver {
    type Self = ImageResolver
    val options: Options                               = image.options
    override def resolve(cursor: DocumentCursor): Span = cursor.validateAndRecover(image, source)

    override def withOptions(options: Options): ImageResolver =
      copy(image = image.withOptions(options))

    def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]
    lazy val unresolvedMessage: String = s"Unresolved image resolver for target '${image.target}'"
  }

}
