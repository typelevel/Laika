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

package laika.rst.std

import laika.ast._
import laika.parse.implicits._
import laika.parse.markup.RecursiveParsers
import laika.parse.text.{CharGroup, TextParsers}
import laika.rst.ext.Directives.DirectivePartBuilder
import laika.rst.ext.Directives.Parts._

/**
  * @author Jens Halm
  */
object StandardDirectiveParts {


  /** The name option which is supported by almost all reStructuredText directives.
    */
  val nameOpt: DirectivePartBuilder[Option[String]] = optField("name")

  /** The class option which is supported by almost all reStructuredText directives.
    */
  val classOpt: DirectivePartBuilder[Option[String]] = optField("class")

  /** The standard class and name options supported by most directives,
    *  combined in the result into an Options instance.
    */
  val stdOpt: DirectivePartBuilder[Options] = (nameOpt ~ classOpt).map { case id ~ styles => toOptions(id, styles) }

  /** Converts an optional id and an optional style parameter containing
    *  a space-delimited list of styles to an `Options` instance.
    */
  def toOptions (id: Option[String], styles: Option[String]): Options =
    Options(id, styles.map(_.split(" ").toSet).getOrElse(Set()))


  /** The image directive for span elements,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#image]] for details.
    */
  def image (p: RecursiveParsers): DirectivePartBuilder[Span] = {
    import TextParsers._

    def multilineURI (text: String) = Right(text.split("\n").map(_.trim).mkString("\n").trim)

    val align = ("top" | "middle" | "bottom" | "left" | "center" | "right" |
      anyChars.flatMap(s => failure(s"illegal value for align: '$s'"))).map { a => Styles(s"align-$a") }

    val scale = sizeAndUnit | (anyOf(CharGroup.digit) ^^ { amt => Size(amt.toInt, "%") })

    (argument(multilineURI, withWS = true) ~
        optField("alt") ~
        optField("width", StandardDirectiveParsers.parseDirectivePart(sizeAndUnit, _)) ~
        optField("height", StandardDirectiveParsers.parseDirectivePart(sizeAndUnit, _)) ~
        optField("scale", StandardDirectiveParsers.parseDirectivePart(scale, _)) ~
        optField("align", StandardDirectiveParsers.parseDirectivePart(align, _)) ~
        optField("target", StandardDirectiveParsers.target(p)) ~
        stdOpt).map { case uri ~ alt ~ width ~ height ~ scale ~ align ~ target ~ opt =>

      val actualWidth  = scale.fold(width) (s =>  width.map(_.scale(s.amount)))
      val actualHeight = scale.fold(height)(s => height.map(_.scale(s.amount)))
      val alignOpt = align.getOrElse(NoOpt)

      val image = Image(alt.getOrElse(""), URI(uri), width = actualWidth, height = actualHeight)

      (target map {
        case ref: ExternalLink  => ref.copy(content = List(image.copy(options = opt)), options = alignOpt)
        case ref: LinkReference => ref.copy(content = List(image.copy(options = opt)), options = alignOpt)
      }).getOrElse(image.copy(options = alignOpt + opt))
    }
  }


}
