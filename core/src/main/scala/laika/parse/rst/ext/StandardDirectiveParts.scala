/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.rst.ext

import laika.parse.core.markup.{EscapedTextParsers, RecursiveParsers}
import laika.parse.rst.Directives.DirectivePart
import laika.parse.rst.Directives.Parts._
import laika.tree.Elements._

/**
  * @author Jens Halm
  */
object StandardDirectiveParts {


  /** The name option which is supported by almost all reStructuredText directives.
    */
  val nameOpt: DirectivePart[Option[String]] = optField("name")

  /** The class option which is supported by almost all reStructuredText directives.
    */
  val classOpt: DirectivePart[Option[String]] = optField("class")

  /** The standard class and name options supported by most directives,
    *  combined in the result into an Options instance.
    */
  val stdOpt: DirectivePart[Options] = (nameOpt ~ classOpt) { (id, styles) => toOptions(id, styles) }

  /** Converts an optional id and an optional style parameter containing
    *  a space-delimited list of styles to an `Options` instance.
    */
  def toOptions (id: Option[String], styles: Option[String]): Options =
    Options(id, styles.map(_.split(" ").toSet).getOrElse(Set()))


  /** The image directive for span elements,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#image]] for details.
    */
  def image (p: RecursiveParsers): DirectivePart[Span] = {
    def multilineURI (text: String) = Right(text.split("\n").map(_.trim).mkString("\n").trim)

    (argument(multilineURI, withWS = true) ~ optField("alt") ~ optField("target", StandardDirectiveParsers.target(p)) ~ stdOpt) { (uri, alt, target, opt) =>
      val image = Image(alt.getOrElse(""), URI(uri), None, opt)
      (target map {
        case ref: ExternalLink  => ref.copy(content = List(image))
        case ref: LinkReference => ref.copy(content = List(image))
      }).getOrElse(image)
    }
  }


}
