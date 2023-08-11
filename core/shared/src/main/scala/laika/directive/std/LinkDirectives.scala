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
import laika.ast.{ DocumentCursor, SpanLink, Target }
import laika.directive.Links
import laika.rewrite.link.LinkConfig

/** Provides the implementation for the link directives included in Laika.
  *
  * These include:
  *
  * - `api`: Convenience directive that allows to reference an api documentation entry (e.g. scaladoc, javadoc)
  * - `source`: Convenience directive that allows to reference a hosted source (e.g. on GitHub)
  *
  * For full documentation see the section about
  * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html#navigation Navigation Directives]]
  * in the manual.
  *
  * @author Jens Halm
  */
private[laika] object LinkDirectives {

  private def linkConfig[T](
      cursor: DocumentCursor,
      extract: LinkConfig => Seq[T]
  ): Either[String, Seq[T]] =
    linkConfig(cursor).map(extract)

  private def linkConfig[T](cursor: DocumentCursor): Either[String, LinkConfig] =
    cursor.config
      .getOpt[LinkConfig]
      .map(_.getOrElse(LinkConfig.empty))
      .leftMap(_.message)

  /** Implementation of the `api` directive that creates links to API documentation based
    * on a specified fully-qualified type name. The type name is the only (required) attribute
    * of the directive.
    *
    * The directive relies on base URIs defined in the transformation's configuration and will
    * otherwise fail. See [[laika.rewrite.link.LinkConfig]] for details.
    */
  lazy val api: Links.Directive = Links.eval("api") { (linkId, cursor) =>
    linkConfig(cursor)
      .flatMap { linkConfig =>
        val matching = linkConfig.apiLinks.toList.filter(l =>
          linkId.startsWith(l.packagePrefix)
        ).maximumByOption(_.packagePrefix.length)
        matching.orElse(linkConfig.apiLinks.find(_.packagePrefix == "*")).fold[Either[
          String,
          SpanLink
        ]](
          Left(s"No base URI defined for '$linkId' and no default URI available.")
        ) { link =>
          def splitAtLast(in: String, char: Char): (String, Option[String]) =
            in.split(char).toSeq match {
              case Seq(single)  => (single, None)
              case init :+ last => (init.mkString(char.toString), Some(last))
              case _            => ("", None)
            }

          val (fqName, method)         = splitAtLast(linkId, '#')
          val (packageName, className) = splitAtLast(fqName, '.')
          val isPackage                = className.contains("package")

          val linkText = {
            val typeText =
              if (isPackage) packageName else className.getOrElse(fqName).stripSuffix("$")
            typeText + method.fold("")(m => "." + m.split('(').head)
          }

          val uri = {
            val typePath =
              if (isPackage) packageName.replace(".", "/") + "/" + link.packageSummary
              else fqName.replace(".", "/") + ".html"
            link.baseUri + typePath + method.fold("")("#" + _)
          }
          cursor.validate(SpanLink(Target.parse(uri))(linkText))
        }
      }
  }

  /** Implementation of the `source` directive that creates links to hosted sources based
    * on a specified fully-qualified type name or a path to markup source file.
    * The type name or path is the only (required) attribute of the directive.
    *
    * The directive relies on base URIs defined in the transformation's configuration and will
    * otherwise fail. See [[laika.rewrite.link.LinkConfig]] for details.
    */
  lazy val source: Links.Directive = Links.eval("source") { (linkId, cursor) =>
    linkConfig(cursor, _.sourceLinks)
      .flatMap { sourceLinks =>
        sourceLinks.toList
          .filter(l => linkId.startsWith(l.packagePrefix))
          .maximumByOption(_.packagePrefix.length)
          .orElse(sourceLinks.find(_.packagePrefix == "*"))
          .fold[Either[String, SpanLink]](
            Left(s"No base URI defined for '$linkId' and no default URI available.")
          ) { link =>
            val typePath = linkId.replace(".", "/") + "." + link.suffix
            val uri      = link.baseUri + typePath
            val text     = linkId.split('.').last
            cursor.validate(SpanLink(Target.parse(uri))(text))
          }
      }
  }

}
