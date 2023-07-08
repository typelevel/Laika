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
import laika.ast.{ Choice, Selection }
import laika.directive.Blocks
import laika.rewrite.nav.Selections

/** Implementation of the `select` directive for block elements in markup documents.
  *
  * Selections represent the same content in different ways,
  * e.g. a code sample in Scala or Java or a build setup in sbt vs. Maven.
  * In the final output these will usually be rendered in a way to allow for a convenient selection by the user.
  *
  * A valid `@:select` directive has at least two separator directives (`@:choice`) in the body.
  *
  * For full documentation see the section about the
  * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html#select Select Directive]]
  * in the manual.
  *
  * @author Jens Halm
  */
private[laika] object SelectDirective {

  /** Implementation of the `select` directive for block elements in markup documents.
    */
  val forBlocks: Blocks.Directive = Blocks.eval("select") {
    import Blocks.dsl._

    val separator: Blocks.SeparatorDirective[Choice] = Blocks.separator("choice", min = 2) {
      (attribute(0).as[String], parsedBody).mapN { (name, body) =>
        Choice(name, name, body)
      }
    }

    (attribute(0).as[String], separatedBody(Seq(separator)), cursor).mapN {
      (name, multiPart, cursor) =>
        cursor.config.get[Selections]
          .leftMap(e => s"Error reading config for selections: ${e.message}")
          .flatMap { config =>
            config.getSelection(name).toRight(s"Not found: selection '$name'").flatMap {
              selection =>
                multiPart
                  .children.toList
                  .traverse { choice =>
                    selection
                      .getLabel(choice.name)
                      .map(label =>
                        choice.copy(content = multiPart.mainBody ++ choice.content, label = label)
                      )
                      .toValidNec(
                        s"No label defined for choice '${choice.name}' in selection '$name'"
                      )
                  }
                  .map(Selection(name, _))
                  .toEither
                  .leftMap(_.mkString_(", "))
            }
          }
    }
  }

}
