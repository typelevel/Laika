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

package laika.rewrite.nav

import laika.api.config.Config
import laika.config.LaikaKeys
import laika.ast._
import laika.api.config.Config.ConfigResult

/** Responsible for applying the navigation order to the contents of a document tree,
  * either based on user-specified configuration
  * or by the alphabetical order of the names of the documents and subtrees.
  *
  * @author Jens Halm
  */
private[laika] object NavigationOrder {

  def applyTo(
      content: Seq[Cursor],
      config: Config
  ): ConfigResult[Seq[Cursor]] = {

    def reAssignPosition(
        cursor: Cursor,
        index: Int
    ): Cursor = cursor match {
      case doc: DocumentCursor => doc.applyPosition(index)
      case tree: TreeCursor    => tree.applyPosition(index)
    }

    def reAssignPositions(content: Seq[Cursor]): Seq[Cursor] =
      content.zipWithIndex.map { case (cursor, index) =>
        reAssignPosition(cursor, index + 1)
      }

    config
      .getOpt[Seq[String]](LaikaKeys.navigationOrder)
      .map(_.fold {
        content.sortBy {
          case d: DocumentCursor => (0, d.path.name)
          case t: TreeCursor     => (1, t.path.name)
        }
      } { list =>
        content.sortBy { nav =>
          list.indexOf(nav.path.name) match { case -1 => Int.MaxValue; case other => other }
        }
      })
      .map(reAssignPositions)
  }

}
