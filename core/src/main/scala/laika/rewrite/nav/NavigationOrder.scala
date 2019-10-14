/*
 * Copyright 2016 the original author or authors.
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
import laika.ast._

/** Responsible for applying the navigation order to the 
 *  contents of a document tree, either based on user-specified
 *  configuration or by the alphabetical order of the names of 
 *  the documents and subtrees.
 *  
 *  @author Jens Halm
 */
object NavigationOrder {
  
  import scala.collection.JavaConverters._
  
  def applyTo (content: Seq[Cursor], config: Config, parentPosition: TreePosition): Seq[Cursor] = {

    def reAssignPosition (cursor: Cursor, position: TreePosition, configF: Config => Config = identity): Cursor = cursor match {
      case doc: DocumentCursor => doc.copy(
        position = position,
        target = doc.target.copy(position = position),
        config = configF(cursor.config)
      )
      case tree: TreeCursor => tree.copy(
        position = position,
        target = tree.target.copy(position = position),
        config = configF(cursor.config)
      )
    }

    def reAssignPositions (content: Seq[Cursor]): Seq[Cursor] =
      content.zipWithIndex.map {
        case (cursor, index) => reAssignPosition(cursor, parentPosition.forChild(index + 1))
      }

    val (titleDoc, otherDocs) = content.partition {
      case d: DocumentCursor if d.path.basename == "title" => true
      case _ => false
    }

    val sorted = config.get[Seq[String]]("navigationOrder").toOption.fold {
      otherDocs.sortBy {
        case d: DocumentCursor => "A-" + d.path.name
        case t: TreeCursor => "B-" + t.path.name
      }
    } { list =>
      otherDocs.sortBy { nav =>
        list.indexOf(nav.path.name) match { case -1 => Int.MaxValue; case other => other }
      }
    } 

    titleDoc.map(reAssignPosition(_, parentPosition, AutonumberConfig.withoutSectionNumbering)) ++
      reAssignPositions(sorted)
  }

}
