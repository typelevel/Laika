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

package laika.rewrite

import laika.tree.Documents.Document
import laika.tree.Documents.DocumentTree
import laika.tree.Documents.TreeContent
import com.typesafe.config.Config

/** Responsible for applying the navigation order to the 
 *  contents of a document tree, either based on user-specified
 *  configuration or by the alphabetical order of the names of 
 *  the documents and subtrees.
 *  
 *  @author Jens Halm
 */
object NavigationOrder {
  
  import scala.collection.JavaConverters._
  
  def applyTo (content: Seq[TreeContent], config: Config): Seq[TreeContent] =
    if (config.hasPath("navigationOrder")) {
      val javaList = config.getList("navigationOrder").unwrapped
      val list = javaList.asScala.collect{ case s:String => s }.toIndexedSeq
      content.sortBy { nav => 
        list.indexOf(nav.path.name) match { case -1 => Int.MaxValue; case other => other }
      }
    }
    else content.sortBy {
      case d: Document => "A-" + d.path.name 
      case t: DocumentTree => "B-" + t.path.name 
    }
  
}
