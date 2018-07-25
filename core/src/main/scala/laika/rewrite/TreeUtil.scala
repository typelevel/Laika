/*
 * Copyright 2013-2016 the original author or authors.
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

import laika.tree.Elements._

/** Generic utilities that help with document tree modifications.
 * 
 *  @author Jens Halm
 */
object TreeUtil {

  /** Extracts all document fragments from the specified sequence of blocks.
   */
  def extractFragments (blocks: Seq[Element]): Map[String,Element] = 
    blocks.collect{case f: DocumentFragment => (f.name, f.root)}.toMap

  def extractConfigValues (container: ElementContainer[_,_]): Map[String,AnyRef] =
    container.collect{case c: ConfigValue => (c.name, c.value)}.toMap
  
  
}
