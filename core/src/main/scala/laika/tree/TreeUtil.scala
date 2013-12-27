/*
 * Copyright 2013 the original author or authors.
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

package laika.tree

import laika.tree.Elements._

/** Generic utilities that help with document tree modifications.
 * 
 *  @author Jens Halm
 */
object TreeUtil {

  
  /** Returns a new instance of the customizable element
   *  without its id.
   */
  def removeId [C <: Customizable] (c: C) = modifyOptions(c, opt => Options(None,opt.styles))

  /** Returns a new instance of the customizable element
   *  with its id set to the specified value, overriding any existing value.
   */
  def setId [C <: Customizable] (c: C, id: String) = modifyOptions(c, opt => Options(Some(id), opt.styles))

  /** Returns a new instance of the customizable element
   *  with its options modified according to the specified function.
   */
  def modifyOptions [C <: Customizable] (c: C, f: Options => Options) = {
    val newElements = (c.productIterator map { 
      case opt:Options => f(opt)   
      case other => other  
    }).toArray
      
    c.getClass.getConstructors()(0)
      .newInstance(newElements.asInstanceOf[Array[AnyRef]]:_*).asInstanceOf[C]
  }
  
  /** Extracts the text from the specified sequence of spans, removing
   *  any formatting or links.
   */
  def extractText (spans: Seq[Span]): String = ("" /: spans) { (acc, span) => span match {
    case Text(content, _)     => acc + content
    case sc: SpanContainer[_] => acc + extractText(sc.content)
    case _ => acc
  }}
  
  /** Extracts all document fragments from the specified sequence of blocks.
   */
  def extractFragments (blocks: Seq[Element]): Map[String,Element] = 
    blocks.collect{case f: DocumentFragment => (f.name, f.root)}.toMap
  
  
}