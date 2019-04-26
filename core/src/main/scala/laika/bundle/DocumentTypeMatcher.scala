/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.bundle

import laika.ast.{DocumentType, Path}

/** The default implementations for determining the document type
  *  of the input based on its path.
  *
  *  @author Jens Halm
  */
object DocumentTypeMatcher {

  import DocumentType._

  private def suffix (name: String) = name.lastIndexOf(".") match {
    case -1    => ""
    case index => name.drop(index+1)
  }

  private val TemplateName = """.+\.template\.[^\.]+$""".r
  private val DynamicName = """.+\.dynamic\.[^\.]+$""".r
  private val StylesheetName = """.+\.fo.css$""".r // stylesheets for HTML are treated as static documents
  private val ConfigName = "directory.conf"


  /** The base matcher that recognizes all file types known to Laika
    * except text markup documents, which depend on the installed parsers
    * and need to be created separately.
    */
  val base: PartialFunction[Path, DocumentType] = { case path: Path =>
    path.name match {
      case ConfigName       => Config
      case TemplateName()   => Template
      case DynamicName()    => Dynamic
      case StylesheetName() => StyleSheet("fo")
      case _                => Static
    }
  }

  /** Creates a document type matcher that recognizes all input files
    * with one of the specified file suffixes as a markup document.
    */
  def forMarkup (fileSuffixes: Set[String]): PartialFunction[Path, DocumentType] = {
    case path if fileSuffixes(suffix(path.name)) => Markup
  }

}
