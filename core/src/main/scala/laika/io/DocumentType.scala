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

package laika.io

import laika.parse.markdown.Markdown
import laika.parse.rst.ReStructuredText
import laika.tree.Paths.Path

/** Base type for all document type descriptors.
 */
sealed abstract class DocumentType

/** Provides all available DocumentTypes.
 */
object DocumentType {
  
  /** A configuration document in the syntax
   *  supported by the Typesafe Config library.
   */
  case object Config extends DocumentType
  
  /** A text markup document produced by a parser.
   */
  case object Markup extends DocumentType
  
  /** A template document that might get applied
   *  to a document when it gets rendered.
   */
  case object Template extends DocumentType
  
  /** A dynamic document that might contain
   *  custom directives that need to get
   *  processed before rendering.
   */
  case object Dynamic extends DocumentType
  
  /** A style sheet that needs to get passed
   *  to a renderer.
   */
  case class StyleSheet (format: String) extends DocumentType
  
  /** A static file that needs to get copied
   *  over to the output target.
   */
  case object Static extends DocumentType
  
  /** A document that should be ignored and neither
   *  get processed nor copied.
   */
  case object Ignored extends DocumentType

}

/** The default implementations for determining the document type
 *  of the input based on its path.
 */
object DefaultDocumentTypeMatcher {
  
  import DocumentType._

  private def suffix (name: String) = name.lastIndexOf(".") match {
    case -1    => ""
    case index => name.drop(index+1)
  }  
  
  private val TemplateName = """.+\.template\.[^\.]+$""".r
  private val DynamicName = """.+\.dynamic\.[^\.]+$""".r
  private val StylesheetName = """.+\.fo.css$""".r // stylesheets for HTML are treated as static documents
  private val ConfigName = """.+\.conf$""".r


  /** The base matcher that recognizes all file types known to Laika
    * except text markup documents, which depend on the installed parsers
    * and need to be created separately.
    */
  val base: PartialFunction[Path, DocumentType] = { case path: Path =>
    path.name match {
      case ConfigName()     => Config
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
