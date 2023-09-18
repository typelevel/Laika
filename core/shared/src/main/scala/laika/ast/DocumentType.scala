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

package laika.ast

import laika.config.TargetFormats

/** Base type for all document type descriptors.
  */
sealed abstract class DocumentType extends Product

/** Base type for all document type descriptors for text input.
  */
sealed abstract class TextDocumentType extends DocumentType

/** Provides all available DocumentTypes.
  */
object DocumentType {

  /** A configuration document in HOCON format.
    */
  case object Config extends TextDocumentType

  /** A text markup document produced by a parser.
    */
  case object Markup extends TextDocumentType

  /** A template document that might get applied
    *  to a document when it gets rendered.
    */
  case object Template extends TextDocumentType

  /** A style sheet that needs to get passed
    *  to a renderer.
    */
  case class StyleSheet(format: String) extends TextDocumentType

  /** A static file that needs to get copied
    *  over to the output target.
    */
  case class Static(formats: TargetFormats = TargetFormats.All) extends DocumentType

  /** A document that should be ignored and neither
    *  get processed nor copied.
    */
  case object Ignored extends DocumentType

}
