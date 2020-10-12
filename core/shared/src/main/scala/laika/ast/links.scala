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

import laika.parse.SourceFragment

/** An internal or external link target that can be referenced by id, usually only part of the raw document tree and then
  * removed by the rewrite rule that resolves link and image references.
  */
case class LinkDefinition (id: String, target: Target, title: Option[String] = None, options: Options = NoOpt) extends Definition with Hidden
  with Span {
  type Self = LinkDefinition
  def withOptions (options: Options): LinkDefinition = copy(options = options)
}

/** A link target pointing to another link target, acting like an alias.
 */
case class LinkAlias (id: String, target: String, options: Options = NoOpt) extends Definition with Span with Hidden {
  type Self = LinkAlias
  def withOptions (options: Options): LinkAlias = copy(options = options)
}

/** A footnote definition that needs to be resolved to a final footnote by a rewrite rule based on the label type.
 */
case class FootnoteDefinition (label: FootnoteLabel, 
                               content: Seq[Block], 
                               source: SourceFragment, 
                               options: Options = NoOpt) extends Definition with BlockContainer with Unresolved {
  type Self = FootnoteDefinition
  def withContent (newContent: Seq[Block]): FootnoteDefinition = copy(content = newContent)
  def withOptions (options: Options): FootnoteDefinition = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved footnote definition with label '$label'"
}

/** Points to the following block or span element, making it a target for links.
 */
case class InternalLinkTarget (options: Options = NoOpt) extends Block with Span with LinkTarget {
  type Self = InternalLinkTarget
  def withOptions (options: Options): InternalLinkTarget = copy(options = options)
}

/** A citation that can be referred to by a `CitationLink` by id.
 */
case class Citation (label: String, content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                with LinkTarget
                                                                                with BlockContainer {
  type Self = Citation
  def withContent (newContent: Seq[Block]): Citation = copy(content = newContent)
  def withOptions (options: Options): Citation = copy(options = options)
}

/** A footnote with resolved id and label that can be referred to by a `FootnoteLink` by id.
 */
case class Footnote (label: String, content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                               with LinkTarget
                                                                                               with BlockContainer {
  type Self = Footnote
  def withContent (newContent: Seq[Block]): Footnote = copy(content = newContent)
  def withOptions (options: Options): Footnote = copy(options = options)
}

/** Base type for all types of footnote labels.
 */
abstract class FootnoteLabel

/** Label with automatic numbering.
 */
case object Autonumber extends FootnoteLabel

/** Label with automatic symbol assignment.
 */
case object Autosymbol extends FootnoteLabel

/** Explicit numeric label.
 */
case class NumericLabel (number: Int) extends FootnoteLabel

/** Label using automatic numbering and explicit label names together.
 */
case class AutonumberLabel (label: String) extends FootnoteLabel

/** An link element, with the span content representing the text (description) of the link.
  */
case class SpanLink (content: Seq[Span], target: Target, title: Option[String] = None, options: Options = NoOpt) extends GlobalLink
  with SpanContainer {
  type Self = SpanLink
  val supportsExternalTargets: Boolean = true
  def withTarget (newTarget: Target): SpanLink = copy(target = newTarget)
  def withContent (newContent: Seq[Span]): SpanLink = copy(content = newContent)
  def withOptions (options: Options): SpanLink = copy(options = options)
}

/** A resolved link to a footnote.
 */
case class FootnoteLink (refId: String, label: String, options: Options = NoOpt) extends LocalLink {
  type Self = FootnoteLink
  def withOptions (options: Options): FootnoteLink = copy(options = options)
}

/** A resolved link to a citation.
 */
case class CitationLink (refId: String, label: String, options: Options = NoOpt) extends LocalLink {
  type Self = CitationLink
  def withOptions (options: Options): CitationLink = copy(options = options)
}

/** An inline image optional title, alt and size properties.
 */
case class Image (target: Target,
                  width: Option[Length] = None,
                  height: Option[Length] = None,
                  alt: Option[String] = None,
                  title: Option[String] = None,
                  options: Options = NoOpt) extends GlobalLink {
  type Self = Image
  val supportsExternalTargets: Boolean = false // has to be embedded for EPUB or PDF
  def withTarget (newTarget: Target): Image = copy(target = newTarget)
  def withOptions (options: Options): Image = copy(options = options)
}

object Image {
  def create (url: String, source: SourceFragment, width: Option[Length] = None,
              height: Option[Length] = None, alt: Option[String] = None, title: Option[String] = None): Span =
    Target.parseInternal(url) match {
      case Right(external) => Image(external, width, height, alt, title)
      case Left(internal)  => ImagePathReference(internal.path, source, width, height, alt, title)
    }
}

/** Represents a font-based icon, identified by its code point.
  * Ideally theme authors provide constants for icons provided out of the box,
  * so that the user does not have to look up or memorize the hex code point.
  * 
  * This avoids the indirection of common approaches where the rendered HTML contains
  * an empty tag with a class which specifies the code point with a `:before` pseudo-class.
  * This approach would currently not work well with Laika's PDF support which is
  * not based on an interim HTML renderer.
  */
case class Icon (codePoint: Char, title: Option[String] = None, options: Options = NoOpt) extends Span {
  def codePointAsEntity: String = s"&#x${Integer.toHexString(codePoint)};"
  type Self = Icon
  def withOptions(newOptions: Options): Icon = copy(options = newOptions)
}

object ParsedLink {
  /** Creates a new span that acts as a link reference based on the specified
    * URL which will be parsed and interpreted as an internal or external target.
    */
  def create (linkText: Seq[Span], url: String, source: SourceFragment, title: Option[String] = None): Span =
    Target.parseInternal(url) match {
      case Right(external) => SpanLink(linkText, external, title)
      case Left(internal)  => LinkPathReference(linkText, internal.path, source, title)
    }
}

object LinkDefinition {
  /** Creates a new link definition that other references can point to based on the specified
    * URL which will be parsed and interpreted as an internal or external target.
    */
  def create (id: String, url: String, title: Option[String] = None): Block with Span = 
    LinkDefinition(id, Target.parse(url), title)
}

/** A reference to content within the virtual input tree, the path pointing to the source path.
  * Only part of the unresolved document tree and then removed by the rewrite rule that 
  * replace the source path with the final target path of the output document, which might
  * differ in more than just the file suffix, depending on configuration.
  */
trait PathReference extends Reference {
  /** The content (section or document or image) this reference points to. */
  def path: RelativePath
  /** Creates the final AST element based on the resolved target. */
  def resolve(target: Target): Link
}

/** A reference to content within the virtual input tree, the path pointing to the source path.
  * Only part of the unresolved document tree and then removed by the rewrite rule that 
  * replace the source path with the final target path of the output document, which might
  * differ in more than just the file suffix, depending on configuration.
  */
case class LinkPathReference(content: Seq[Span],
                             path: RelativePath,
                             source: SourceFragment,
                             title: Option[String] = None,
                             options: Options = NoOpt) extends PathReference with SpanContainer {
  type Self = LinkPathReference
  def withContent (newContent: Seq[Span]): LinkPathReference = copy(content = newContent)
  def withOptions (options: Options): LinkPathReference = copy(options = options)
  def resolve(target: Target): Link = SpanLink(content, target, title, options)
  lazy val unresolvedMessage: String = s"Unresolved internal reference to '${path.toString}'"
}

/** An image reference to content within the virtual input tree, the path pointing to the source path.
  * Only part of the unresolved document tree and then removed by the rewrite rule that 
  * replace the source path with the final target path of the output document, resolving any
  * relative path references in the process.
  */
case class ImagePathReference (path: RelativePath,
                               source: SourceFragment,
                               width: Option[Length] = None,
                               height: Option[Length] = None,
                               alt: Option[String] = None,
                               title: Option[String] = None,
                               options: Options = NoOpt) extends PathReference {
  type Self = ImagePathReference
  def withOptions (options: Options): ImagePathReference = copy(options = options)
  def resolve(target: Target): Link = Image(target, width, height, alt, title, options)
  lazy val unresolvedMessage: String = s"Unresolved internal reference to image with path '$path'"
}

/** An image reference, the id pointing to the id of a `LinkTarget`. Only part of the
 *  raw document tree and then removed by the rewrite rule that resolves link and image references.
 */
case class ImageIdReference (text: String, id: String, source: SourceFragment, options: Options = NoOpt) extends Reference {
  type Self = ImageIdReference
  def withOptions (options: Options): ImageIdReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved reference to image definition with id '$id'"
}

/** A reference to a footnote with a matching label.  Only part of the
 *  raw document tree and then removed by the rewrite rule that resolves link and image references.
 */
case class FootnoteReference (label: FootnoteLabel, source: SourceFragment, options: Options = NoOpt) extends Reference {
  type Self = FootnoteReference
  def withOptions (options: Options): FootnoteReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved reference to footnote with label '$label'"
}

/** A reference to a citation with a matching label.  Only part of the
 *  raw document tree and then removed by the rewrite rule that resolves link and image references.
 */
case class CitationReference (label: String, source: SourceFragment, options: Options = NoOpt) extends Reference {
  type Self = CitationReference
  def withOptions (options: Options): CitationReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved reference to citation with label '$label'"
}

/** A reference to any kind of referencable object, e.g. a link definition or an internal target.
  * 
  * The reference can be local, in the same document, or anywhere else in the input tree, as long
  * as the id is not ambiguous. 
  * Search for a matching target happens recursively, from the current document, 
  * to the current tree (directory) upwards to the root tree.
  */
case class LinkIdReference (content: Seq[Span], ref: String, source: SourceFragment, options: Options = NoOpt) extends Reference
  with SpanContainer {
  type Self = LinkIdReference
  def withContent (newContent: Seq[Span]): LinkIdReference = copy(content = newContent)
  def withOptions (options: Options): LinkIdReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved link id reference '$ref'"
}
