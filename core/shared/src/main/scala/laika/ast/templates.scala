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

import laika.internal.rewrite.ReferenceResolver.CursorKeys
import laika.parse.GeneratedSource

/** The base type for all inline elements that
  *  can be found in a template.
  */
trait TemplateSpan extends Span

/** A container of other TemplateSpan elements.
  */
trait TemplateSpanContainer extends ElementContainer[TemplateSpan] with RewritableContainer {

  type Self <: TemplateSpanContainer

  /** Rewrites all template span children of this container based on the specified rules.
    *
    * Concrete types are expected to support rewriting at least for all standard block, span and template span
    * elements they contain, plus optionally for any other elements that have custom support for rewriting.
    */
  def rewriteTemplateSpans(rules: RewriteRule[TemplateSpan]): Self = rewriteChildren(
    RewriteRules.forTemplates(rules)
  )

  def rewriteChildren(rules: RewriteRules): Self = withContent(rules.rewriteTemplateSpans(content))

  /** Creates a copy of this instance with the specified new content.
    *
    * Implementation note: This method exists to deal with the fact that there is no polymorphic copy method
    * and trades a small bit of boilerplate for avoiding the compile time hit of using shapeless for this.
    */
  def withContent(newContent: Seq[TemplateSpan]): Self

}

/** Common methods for simple template span containers (without additional parameters). */
trait TemplateSpanContainerCompanion {

  type ContainerType

  /** Creates an empty instance */
  def empty: ContainerType = createSpanContainer(Nil)

  /** Create an instance only containing a single TemplateString span */
  def apply(text: String, texts: String*): ContainerType = createSpanContainer(
    (text +: texts).map(TemplateString(_))
  )

  /** Create an instance containing a one or more spans */
  def apply(span: TemplateSpan, spans: TemplateSpan*): ContainerType = createSpanContainer(
    span +: spans.toList
  )

  protected def createSpanContainer(spans: Seq[TemplateSpan]): ContainerType

}

/** Wraps a generic element that otherwise could not be placed directly into
  *  a template document tree. Useful when custom tags which are placed inside
  *  a template produce non-template tree elements.
  */
case class TemplateElement(element: Element, indent: Int = 0, options: Options = Options.empty)
    extends TemplateSpan with ElementTraversal
    with RewritableContainer {
  type Self = TemplateElement

  def rewriteChildren(rules: RewriteRules): TemplateElement =
    copy(element = rules.rewriteElement(element))

  def withOptions(options: Options): TemplateElement = copy(options = options)
}

/** A generic container element containing a list of template spans. Can be used where a sequence
  *  of spans must be inserted in a place where a single element is required by the API.
  *  Usually renderers do not treat the container as a special element and render its children
  *  as s sub flow of the parent container.
  */
case class TemplateSpanSequence(content: Seq[TemplateSpan], options: Options = Options.empty)
    extends TemplateSpan with TemplateSpanContainer {
  type Self = TemplateSpanSequence
  def withContent(newContent: Seq[TemplateSpan]): TemplateSpanSequence = copy(content = newContent)
  def withOptions(options: Options): TemplateSpanSequence              = copy(options = options)
}

object TemplateSpanSequence extends TemplateSpanContainerCompanion {
  type ContainerType = TemplateSpanSequence

  /** Create an instance containing one or more spans, translating them to TemplateSpans */
  def adapt(spans: Seq[Span]): ContainerType = {
    val templateSpans = spans.map {
      case ts: TemplateSpan => ts
      case span             => TemplateElement(span)
    }
    createSpanContainer(templateSpans)
  }

  /** Create an instance containing one or more spans, translating them to TemplateSpans */
  def adapt(span: Span, spans: Span*): ContainerType = adapt(span +: spans.toList)

  protected def createSpanContainer(spans: Seq[TemplateSpan]): ContainerType = TemplateSpanSequence(
    spans
  )

}

/** A simple string element, representing the parts of a template
  *  that are not detected as special markup constructs and treated as raw text.
  */
case class TemplateString(content: String, options: Options = Options.empty) extends TemplateSpan
    with TextContainer {
  type Self = TemplateString
  def withOptions(options: Options): TemplateString = copy(options = options)
}

/** The root element of a template document tree.
  */
case class TemplateRoot(content: Seq[TemplateSpan], options: Options = Options.empty) extends Block
    with TemplateSpan with TemplateSpanContainer {
  type Self = TemplateRoot
  def withContent(newContent: Seq[TemplateSpan]): TemplateRoot = copy(content = newContent)
  def withOptions(options: Options): TemplateRoot              = copy(options = options)
}

/** Companion with a fallback instance for setups without a default template */
object TemplateRoot extends TemplateSpanContainerCompanion {

  type ContainerType = TemplateRoot
  protected def createSpanContainer(spans: Seq[TemplateSpan]): ContainerType = TemplateRoot(spans)

  /** A fallback instance that can be used when no user-specified template
    * is available. It simply inserts the content of the parsed markup document
    * without any surrounding decoration.
    */
  val fallback: TemplateRoot = TemplateRoot(
    TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
  )

}

/** The root element of a document tree (originating from text markup) inside a template.
  *  Usually created by a template reference like `\${cursor.currentDocument.content}`.
  */
case class EmbeddedRoot(content: Seq[Block], indent: Int = 0, options: Options = Options.empty)
    extends TemplateSpan with BlockContainer {
  type Self = EmbeddedRoot
  def withContent(newContent: Seq[Block]): EmbeddedRoot = copy(content = newContent)
  def withOptions(options: Options): EmbeddedRoot       = copy(options = options)
}

object EmbeddedRoot extends BlockContainerCompanion {
  type ContainerType = EmbeddedRoot
  override protected def createBlockContainer(blocks: Seq[Block]) = EmbeddedRoot(blocks)
}
