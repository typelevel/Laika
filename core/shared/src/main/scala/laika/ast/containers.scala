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

import laika.api.Renderer
import laika.format.AST

/** A generic container.
  *  Usually not mixed in directly, instead one of the sub-traits
  *  `TextContainer`, `ListContainer`, `SpanContainer` or `BlockContainer` should be used.
  */
trait Container[+T] extends Element {
  def content: T
}

/** A container for plain text.
  */
trait TextContainer extends Container[String]

/** A generic container of other elements.
  *  Provides means to traverse, select and rewrite children of
  *  this container.
  *
  *  Usually not mixed in directly, instead one of the sub-traits
  *  `ListContainer`, `SpanContainer` or `BlockContainer` should be used.
  */
trait ElementContainer[+E <: Element] extends Container[Seq[E]] with ElementTraversal {
  override def toString: String = FormattedElementString.render(this)
}

/** A generic container of child elements which can have
  * rewrite rules applied to them in recursive tree rewriting.
  */
trait RewritableContainer extends Element {

  type Self <: RewritableContainer

  /** Rewrites all children of this container based on the specified rules.
    *
    * Concrete types are expected to support rewriting at least for all standard block, span and template span
    * elements they contain, plus optionally for any other elements that have custom support for rewriting.
    */
  def rewriteChildren (rules: RewriteRules): Self

  /** Rewrites all span children of this container based on the specified rules.
    *
    * Concrete types are expected to support rewriting at least for all standard block, span and template span
    * elements they contain, plus optionally for any other elements that have custom support for rewriting.
    */
  def rewriteSpans (rule: RewriteRule[Span]): Self = rewriteChildren(RewriteRules(spanRules = Seq(rule)))

}

/** A container of other Block elements. Such a container is usually
  *  also a Block itself.
  */
trait BlockContainer extends ElementContainer[Block] with RewritableContainer {

  type Self <: BlockContainer

  /** Rewrites all block children of this container based on the specified rules.
    *
    * Concrete types are expected to support rewriting at least for all standard block, span and template span
    * elements they contain, plus optionally for any other elements that have custom support for rewriting.
    */
  def rewriteBlocks (rules: RewriteRule[Block]): Self = rewriteChildren(RewriteRules(blockRules = Seq(rules)))

  def rewriteChildren (rules: RewriteRules): Self = withContent(rules.rewriteBlocks(content))

  /** Creates a copy of this instance with the specified new content.
    *
    * Implementation note: This method exists to deal with the fact that there is no polymorphic copy method
    * and trades a small bit of boilerplate for avoiding the compile time hit of using shapeless for this.
    */
  def withContent (newContent: Seq[Block]): Self

}

/** A container of other Span elements. Such a container may be a Block
  *  or a Span itself.
  */
trait SpanContainer extends ElementContainer[Span] with RewritableContainer {

  type Self <: SpanContainer

  def rewriteChildren (rules: RewriteRules): Self = withContent(rules.rewriteSpans(content))

  /** Creates a copy of this instance with the specified new content.
    *
    * Implementation note: This method exists to deal with the fact that there is no polymorphic copy method
    * and trades a small bit of boilerplate for avoiding the compile time hit of using shapeless for this.
    */
  def withContent (newContent: Seq[Span]): Self

  /**  Extracts the text from the spans of this container, removing
    *  any formatting or links.
    */
  def extractText: String = content.map {
    case tc: TextContainer => tc.content
    case sc: SpanContainer => sc.extractText
    case _ => ""
  }.mkString

}

/** A container of list items. Such a container is usually a Block itself.
  */
trait ListContainer extends ElementContainer[ListItem]

/** Common methods for simple span containers (without additional parameters). */
trait SpanContainerCompanion {

  type ContainerType

  /** Creates an empty instance */
  def empty: ContainerType = createSpanContainer(Nil)

  /** Create an instance only containing only one or more Text spans */
  def apply(text: String, texts: String*): ContainerType = createSpanContainer((text +: texts).map(Text(_)))

  /** Create an instance containing one or more spans */
  def apply(span: Span, spans: Span*): ContainerType = createSpanContainer(span +: spans.toList)

  protected def createSpanContainer (spans: Seq[Span]): ContainerType
}

/** Common methods for simple block containers (without additional parameters). */
trait BlockContainerCompanion extends SpanContainerCompanion {

  override def empty: ContainerType = createBlockContainer(Nil)

  protected def createSpanContainer (spans: Seq[Span]): ContainerType = createBlockContainer(Seq(Paragraph(spans)))

  /** Create an instance containing one or more blocks */
  def apply(block: Block, blocks: Block*): ContainerType = createBlockContainer(block +: blocks.toList)

  protected def createBlockContainer (blocks: Seq[Block]): ContainerType
}

private[ast] object FormattedElementString {
  private lazy val renderer: Renderer = Renderer.of(AST).build
  def render (elem: Element): String = "\n" + renderer.render(elem) + "\n"
}
