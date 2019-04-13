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

package laika.ast

import laika.rewrite.TemplateRewriter

/** Represents a placeholder inline element that needs
 *  to be resolved in a rewrite step.
 *  Useful for elements that need access to the
 *  document, structure, title or configuration before
 *  being fully resolved.
 */
trait SpanResolver extends Span {
  def resolve (cursor: DocumentCursor): Span
}

/** Represents a placeholder block element that needs
 *  to be resolved in a rewrite step.
 *  Useful for elements that need access to the
 *  document, structure, title or configuration before
 *  being fully resolved.
 */
trait BlockResolver extends Block {
  def resolve (cursor: DocumentCursor): Block
}

/** Represents a reference to a value from the context
 *  of the current document. The `ref` attribute
 *  is a simple path expression in dot notation
 *  for accessing properties of an object (or keys
 *  of a Map).
 *
 *  The root elements accessible to such a reference are:
 *
 *  - `document`: the current document with all of its public properties
 *  - `parent`: the parent tree of the current document
 *  - `root`: the root tree
 *  - `config`: all configuration values for the current document,
 *    including those inherited from parent trees
 */
abstract class ContextReference[T <: Span] (ref: String) extends SpanResolver {

  def result (value: Option[Any]): T

  def resolve (cursor: DocumentCursor): Span = cursor.resolveReference(ref) match {
    case Some(s: ElementTraversal[_]) => result(Some(s rewrite TemplateRewriter.rewriteRules(cursor)))
    case other => result(other)
  }
}

/** A context reference specifically for use in template documents.
 */
case class TemplateContextReference (ref: String, options: Options = NoOpt) extends ContextReference[TemplateSpan](ref) with TemplateSpan {

  def result (value: Option[Any]): TemplateSpan = value match {
    case Some(s: TemplateSpan)      => s
    case Some(RootElement(content)) => EmbeddedRoot(content)
    case Some(e: Element)           => TemplateElement(e)
    case Some(other)                => TemplateString(other.toString)
    case None                       => TemplateString("")
  }
}

/** A context reference specifically for use in markup documents.
 */
case class MarkupContextReference (ref: String, options: Options = NoOpt) extends ContextReference[Span](ref) {

  def result (value: Option[Any]): Span = value match {
    case Some(s: Span)    => s
    case Some(e: Element) => TemplateElement(e)
    case Some(other)      => Text(other.toString)
    case None             => Text("")
  }
}


/** The base type for all inline elements that
 *  can be found in a template.
 */
trait TemplateSpan extends Span

/** A container of other TemplateSpan elements.
  */
trait TemplateSpanContainer[Self <: TemplateSpanContainer[Self]] extends ElementContainer[TemplateSpan, Self] with Rewritable[Self] { this: Self =>

  def rewriteTemplateSpans (rules: RewriteRule[TemplateSpan]): Self = rewrite2(RewriteRules(templateRules = rules))

  def rewrite2 (rules: RewriteRules): Self = rules.rewriteTemplateSpans(content).fold(this)(withContent)

  def withContent (newContent: Seq[TemplateSpan]): Self
  
} 

/** Wraps a generic element that otherwise could not be placed directly into
 *  a template document tree. Useful when custom tags which are placed inside
 *  a template produce non-template tree elements.
 */
case class TemplateElement (element: Element, indent: Int = 0, options: Options = NoOpt) extends TemplateSpan with ElementTraversal[TemplateElement] 
                                                                                                              with Rewritable[TemplateElement] {

  def rewrite2 (rules: RewriteRules): TemplateElement = element match {
    case t: TemplateSpan => rules.rewriteTemplateSpans(Seq(t)).fold(this)(_.headOption.fold(copy(element = TemplateSpanSequence(Nil)))(t => copy(element = t)))
    case s: Span => rules.rewriteSpans(Seq(s)).fold(this)(_.headOption.fold(copy(element = SpanSequence(Nil)))(s => copy(element = s)))
    case b: Block => rules.rewriteBlocks(Seq(b)).fold(this)(_.headOption.fold(copy(element = BlockSequence(Nil)))(b => copy(element = b)))
    case r: Rewritable[_] => 
      val rewritten = r.rewrite2(rules).asInstanceOf[Element]
      if (rewritten.eq(element)) this else copy(element = rewritten)
  }
  
}

/** A generic container element containing a list of template spans. Can be used where a sequence
 *  of spans must be inserted in a place where a single element is required by the API.
 *  Usually renderers do not treat the container as a special element and render its children
 *  as s sub flow of the parent container.
 */
case class TemplateSpanSequence (content: Seq[TemplateSpan], options: Options = NoOpt) extends TemplateSpan with TemplateSpanContainer[TemplateSpanSequence] {
  def withContent (newContent: Seq[TemplateSpan]): TemplateSpanSequence = copy(content = newContent)
}

/** A simple string element, representing the parts of a template
 *  that are not detected as special markup constructs and treated as raw text.
 */
case class TemplateString (content: String, options: Options = NoOpt) extends TemplateSpan with TextContainer

/** The root element of a template document tree.
 */
case class TemplateRoot (content: Seq[TemplateSpan], options: Options = NoOpt) extends Block with TemplateSpanContainer[TemplateRoot] {
  def withContent (newContent: Seq[TemplateSpan]): TemplateRoot = copy(content = newContent)
}

/** Companion with a fallback instance for setups without a default template */
object TemplateRoot {

  /** A fallback instance that can be used when no user-specified template
    * is available. It simply inserts the content of the parsed markup document
    * without any surrounding decoration. */
  val fallback = TemplateRoot(List(TemplateContextReference("document.content")))
}

/** The root element of a document tree (originating from text markup) inside a template.
 *  Usually created by a template reference like `{{document.content}}`.
 */
case class EmbeddedRoot (content: Seq[Block], indent: Int = 0, options: Options = NoOpt) extends TemplateSpan with BlockContainer[EmbeddedRoot] {
  def withContent (newContent: Seq[Block]): EmbeddedRoot = copy(content = content)
}
