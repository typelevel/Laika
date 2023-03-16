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

import laika.parse.code.CodeCategory

/** A simple text element.
  */
case class Text(content: String, options: Options = NoOpt) extends Span with TextContainer {
  type Self = Text
  def withOptions(options: Options): Text = copy(options = options)
}

/** A span of emphasized inline elements that may contain nested spans.
  */
case class Emphasized(content: Seq[Span], options: Options = NoOpt) extends Span
    with SpanContainer {
  type Self = Emphasized
  def withContent(newContent: Seq[Span]): Emphasized = copy(content = newContent)
  def withOptions(options: Options): Emphasized      = copy(options = options)
}

object Emphasized extends SpanContainerCompanion {
  type ContainerType = Emphasized
  protected def createSpanContainer(spans: Seq[Span]): Emphasized = Emphasized(spans)
}

/** A span of strong inline elements that may contain nested spans.
  */
case class Strong(content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Strong
  def withContent(newContent: Seq[Span]): Strong = copy(content = newContent)
  def withOptions(options: Options): Strong      = copy(options = options)
}

object Strong extends SpanContainerCompanion {
  type ContainerType = Strong
  protected def createSpanContainer(spans: Seq[Span]): Strong = Strong(spans)
}

/** A span containing plain, unparsed text.
  */
case class Literal(content: String, options: Options = NoOpt) extends Span with TextContainer {
  type Self = Literal
  def withOptions(options: Options): Literal = copy(options = options)
}

/** A span of program code. The content is a sequence of spans to support
  *  the integration of syntax highlighting systems. Without this support
  *  the sequence will only consist of a single `Text` element.
  */
case class InlineCode(language: String, content: Seq[Span], options: Options = NoOpt) extends Span
    with SpanContainer {
  type Self = InlineCode
  def withContent(newContent: Seq[Span]): InlineCode = copy(content = newContent)
  def withOptions(options: Options): InlineCode      = copy(options = options)
}

/** A span representing deleted inline elements that may contain nested spans.
  */
case class Deleted(content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Deleted
  def withContent(newContent: Seq[Span]): Deleted = copy(content = newContent)
  def withOptions(options: Options): Deleted      = copy(options = options)
}

object Deleted extends SpanContainerCompanion {
  type ContainerType = Deleted
  protected def createSpanContainer(spans: Seq[Span]): Deleted = Deleted(spans)
}

/** A span representing inserted inline elements that may contain nested spans.
  */
case class Inserted(content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Inserted
  def withContent(newContent: Seq[Span]): Inserted = copy(content = newContent)
  def withOptions(options: Options): Inserted      = copy(options = options)
}

object Inserted extends SpanContainerCompanion {
  type ContainerType = Inserted
  protected def createSpanContainer(spans: Seq[Span]): Inserted = Inserted(spans)
}

/** A generic container element containing a list of spans. Can be used where a sequence
  *  of spans must be inserted in a place where a single element is required by the API.
  *  Usually renderers do not treat the container as a special element and render its children
  *  as s sub flow of the parent container. A span sequence is special in that in can be
  *  used as both a span and a block.
  */
case class SpanSequence(content: Seq[Span], options: Options = NoOpt) extends Block with Span
    with SpanContainer {
  type Self = SpanSequence
  def withContent(newContent: Seq[Span]): SpanSequence = copy(content = newContent)
  def withOptions(options: Options): SpanSequence      = copy(options = options)
}

object SpanSequence extends SpanContainerCompanion {
  type ContainerType = SpanSequence
  protected def createSpanContainer(spans: Seq[Span]): SpanSequence = SpanSequence(spans)
}

/** Represents a section number, usually used in header elements
  *  when autonumbering is applied.
  */
case class SectionNumber(position: Seq[Int], options: Options = NoOpt) extends Span
    with TextContainer {

  type Self = SectionNumber

  val content = position.mkString(".") + " "

  /** Creates a new instance for a child section
    *  of this section at the specified position.
    */
  def child(childPosition: Int) = SectionNumber(position :+ childPosition)

  def withOptions(options: Options): SectionNumber = copy(options = options)
}

/** A single span inside a code block that has been
  * categorized by a syntax highlighter.
  */
sealed trait CategorizedCode extends Span

/** A span of code associated with zero or more code categories.
  */
case class CodeSpan(content: String, categories: Set[CodeCategory], options: Options = NoOpt)
    extends CategorizedCode with TextContainer {
  type Self = CodeSpan
  def withOptions(options: Options): CodeSpan = copy(options = options)
}

object CodeSpan {

  def apply(content: String, category: CodeCategory): CodeSpan = apply(content, Set(category))

  def apply(content: String): CodeSpan = apply(content, Set(), NoOpt)

}

object CodeSpans {

  /** Extracts all code spans from the given span while at the same time
    * converting all regular text nodes to code spans associated with the specified
    * set of categories.
    *
    * This is a fairly low-level operation, usually performed after using a generic
    * inline parser (like `InlineParsers.spans`) for syntax highlighting.
    */
  def extract(defaultCategories: Set[CodeCategory] = Set())(span: Span): Seq[CodeSpan] =
    span match {
      case Text(content, _)          => Seq(CodeSpan(content, defaultCategories))
      case codeSpan: CodeSpan        => Seq(codeSpan)
      case codeSeq: CodeSpanSequence => codeSeq.collect { case cs: CodeSpan => cs }
      case _                         => Nil
    }

  /** Merges all occurrences of two or more adjacent spans with the exact same set of
    * associated code categories.
    */
  def merge(spans: Seq[CodeSpan]): Seq[CodeSpan] = {
    val filtered = spans.filterNot(_.content.isEmpty)
    if (filtered.isEmpty) Nil
    else {
      filtered.tail.foldLeft(List(filtered.head)) { case (acc, next) =>
        if (acc.last.categories == next.categories)
          acc.init :+ CodeSpan(acc.last.content + next.content, next.categories)
        else acc :+ next
      }
    }
  }

}

/** A sequence of code spans where most of them are usually associated with zero or more code categories.
  */
case class CodeSpanSequence(content: Seq[Span], options: Options = NoOpt) extends CategorizedCode
    with SpanContainer {
  type Self = CodeSpanSequence
  def withContent(newContent: Seq[Span]): CodeSpanSequence = copy(content = newContent)
  def withOptions(options: Options): CodeSpanSequence      = copy(options = options)
}

object CodeSpanSequence extends SpanContainerCompanion {
  type ContainerType = CodeSpanSequence
  protected def createSpanContainer(spans: Seq[Span]): CodeSpanSequence = CodeSpanSequence(spans)
}

/** An explicit hard line break.
  */
case class LineBreak(options: Options = NoOpt) extends Span {
  type Self = LineBreak
  def withOptions(options: Options): LineBreak = copy(options = options)
}

case class Reverse(length: Int, target: Span, fallback: Span, options: Options = NoOpt)
    extends Span {
  type Self = Reverse
  def withOptions(options: Options): Reverse = copy(options = options)
}
