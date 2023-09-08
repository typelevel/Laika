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

package laika.rst.bundle

import laika.ast.*
import laika.render.TagFormatter
import laika.rst.ast.*

/** HTML renderer for special reStructuredText tree elements not part of the default document tree model.
  *
  *  The following tree elements are specific to reStructuredText and are not included in the default model:
  *
  *  - `FieldList` and corresponding child elements
  *  - `OptionList` and corresponding child elements
  *  - `DoctestBlock`
  *
  *  FieldLists being part of a directive declaration will be processed by the default parser, the `FieldList`
  *  element only appears in the final document model if field lists are used outside of directives.
  *
  *  @author Jens Halm
  */
private[laika] class ExtendedHTMLRenderer {

  private case class ProgramOptions(content: Seq[Element], options: Options = NoOpt) extends Block
      with ElementContainer[Element] {
    type Self = ProgramOptions
    def withOptions(options: Options): ProgramOptions = copy(options = options)
  }

  /** Converts an `OptionList` to an interim table model for rendering.
    */
  private def toTable(ol: OptionList): Table = {
    def intersperse[T](list: List[T], sep: T): List[T] = list match {
      case one :: two :: rest => one :: sep :: intersperse(two :: rest, sep)
      case short              => short
    }
    def options(value: Seq[ProgramOption])             = BodyCell(
      ProgramOptions(intersperse(value.toList, Text(", ")))
    )
    def body(value: Seq[Block])                        = BodyCell(value)
    val rows = ol.content map (o => Row(List(options(o.programOptions), body(o.content))))
    Table(
      TableHead(Nil),
      TableBody(rows),
      Caption(),
      Columns.options(RstStyle.option, RstStyle.description),
      RstStyle.optionList
    )
  }

  /** Converts a `FieldList` to an interim table model for rendering.
    */
  private def toTable(fl: FieldList): Table = {
    def name(value: Seq[Span])  = HeadCell(SpanSequence(value :+ Text(":")))
    def body(value: Seq[Block]) = BodyCell(value)
    val rows                    = fl.content map (f => Row(List(name(f.name), body(f.content))))
    Table(
      TableHead(Nil),
      TableBody(rows),
      Caption(),
      Columns.options(RstStyle.fieldName, RstStyle.fieldBody),
      RstStyle.fieldList
    )
  }

  val custom: PartialFunction[(TagFormatter, Element), String] = {
    case (fmt, DoctestBlock(content, opt))    =>
      fmt.withoutIndentation(
        _.textElement("pre", Text(">>> " + content).withOptions(opt), "class" -> "doctest-block")
      )
    case (fmt, fl: FieldList)                 => fmt.child(toTable(fl))
    case (fmt, ol: OptionList)                => fmt.child(toTable(ol))
    case (fmt, po: ProgramOptions)            => fmt.element("kbd", po)
    case (fmt, ProgramOption(name, arg, opt)) =>
      val spans = SpanSequence(Text(name), arg.getOrElse(Text(""))).withOptions(opt)
      fmt.element("span", spans, "class" -> "option")
    case (_, OptionArgument(value, delim, _)) => s"$delim<var>$value</var>"
    case (fmt, l: Line)                       => fmt.element("div", l.mergeOptions(RstStyle.line))
    case (fmt, lb: LineBlock)                 =>
      fmt.indentedElement("div", lb.mergeOptions(RstStyle.lineBlock))
  }

}

private[laika] object ExtendedHTMLRenderer extends ExtendedHTMLRenderer
