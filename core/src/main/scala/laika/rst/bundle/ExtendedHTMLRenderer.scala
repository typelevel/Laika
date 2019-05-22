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

package laika.rst.bundle

import laika.ast._
import laika.render.HTMLFormatter
import laika.rst.ast._

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
class ExtendedHTMLRenderer {

  
  private case class ProgramOptions (opts: Seq[Element], options: Options = NoOpt) extends Block
  
  /** Converts an `OptionList` to an interim table model for rendering.
   */
  def toTable (ol: OptionList): Table = {
    def intersperse [T](list: List[T], sep: T): List[T] = list match {    
      case one :: two :: rest => one :: sep :: intersperse(two :: rest, sep)    
      case short              => short   
    }
    def options (value: Seq[ProgramOption]) = Cell(BodyCell, List(ProgramOptions(intersperse(value.toList,Text(", ")))))
    def body (value: Seq[Block]) = Cell(BodyCell, value)
    val rows = ol.content map (o => Row(List(options(o.programOptions),body(o.content))))
    Table(TableHead(Nil), TableBody(rows), Caption(),
        Columns.options(Styles("option"),Styles("description")), Styles("option-list"))
  }
  
  /** Converts a `FieldList` to an interim table model for rendering.
   */
  def toTable (fl: FieldList): Table = {
    def name (value: Seq[Span]) = Cell(HeadCell, List(SpanSequence(value :+ Text(":"))))
    def body (value: Seq[Block]) = Cell(BodyCell, value)
    val rows = fl.content map (f => Row(List(name(f.name),body(f.content))))
    Table(TableHead(Nil), TableBody(rows), Caption(),
        Columns.options(Styles("field-name"),Styles("field-body")), Styles("field-list"))
  }
  
  val custom: PartialFunction[(HTMLFormatter, Element), String] = {
    case (fmt, DoctestBlock(content,opt))  => fmt.withoutIndentation(_.textElement("pre", opt, ">>> "+content, "class" -> "doctest-block"))
    case (fmt, fl: FieldList)              => fmt.child(toTable(fl))
    case (fmt, ol: OptionList)             => fmt.child(toTable(ol))
    case (fmt, ProgramOptions(options,opt))=> fmt.element("kbd", opt, options)
    case (fmt, ProgramOption(name,arg))    => fmt.element("span", NoOpt, Seq(Text(name), arg.getOrElse(Text(""))), "class" -> "option")
    case (_, OptionArgument(value,delim))  => s"$delim<var>$value</var>"
  }


}

object ExtendedHTMLRenderer extends ExtendedHTMLRenderer
