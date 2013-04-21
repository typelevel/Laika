/*
 * Copyright 2013 the original author or authors.
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

package laika.parse.rst

import laika.render.HTMLWriter
import laika.tree.Elements._
import laika.parse.rst.Elements._

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
 *  The renderer must be applied explicitly when any of these constructs are used in the markup:
 *  
 *  {{{
 *  val transform = Transform from ReStructuredText to HTML rendering ExtendedHTML
 *  }}}
 * 
 *  @author Jens Halm
 */
class ExtendedHTML extends (HTMLWriter => PartialFunction[Element, Unit]) {

  
  private case class ProgramOptions (opts: Seq[Element], options: Options = NoOpt) extends Block
  
  /** Converts an `OptionList` to an interim table model for rendering.
   */
  def toTable (ol: OptionList) = {
    def intersperse [T](list: List[T], sep: T): List[T] = list match {    
      case one :: two :: rest => one :: sep :: intersperse(two :: rest, sep)    
      case short              => short   
    }
    def options (value: Seq[ProgramOption]) = Cell(BodyCell, List(ProgramOptions(intersperse(value.toList,Text(", ")))))
    def body (value: Seq[Block]) = Cell(BodyCell, value)
    val rows = ol.content map (o => Row(List(options(o.programOptions),body(o.content))))
    Table(TableHead(Nil), TableBody(rows), 
        Columns.options(Styles("option"),Styles("description")), Styles("option-list"))
  }
  
  /** Converts a `FieldList` to an interim table model for rendering.
   */
  def toTable (fl: FieldList) = {
    def name (value: Seq[Span]) = Cell(BodyCell, List(SpanSequence(value :+ Text(":"))))
    def body (value: Seq[Block]) = Cell(BodyCell, value)
    val rows = fl.content map (f => Row(List(name(f.name),body(f.content))))
    Table(TableHead(Nil), TableBody(rows),
        Columns.options(Styles("field-name"),Styles("field-body")), Styles("field-list"))
  }
  
  def apply (out: HTMLWriter): PartialFunction[Element, Unit] = {
    
    val pf: PartialFunction[Element, Unit] = {
      case DoctestBlock(content,_)    => out << """<pre class="doctest-block">""" <<<& (">>> "+content) <<  "</pre>"
      case fl: FieldList              => out << toTable(fl)
      case ol: OptionList             => out << toTable(ol)
      case ProgramOptions(options,_)  => out << "<kbd>" << options << "</kbd>"
      case ProgramOption(name,arg)    => out << """<span class="option">""" << name << arg.getOrElse(Text("")) << "</span>"
      case OptionArgument(value,delim)=> out << delim << "<var>" << value << "</var>"
    } 
    
    pf
      
  }
  
  
  
}

object ExtendedHTML extends ExtendedHTML