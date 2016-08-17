/*
 * Copyright 2016 the original author or authors.
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

package laika.rewrite

import laika.tree.Elements.Span
import laika.tree.Elements.Text
import laika.tree.Elements.Styles

/**
 * @author Jens Halm
 */
trait TitleNumber {

  def styleName: String

  def appendText(text: String, sep: String = "."): Seq[Span] = append(List(Text(text)), sep)
  
  def append(spans: Seq[Span], sep: String = "."): Seq[Span] =
    if (isDefined) Text(toString(sep) + " ", Styles(styleName)) +: spans
    else spans
  
  def toSeq: Seq[Int]
  
  override def toString: String = toString(".")
  
  def toString(sep: String): String = toSeq.mkString(sep)
  
  def isDefined: Boolean = toSeq.nonEmpty
  
}

case class SectionNumber(toSeq: Seq[Int]) extends TitleNumber {
  
  val styleName = "sectionNumber"
  
  def child(num: Int) = SectionNumber(toSeq :+ num)
}

case class DocumentNumber(toSeq: Seq[Int]) extends TitleNumber {
  
  val styleName = "titleNumber"
  
  def childDoc(num: Int) = DocumentNumber(toSeq :+ num)
  def section(num: Int) = SectionNumber(toSeq :+ num)
}
