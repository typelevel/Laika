/*
 * Copyright 2013-2017 the original author or authors.
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

import laika.parse.core.Parser
import laika.parse.core.text.{Characters, TextParsers}
import laika.parse.core.text.TextParsers._
import laika.tree.Elements._

/**
  * @author Jens Halm
  */
object BaseParsers {


  /** Parses punctuation characters as supported by transitions (rules) and
    *  overlines and underlines for header sections.
    */
  val punctuationChar: Characters =
    anyOf('!','"','#','$','%','&','\'','(',')','[',']','{','}','*','+',',','-','.',':',';','/','<','>','=','?','@','\\','^','_','`','|','~')

  /** Parses a simple reference name that only allows alphanumerical characters
    *  and the punctuation characters `-`, `_`, `.`, `:`, `+`.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#reference-names]].
    */
  val simpleRefName = TextParsers.refName

  /** Parses any of the four supported types of footnote labels.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnote-references]].
    */
  val footnoteLabel: Parser[FootnoteLabel] = {
    val decimal = (anyIn('0' to '9') min 1) ^^ { n => NumericLabel(n.toInt) }
    val autonumber = '#' ^^^ Autonumber
    val autosymbol = '*' ^^^ Autosymbol
    val autonumberLabel = '#' ~> simpleRefName ^^ AutonumberLabel

    decimal | autonumberLabel | autonumber | autosymbol
  }

}
