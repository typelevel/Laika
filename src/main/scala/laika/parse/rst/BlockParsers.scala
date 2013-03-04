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

import laika.tree.Elements._
import laika.parse.InlineParsers

/**
 * @author Jens Halm
 */
trait BlockParsers extends BlockBaseParsers 
                      with ListParsers 
                      with TableParsers 
                      with ExplicitBlockParsers { self: InlineParsers => // TODO - probably needs to be rst.InlineParsers

  
  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special reStructuredText block type will be parsed as a regular paragraph.
   */
  def paragraph: Parser[Paragraph] = 
      ((not(blankLine) ~> restOfLine) +) ^^ { lines => Paragraph(parseInline(lines mkString "\n")) }

  
  def nonRecursiveBlock: Parser[Block] = comment | paragraph
  
  def topLevelBlock: Parser[Block] = unorderedList | 
                                     orderedList | 
                                     definitionList | 
                                     fieldList | 
                                     optionList |
                                     explicitBlockItem |
                                     gridTable |
                                     simpleTable |
                                     paragraph
 
  def nestedBlock: Parser[Block] = topLevelBlock
  
  
}