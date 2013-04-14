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

import laika.io.Input
import laika.tree.Elements._
import laika.parse.rst.Directives._
import laika.parse.rst.TextRoles._
  
/** A parser for text written in reStructuredText markup. Instances of this class may be passed directly
 *  to the `Parse` or `Transform` APIs:
 *  
 *  {{{
 *  val document = Parse as ReStructuredText fromFile "hello.rst"
 *  
 *  Transform from ReStructuredText to HTML fromFile "hello.rst" toFile "hello.html"
 *  }}}
 * 
 *  reStructuredText has several types of extension points that Laika supports.
 * 
 *  TODO - examples and pointers to rst spec
 *  
 *  
 *  @author Jens Halm
 */
class ReStructuredText private (
    blockDirectives: List[Directive[Block]],
    spanDirectives: List[Directive[Span]],
    textRoles: List[TextRole]
    ) extends (Input => Document) { self =>

  
  /** Adds the specified directives to the parser.
   *  These block directives may then be used anywhere in documents parsed by this instance.
   */
  def withBlockDirectives (directives: Directive[Block]*) = {
    new ReStructuredText(blockDirectives ++ directives, spanDirectives, textRoles)    
  }
     
  /** Adds the specified directives to the parser.
   *  These span directives can then be referred to by substitution references.
   */ 
  def withSpanDirectives (directives: Directive[Span]*) = {
    new ReStructuredText(blockDirectives, spanDirectives ++ directives, textRoles)    
  }
  
  /** Adds the specified text roles to the parser.
   *  These text roles may then be used in interpreted text spans.
   */
  def withTextRoles (roles: TextRole*) = {
    new ReStructuredText(blockDirectives, spanDirectives, textRoles ++ roles)    
  }
      
  
  private lazy val parser = {
    new BlockParsers with InlineParsers {
      val blockDirectives = self.blockDirectives map { d => (d.name, d.part) } toMap
      val spanDirectives = self.spanDirectives map { d => (d.name, d.part) } toMap
      val textRoles = self.textRoles map { r => (r.name, r) } toMap
    }
  }

  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  def apply (input: Input) = parser.parseDocument(input.asParserInput)
  
}

/** The default reStructuredText parser configuration, without any directives or text roles installed.
 * 
 *  @author Jens Halm
 */
object ReStructuredText extends ReStructuredText(Nil,Nil,Nil)