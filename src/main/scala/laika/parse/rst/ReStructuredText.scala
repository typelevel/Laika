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
import scala.util.parsing.input.CharSequenceReader
import laika.parse.util.WhitespacePreprocessor
import laika.parse.rst.ext.StandardBlockDirectives
import laika.parse.rst.ext.StandardSpanDirectives
  
/** A parser for text written in reStructuredText markup. Instances of this class may be passed directly
 *  to the `Parse` or `Transform` APIs:
 *  
 *  {{{
 *  val document = Parse as ReStructuredText fromFile "hello.rst"
 *  
 *  Transform from ReStructuredText to HTML fromFile "hello.rst" toFile "hello.html"
 *  }}}
 * 
 *  reStructuredText has several types of extension points that are fully supported by Laika.
 *  In contrast to the original Python implementation, the API has been redesigned to be a more
 *  idiomatic, concise and type-safe Scala DSL.
 * 
 *  The following extension types are available:
 * 
 *  - Block Directives - an extension hook for adding new block level elements to
 *    reStructuredText markup. Use the `withBlockDirectives` method of this class to
 *    add directive implementations to the parser. Specification entry: 
 *    [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#directives]]
 * 
 *  - Substitution Definitions - an extension hook for adding new span level elements to
 *    reStructuredText markup that can be used by substitution references (like `|subst|`). 
 *    Use the `withSpanDirectives` method of this class to
 *    add directive implementations to the parser that can be used as substitution definitions. 
 *    Specification entry: 
 *    [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-definitions]]
 * 
 *  - Interpreted Text Roles - an extension hook for adding new dynamic span level elements to
 *    reStructuredText markup. In contrast to substitution definitions the implementation of a text
 *    role uses the text from the occurrences in the markup referring to the role as input.
 *    Use the `withTextRoles` method of this class to
 *    add custom text role implementations to the parser that can be referred to by interpreted text. 
 *    Specification entry: 
 *    [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#custom-interpreted-text-roles]]
 * 
 *  @author Jens Halm
 */
class ReStructuredText private (
    blockDirectives: List[Directive[Block]],
    spanDirectives: List[Directive[Span]],
    textRoles: List[TextRole]
    ) extends (Input => RawDocument) { self =>

  
  /** Adds the specified directives and returns a new instance of the parser.
   *  These block directives may then be used anywhere in documents parsed by this instance.
   * 
   *  Example:
   * 
   *  {{{
   *  case class Note (title: String, content: Seq[Block]) extends Block with BlockContainer[Note]
   *
   *  val rst = ReStructuredText withBlockDirectives (
   *    BlockDirective("note") {
   *      (argument() ~ blockContent)(Note)
   *    }
   *  )                                              
   *
   *  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"
   *  }}}   
   * 
   *  For more details on implementing directives see [[laika.parse.rst.Directives]].
   */
  def withBlockDirectives (directives: Directive[Block]*) = {
    new ReStructuredText(blockDirectives ++ directives, spanDirectives, textRoles)    
  }
     
  /** Adds the specified directives and returns a new instance of the parser.
   *  These span directives can then be referred to by substitution references.
   * 
   *  Example:
   * 
   *  {{{
   *  val rst = ReStructuredText withSpanDirectives (
   *    SpanDirective("replace") {
   *      spanContent map SpanSequence
   *    }
   *  ) 
   * 
   *  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"   
   *  }}}
   * 
   *  For more details on implementing directives see [[laika.parse.rst.Directives]].
   */ 
  def withSpanDirectives (directives: Directive[Span]*) = {
    new ReStructuredText(blockDirectives, spanDirectives ++ directives, textRoles)    
  }
  
  /** Adds the specified text roles and returns a new instance of the parser.
   *  These text roles may then be used in interpreted text spans.
   *  
   *  Example:
   * 
   *  {{{
   *  val rst = ReStructuredText withTextRoles (
   *    TextRole("link", "http://www.our-server.com/tickets/")(field("base-url")) {
   *      (base, text) => Link(List(Text(text)), base + text)
   *    }
   *  )
   *   
   *  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"   
   *  }}}
   * 
   *  For more details on implementing directives see [[laika.parse.rst.TextRoles]].
   */
  def withTextRoles (roles: TextRole*) = {
    new ReStructuredText(blockDirectives, spanDirectives, textRoles ++ roles)    
  }
      
  
  private lazy val parser = {
    new BlockParsers with InlineParsers {
      val std = new StandardBlockDirectives with StandardSpanDirectives {}
      
      val blockDirectives = (std.blockDirectives ++ self.blockDirectives) map { d => (d.name, d.part) } toMap
      val spanDirectives = (std.spanDirectives ++ self.spanDirectives) map { d => (d.name, d.part) } toMap
      val textRoles = self.textRoles map { r => (r.name, r.part) } toMap
      
      def blockDirective (name: String): Option[DirectivePart[Block]]  = blockDirectives.get(name).map(_(this))
      def spanDirective (name: String): Option[DirectivePart[Span]]     = spanDirectives.get(name).map(_(this))
      def textRole (name: String): Option[RoleDirectivePart[String => Span]] = textRoles.get(name).map(_(this))
    }
  }

  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  def apply (input: Input) = {
    val raw = input.asParserInput.source
    val preprocessed = (new WhitespacePreprocessor)(raw.toString)
    parser.parseDocument(new CharSequenceReader(preprocessed))
  }
  
}

/** The default reStructuredText parser configuration, without any directives or text roles installed.
 * 
 *  @author Jens Halm
 */
object ReStructuredText extends ReStructuredText(Nil,Nil,Nil)
