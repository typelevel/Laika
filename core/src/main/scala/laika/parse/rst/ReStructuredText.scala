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

package laika.parse.rst

import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders, Theme}
import laika.factory.{ParserFactory, RendererFactory}
import laika.io.Input
import laika.parse.core.ParserContext
import laika.parse.rst.Directives._
import laika.parse.rst.TextRoles._
import laika.parse.rst.ext._
import laika.parse.util.WhitespacePreprocessor
import laika.render.{HTML, HTMLWriter}
import laika.rewrite.DocumentCursor
import laika.tree.Documents.Document
import laika.tree.Elements._
  
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
 *  In addition to the standard reStructuredText directives, the API also supports a custom directive
 *  type unique to Laika. They represent a library-wide extension mechanism and allow you to implement
 *  tags which can be used in any of the supported markup formats or in templates. If you need this
 *  level of flexibility, it is recommended to use the Laika directives, if you want to stay compatible
 *  with the reStructuredText reference parser, you should pick the standard directives.
 *  
 *  Laika directives can be registered with the `DirectiveSupport` extension bundle.
 *  The DSLs for creating directives are similar, but still different,
 *  due to differences in the feature set of the two variants. The Laika directives try to avoid some
 *  of the unnecessary complexities of reStructuredText directives.
 * 
 *  @author Jens Halm
 */
class ReStructuredText private (
       blockDirectives: List[Directive[Block]],
       spanDirectives: List[Directive[Span]],
       textRoles: List[TextRole],
       defaultTextRole: String = "title-reference",
       rawContent: Boolean = false
    ) extends ParserFactory { self =>


  val fileSuffixes: Set[String] = Set("rest","rst")

  val extensions = Seq(new ExtensionBundle {
    override val useInStrictMode: Boolean = true
    override def rewriteRules: Seq[DocumentCursor => RewriteRule] = Seq(RewriteRules)

    override def themeFor[Writer](rendererFactory: RendererFactory[Writer]): Theme[Writer] = rendererFactory match {
      case _: HTML => Theme[HTMLWriter](customRenderers = Seq(ExtendedHTML))
      case _ => Theme[Writer]() // TODO - refactor to return Option instead
    }
  })

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
  def withBlockDirectives (directives: Directive[Block]*): ReStructuredText =
    new ReStructuredText(blockDirectives ++ directives, spanDirectives, textRoles, defaultTextRole, rawContent)

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
  def withSpanDirectives (directives: Directive[Span]*): ReStructuredText =
    new ReStructuredText(blockDirectives, spanDirectives ++ directives, textRoles, defaultTextRole, rawContent)

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
  def withTextRoles (roles: TextRole*): ReStructuredText =
    new ReStructuredText(blockDirectives, spanDirectives, textRoles ++ roles, defaultTextRole, rawContent)

  /** Specifies the name of the default text role
   *  to apply when interpreted text
   *  is used in markup without an explicit role name.
   */
  def withDefaultTextRole (role: String): ReStructuredText =
    new ReStructuredText(blockDirectives, spanDirectives, textRoles, role, rawContent)

  /** Adds the `raw` directive and text roles to the parser.
   *  These are disabled by default as they present a potential security risk.
   */
  def withRawContent: ReStructuredText = {
    new ReStructuredText(blockDirectives, spanDirectives, textRoles, defaultTextRole, true)
  }

  private def createParser (parserExtensions: ParserDefinitionBuilders): RootParser = {

    val stdBlocks = new StandardBlockDirectives
    val stdSpans = new StandardSpanDirectives
    val stdTextRoles = new StandardTextRoles

    val rawDirective = if (rawContent) List(BlockDirective("raw")(stdBlocks.rawDirective)) else Nil
    val rawTextRole = if (rawContent) List(stdTextRoles.rawTextRole) else Nil

    val rstBlockDirectives = stdBlocks.blockDirectives ++ blockDirectives ++ rawDirective
    val rstSpanDirectives  = stdSpans.spanDirectives   ++ spanDirectives
    val rstTextRoles       = stdTextRoles.allRoles     ++ textRoles       ++ rawTextRole

    new RootParser(parserExtensions, rstBlockDirectives, rstSpanDirectives, rstTextRoles, defaultTextRole)
  }

  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  def newParser (parserExtensions: ParserDefinitionBuilders): Input => Document = input => {
    val raw = input.asParserInput.input
    val preprocessed = (new WhitespacePreprocessor)(raw.toString)
    createParser(parserExtensions).parseDocument(ParserContext(preprocessed), input.path)
  }
  
}

/** The default reStructuredText parser configuration, without any directives or text roles installed.
 * 
 *  @author Jens Halm
 */
object ReStructuredText extends ReStructuredText(Nil,Nil,Nil,"title-reference",false)
