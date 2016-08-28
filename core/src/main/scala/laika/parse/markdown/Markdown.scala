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

package laika.parse.markdown

import laika.io.Input
import laika.parse.markdown.html.HTMLParsers
import laika.tree.Documents.Document
import laika.factory.ParserFactory
import laika.directive.StandardDirectives
import laika.directive.Directives.Blocks
import laika.directive.Directives.Spans
import laika.template.TemplateParsers
import laika.tree.Elements.Block
import laika.tree.Elements.Header
import laika.tree.Elements.Id
import laika.rewrite.TreeUtil
  
/** A parser for Markdown text. Instances of this class may be passed directly
 *  to the `Parse` or `Transform` APIs:
 *  
 *  {{{
 *  val document = Parse as Markdown fromFile "hello.md"
 *  
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 *  
 *  Since this library is not solely focused on producing HTML output,
 *  parsing verbatim HTML elements like defined by the official Markdown 
 *  syntax description is an optional feature, as some types of renderers 
 *  would not know what to do with HTML nodes in the document tree. 
 *  It must be enabled explicitly:
 *  
 *  {{{
 *  val document = Parse as (Markdown withVerbatimHTML) fromFile "hello.md"
 *  }}}
 *  
 *  The methods `withSpanDirectives` and `withBlockDirectives` allow to 
 *  register custom tags which will be processed by the parser in addition
 *  to standard Markdown markup. For more details on directives see
 *  [[laika.directive.Directives]].
 *  
 *  To switch this feature off alongside other custom extensions like
 *  configuration sections at the start of the document or automatic
 *  id generation for headers, you can run the parser in strict mode:
 *  
 *  {{{
 *  Transform from Markdown.strict to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 * 
 *  @author Jens Halm
 */
class Markdown private (
    blockDirectives: List[Blocks.Directive],
    spanDirectives: List[Spans.Directive],
    verbatimHTML: Boolean,
    isStrict: Boolean) extends ParserFactory {

  
  val fileSuffixes: Set[String] = Set("md","markdown")
  
  val rewriteRules = Seq()
  
  /** Adds the specified Laika directives and returns a new instance of the parser.
   * 
   *  Example:
   * 
   *  {{{
   *  case class Note (title: String, content: Seq[Block], options: Options = NoOpt) 
   *                                                       extends Block with BlockContainer[Note]
   *  
   *  val md = Markdown withBlockDirectives (
   *    Blocks.create("note") {
   *      (attribute(Default) ~ body(Default))(Note(_,_))
   *    }
   *  )   
   *  Transform from md to HTML fromFile "hello.md" toFile "hello.html"   
   *  }}}
   * 
   *  For more details on implementing Laika directives see [[laika.directive.Directives]].
   */     
  def withBlockDirectives (directives: Blocks.Directive*): Markdown =
    new Markdown(blockDirectives ++ directives, spanDirectives, verbatimHTML, isStrict)      
  
  /** Adds the specified Laika directives and returns a new instance of the parser.
   * 
   *  Example:
   * 
   *  {{{
   *  val md = Markdown withSpanDirectives (
   *    Spans.create("ticket") {
   *      (attribute(Default) ~ attribute("param").optional) { (ticketNo, param) =>
   *        val base = "http://tickets.service.com/"+ticketNo
   *        val url = base + (param map (p => "?param="+p) getOrElse "")
   *        ExternalLink(Seq(Text("Ticket "+ticketNo)), url, options = Styles("ticket"))
   *      }
   *    }
   *  )    
   * 
   *  Transform from md to HTML fromFile "hello.md" toFile "hello.html"   
   *  }}}
   *  
   *  The code above registers a span directive that detects markup like
   *  `@:ticket 2356.` and turns it into an external link node for the
   *  URL `http://tickets.service.com/2356`.
   * 
   *  For more details on implementing Laika directives see [[laika.directive.Directives]].
   */ 
  def withSpanDirectives (directives: Spans.Directive*): Markdown = 
    new Markdown(blockDirectives, spanDirectives ++ directives, verbatimHTML, isStrict)  
  
  /** Returns a Markdown parser that also parses verbatim HTML elements alongside
   *  the standard Markdown markup. Usually only recommended when used together
   *  with an HTML renderer, as such a parser returns a document tree that contains
   *  HTML elements which some parsers would not know how to handle.
   */
  def withVerbatimHTML: Markdown = new Markdown(blockDirectives, spanDirectives, true, isStrict)
  
  /** Turns strict mode on for the returned parser, switching off any
   *  features not part of the original Markdown syntax.
   *  This includes the registration of directives (custom tags) as well as configuration
   *  sections at the start of the document or id generation for all headers.
   */
  def strict: Markdown = new Markdown(blockDirectives, spanDirectives, verbatimHTML, true)
  
  private lazy val parser: BlockParsers with InlineParsers = {
    trait ExtendedParsers extends TemplateParsers.MarkupBlocks with TemplateParsers.MarkupSpans with StandardDirectives {
      lazy val blockDirectiveMap = Blocks.toMap(stdBlockDirectives) ++ Blocks.toMap(blockDirectives)
      lazy val spanDirectiveMap = Spans.toMap(stdSpanDirectives) ++ Spans.toMap(spanDirectives)
      def getBlockDirective (name: String) = blockDirectiveMap.get(name)
      def getSpanDirective (name: String) = spanDirectiveMap.get(name)
      
      override def blockList (parser: => Parser[Block]): Parser[List[Block]] = super.blockList(parser) ^^ {
        _ map { case h: Header => 
            h.copy(options = h.options + Id(TreeUtil.extractText(h.content).replaceAll("[\n ]+", " ").toLowerCase))
          case other => other
        }
      }
    }
    if (verbatimHTML && !isStrict) new BlockParsers with InlineParsers with ExtendedParsers with HTMLParsers
    else if (verbatimHTML)         new BlockParsers with InlineParsers with HTMLParsers
    else if (!isStrict)            new BlockParsers with InlineParsers with ExtendedParsers
    else                           new BlockParsers with InlineParsers
  }

  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  val newParser: Input => Document = (input: Input) => parser.parseDocument(input.asParserInput, input.path)
  
}

/** The default Markdown parser configuration, with verbatim HTML elements disabled.
 * 
 *  @author Jens Halm
 */
object Markdown extends Markdown(Nil,Nil,false,false)
