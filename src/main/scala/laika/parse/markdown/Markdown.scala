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

package laika.parse.markdown

import laika.io.Input
import laika.parse.markdown.html.HTMLParsers
import laika.tree.Documents.Document
import laika.factory.ParserFactory
import laika.directive.Directives.Blocks
import laika.directive.Directives.Spans
import laika.template.TemplateParsers
  
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
 *  @author Jens Halm
 */
class Markdown private (
    blockDirectives: List[Blocks.Directive],
    spanDirectives: List[Spans.Directive],
    verbatimHTML: Boolean,
    isStrict: Boolean) extends ParserFactory {

  
  val fileSuffixes = Set("md","markdown")
  
      
  def withBlockDirectives (directives: Blocks.Directive*) =
    new Markdown(blockDirectives ++ directives, spanDirectives, verbatimHTML, isStrict)      
  
  def withSpanDirectives (directives: Spans.Directive*) = 
    new Markdown(blockDirectives, spanDirectives ++ directives, verbatimHTML, isStrict)  
  
  /** Returns a Markdown parser that also parses verbatim HTML elements alongside
   *  the standard Markdown markup. Usually only recommended when used together
   *  with an HTML renderer, as such a parser returns a document tree that contains
   *  HTML elements which some parsers would not know how to handle.
   */
  def withVerbatimHTML = new Markdown(blockDirectives, spanDirectives, true, isStrict)
  
  def strict = new Markdown(blockDirectives, spanDirectives, verbatimHTML, true)
  
  private lazy val parser = {
    trait ExtendedParsers extends TemplateParsers.MarkupBlocks with TemplateParsers.MarkupSpans {
      lazy val blockDirectiveMap = blockDirectives map { d => (d.name, d) } toMap
      lazy val spanDirectiveMap = spanDirectives  map { d => (d.name, d) } toMap
      def getBlockDirective (name: String) = blockDirectiveMap.get(name)
      def getSpanDirective (name: String) = spanDirectiveMap.get(name)
    }
    if (verbatimHTML && !isStrict) new BlockParsers with InlineParsers with ExtendedParsers with HTMLParsers
    else if (verbatimHTML)         new BlockParsers with InlineParsers with HTMLParsers
    else if (!isStrict)            new BlockParsers with InlineParsers with ExtendedParsers
    else                           new BlockParsers with InlineParsers
  }

  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  val newParser = (input: Input) => parser.parseDocument(input.asParserInput, input.path)
  
}

/** The default Markdown parser configuration, with verbatim HTML elements disabled.
 * 
 *  @author Jens Halm
 */
object Markdown extends Markdown(Nil,Nil,false,false)