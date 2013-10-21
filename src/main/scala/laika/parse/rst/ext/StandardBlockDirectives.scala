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
  
package laika.parse.rst.ext

import laika.tree.Elements._
import laika.parse.rst.Directives._
import laika.parse.rst.Directives.Parts._
import laika.parse.rst.BlockParsers
import laika.parse.rst.InlineParsers

/** Defines all supported standard block directives of the reStructuredText reference parser.
 * 
 *  The following directives are fully supported:
 * 
 *  - `compound`
 *  - `container`
 *  - `admonition`
 *  - `attention`
 *  - `caution`
 *  - `danger`
 *  - `error`
 *  - `hint`
 *  - `important`
 *  - `note`
 *  - `tip`
 *  - `warning`
 *  - `topic`
 *  - `sidebar`
 *  - `rubric`
 *  - `epigraph`
 *  - `highlights`
 *  - `pull-quote`
 *  - `parsed-literal`
 *  - `table`
 *  
 *  The following directives are supported with some limitations:
 * 
 *  - `figure` and `image` do not support the various layout options (`width`, `height`, `scale`, `align`), as no other
 *    tree nodes in Laika carry concrete layout information. It is recommended to use styles instead.
 * 
 *  - `code` does currently not support syntax highlighting 
 *    (it allows to set the language so client-side highlighters can be integrated if required)
 * 
 *  - `raw` does not support the `file` or `url` options (multi-file transformations are planned for version 0.4).
 * 
 *  Support for the following directives is deferred to the 0.4 release as that will add support for template based
 *  site generation and the corresponding addition of features like logic for the generation of tables of content
 *  will make the implementation of these directives easier:
 * 
 *  - `contents`
 *  - `sectnum`
 *  - `header`
 *  - `footer`
 *  - `title`
 *  - `meta`
 *  - `include`
 * 
 *  Finally, for some directives there is currently no support planned:
 * 
 *  - `math` (would require LaTeX integration)
 *  - `csv-table`, `list-table` (would just require some work, contributions are welcome)
 *  - `target-notes`, `class` (these would require processing beyond the directive itself, therefore would require new API) 
 * 
 *  @author Jens Halm
 */
trait StandardBlockDirectives { this: StandardSpanDirectives =>

  
  /** The compound directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#compound-paragraph]] for details.
   */
  lazy val compound: DirectivePart[Block] = {
    (blockContent ~ stdOpt) { (content, opt) => 
      BlockSequence(content, opt + Styles("compound"))
    } 
  }
  
  /** The container directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#container]] for details.
   */
  lazy val container: DirectivePart[Block] = {
    (optArgument(withWS = true) ~ blockContent ~ nameOpt) { (styles, content, id) => 
      BlockSequence(content, Options(id, styles.map(_.split(" ")).toList.flatten))
    } 
  }
  
  /** The admonition directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#generic-admonition]] for details.
   */
  def genericAdmonition (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ blockContent ~ stdOpt) { (title, content, opt) => 
      TitledBlock(title, content, opt + Styles("admonition"))
    } 
  }

  /** The attention, caution, danger, error, hint, important, note, tip and warning directives,
   *  which are all identical apart from their title which can be specified with the style parameter. 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#specific-admonitions]] for details.
   */
  def admonition (style: String, title: String): DirectivePart[Block] = {
    (blockContent ~ stdOpt) { (content, opt) => 
      TitledBlock(List(Text(title)), content, opt + Styles(style))
    } 
  }
  
  /** The topic directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#topic]] for details.
   */
  def topic (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ blockContent ~ stdOpt) { (title, content, opt) => 
      TitledBlock(title, content, opt + Styles("topic"))
    } 
  }
  
  /** The sidebar directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#sidebar]] for details.
   */
  def sidebar (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ optField("subtitle", parse.standardSpans(p)) ~ 
        blockContent ~ stdOpt) { (title, subtitle, content, opt) =>
      val titleAndContent = subtitle.map(s => Paragraph(s, Styles("subtitle"))).toList ++ content
      TitledBlock(title, titleAndContent, opt + Styles("sidebar"))
    } 
  }
  
  /** The rubric directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#rubric]] for details.
   */
  def rubric (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ stdOpt) { (text, opt) => 
      Paragraph(text, opt + Styles("rubric"))
    } 
  }
  
  /** The header directive,
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#document-header-footer]] for details.
   */
  def header (p: BlockParsers) = blockContent map { blocks => DocumentFragment("header", BlockSequence(blocks)) }
  
  /** The footer directive,
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#document-header-footer]] for details.
   */
  def footer (p: BlockParsers) = blockContent map { blocks => DocumentFragment("footer", BlockSequence(blocks)) }
  
  /** The epitaph, highlights and pull-quote directives, which are all identical apart from the style
   *  parameter, see 
   *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#epigraph epigraph]],
   *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#highlights highlights]] and
   *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#pull-quote pull-quote]] for details.
   */
  def quotedBlock (p:BlockParsers, style: String): DirectivePart[Block] = content(parse.quotedBlock(p,style))
  
  /** The parsed-literal directive, see 
   *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#parsed-literal-block]] for details.
   */
  def parsedLiteral (p: InlineParsers): DirectivePart[Block] = {
    (content(parse.standardSpans(p)) ~ stdOpt) { (content, opt) => 
      ParsedLiteralBlock(content, opt)
    } 
  }
  
  /** The table directive, adding a title to standard reStructuredText tables, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#table]] for details.
   */
  def table (p: BlockParsers with InlineParsers): DirectivePart[Block] = {
    (optArgument(parse.standardSpans(p), withWS = true) ~ content(parse.table(p)) ~ stdOpt) { (caption, table, opt) => 
      table.copy(caption = Caption(caption.toList.flatten), options = opt)
    } 
  }
  
  /** The code directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#code]] for details.
   *  The current implementation does not support syntax highlighting.
   */
  lazy val code: DirectivePart[Block] = {
    (argument() ~ content(Right(_)) ~ stdOpt) { (language, content, opt) => 
      CodeBlock(language, List(Text(content)), opt)
    } 
  }
  
  /** The image directive for block elements, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#image]] for details.
   */
  lazy val imageBlock: DirectivePart[Block] = image map (img => Paragraph(List(img)))
  
  /** The figure directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#figure]] for details.
   */
  def figure (p: BlockParsers): DirectivePart[Block] = {
    (image ~ content(parse.captionAndLegend(p)) ~ optField("figclass")) { (image, captionAndLegend, figStyles) => 
      Figure(image, captionAndLegend._1, captionAndLegend._2, toOptions(None, figStyles))
    } 
  }
  
  /** The raw directive, which is not enabled by default, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#raw-data-pass-through]] for details.
   *  It can be enabled with `ReStructuredText.withRawContent`.
   */
  lazy val rawDirective: DirectivePart[Block] = {
    (argument(withWS = true) ~ content(Right(_))) { (formats, content) =>
      RawContent(formats.split(" "), content)
    } 
  }
    
  /** All standard block directives currently supported by Laika, except for
   *  the `raw` directive which needs to be enabled explicitly.
   */
  lazy val blockDirectives = List(
    BlockDirective("compound")(compound),
    BlockDirective("container")(container),
    BlockDirective.recursive("topic")(topic),
    BlockDirective.recursive("sidebar")(sidebar),
    BlockDirective.recursive("rubric")(rubric),
    BlockDirective.recursive("epigraph")(quotedBlock(_,"epigraph")),
    BlockDirective.recursive("highlights")(quotedBlock(_,"highlights")),
    BlockDirective.recursive("pull-quote")(quotedBlock(_,"pull-quote")),
    BlockDirective.recursive("parsed-literal")(parsedLiteral),
    BlockDirective.recursive("table")(table),
    BlockDirective.recursive("figure")(figure),
    BlockDirective.recursive("admonition")(genericAdmonition),
    BlockDirective("code")(code),
    BlockDirective("image")(imageBlock),
    BlockDirective("attention")(admonition("attention","Attention!")),
    BlockDirective("caution")(admonition("caution","Caution!")),
    BlockDirective("danger")(admonition("danger","!DANGER!")),
    BlockDirective("error")(admonition("error","Error")),
    BlockDirective("hint")(admonition("hint","Hint")),
    BlockDirective("important")(admonition("important","Important")),
    BlockDirective("note")(admonition("note","Note")),
    BlockDirective("tip")(admonition("tip","Tip")),
    BlockDirective("warning")(admonition("warning","Warning"))
  )
  
  
}