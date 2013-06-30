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

  
  lazy val compound: DirectivePart[Block] = {
    (blockContent ~ nameOpt ~ classOpt) { (content, id, styles) => 
      BlockSequence(content, toOptions(id,styles) + Styles("compound"))
    } 
  }
  
  lazy val container: DirectivePart[Block] = {
    (optArgument(withWS = true) ~ blockContent ~ nameOpt) { (styles, content, id) => 
      BlockSequence(content, Options(id, styles.map(_.split(" ")).toList.flatten))
    } 
  }
  
  def genericAdmonition (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ blockContent ~ nameOpt ~ classOpt) { (title, content, id, styles) => 
      TitledBlock(title, content, toOptions(id,styles) + Styles("admonition"))
    } 
  }

  def admonition (style: String): DirectivePart[Block] = {
    (blockContent ~ nameOpt ~ classOpt) { (content, id, styles) => 
      TitledBlock(List(Text(style)), content, toOptions(id,styles) + Styles(style))
    } 
  }
  
  def topic (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ blockContent ~ nameOpt ~ classOpt) { (title, content, id, styles) => 
      TitledBlock(title, content, toOptions(id,styles) + Styles("topic"))
    } 
  }
  
  def sidebar (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ optField("subtitle", parse.standardSpans(p)) ~ 
        blockContent ~ nameOpt ~ classOpt) { (title, subtitle, content, id, styles) =>
      val titleAndContent = subtitle.map(s => Paragraph(s, Styles("subtitle"))).toList ++ content
      TitledBlock(title, titleAndContent, toOptions(id,styles) + Styles("sidebar"))
    } 
  }
  
  def rubric (p: InlineParsers): DirectivePart[Block] = {
    (argument(parse.standardSpans(p), withWS = true) ~ nameOpt ~ classOpt) { (text, id, styles) => 
      Paragraph(text, toOptions(id,styles) + Styles("rubric"))
    } 
  }
  
  def quotedBlock (p:BlockParsers, style: String): DirectivePart[Block] = content(parse.quotedBlock(p,style))
  
  def parsedLiteral (p: InlineParsers): DirectivePart[Block] = {
    (content(parse.standardSpans(p)) ~ nameOpt ~ classOpt) { (content, id, styles) => 
      ParsedLiteralBlock(content, toOptions(id,styles))
    } 
  }
  
  def table (p: BlockParsers with InlineParsers): DirectivePart[Block] = {
    (optArgument(parse.standardSpans(p), withWS = true) ~ content(parse.table(p)) ~ nameOpt ~ classOpt) { (caption, table, id, styles) => 
      table.copy(caption = Caption(caption.toList.flatten), options = toOptions(id,styles))
    } 
  }
  
  lazy val code: DirectivePart[Block] = {
    (argument() ~ content(Right(_)) ~ nameOpt ~ classOpt) { (language, content, id, styles) => 
      CodeBlock(language, List(Text(content)), toOptions(id,styles))
    } 
  }
  
  lazy val imageBlock: DirectivePart[Block] = image map (img => Paragraph(List(img)))
  
  def figure (p: BlockParsers): DirectivePart[Block] = {
    (image ~ content(parse.captionAndLegend(p)) ~ optField("figclass")) { (image, captionAndLegend, figStyles) => 
      Figure(image, captionAndLegend._1, captionAndLegend._2, toOptions(None, figStyles))
    } 
  }
  
  lazy val rawDirective: DirectivePart[Block] = {
    (argument(withWS = true) ~ content(Right(_))) { (formats, content) =>
      RawContent(formats.split(" "), content)
    } 
  }
    
  /** All standard block directives currently supported by Laika.
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
    BlockDirective("attention")(admonition("attention")),
    BlockDirective("caution")(admonition("caution")),
    BlockDirective("danger")(admonition("danger")),
    BlockDirective("error")(admonition("error")),
    BlockDirective("hint")(admonition("hint")),
    BlockDirective("important")(admonition("important")),
    BlockDirective("note")(admonition("note")),
    BlockDirective("tip")(admonition("tip")),
    BlockDirective("warning")(admonition("warning"))
  )
  
  
}