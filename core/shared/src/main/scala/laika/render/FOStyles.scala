/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.render

import laika.ast.{ParentSelector, Path, StyleDeclaration, StyleDeclarationSet, StyleSelector}
import laika.ast.StylePredicate.{ElementType, StyleName}
import laika.bundle.Precedence
import laika.parse.code.CodeCategory

/** The default styles for PDF and XSL-FO output.
  * 
  * @author Jens Halm
  */
object FOStyles {

  private def forElement (elementName: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(ElementType(elementName), attributes: _*)

  private def forStyleName (name: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleName(name), attributes: _*)

  private def forChildElement (parentElement: String, styleName: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(
      Set(StyleName(styleName)), 
      Some(ParentSelector(StyleSelector(Set(ElementType(parentElement))), immediate = false))
    ), 
    attributes.toMap)

  private def forElementAndStyle (element: String, styleName: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(Set(ElementType(element), StyleName(styleName)), None), attributes.toMap)

  private def forElementAndStyles (element: String, styleName1: String, styleName2: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(Set(ElementType(element), StyleName(styleName1), StyleName(styleName2)), None), attributes.toMap)
  
  private val bodyFont = fontFamily("Lato")
  private val headerFont = fontFamily("Lato")
  private val codeFont = fontFamily("FiraCode")
  
  private def fontFamily (name: String) = "font-family" -> name
  private def fontSize (value: Int) = "font-size" -> s"${value}pt"
  private def lineHeight (value: Double) = "line-height" -> value.toString
  private def spaceBefore (value: Int) = "space-before" -> s"${value}mm"
  private def spaceAfter (value: Int) = "space-after" -> s"${value}mm"
  private def bgColor (value: String) = "background-color" -> s"#$value"
  private def color (value: String) = "color" -> s"#$value"
  private def padding (value: Int) = "padding" -> s"${value}mm"
  private def paddingTop (value: Int) = "padding-top" -> s"${value}mm"
  private def paddingLeft (value: Int) = "padding-left" -> s"${value}mm"
  private def paddingRight (value: Int) = "padding-right" -> s"${value}mm"
  private def marginLeft (value: Int) = "margin-left" -> s"${value}mm"
  private def marginRight (value: Int) = "margin-right" -> s"${value}mm"
  private def borderRadius (value: Int) = "fox:border-radius" -> s"${value}mm"
  private def startDistance (value: Int) = "provisional-distance-between-starts" -> s"${value}mm"
  private val bold = "font-weight" -> "bold"
  private val italic = "font-style" -> "italic"
  private val rightAlign = "text-align" -> "right"
  private val preserveWhitespace = Seq(
    "linefeed-treatment" -> "preserve", 
    "white-space-treatment" -> "preserve", 
    "white-space-collapse" -> "false"
  )

  private val secondaryColor = "931813"
  
  private def codeColor (value: String, categories: CodeCategory*): Seq[StyleDeclaration] = 
    categories.map(c => forChildElement("CodeBlock", c.name, color(value)))

  private val syntaxBaseColors: Vector[String] = Vector("fae3be", "d8ac66", "9f7e4a", "57462c", "362e21")
  private val syntaxWheelColors: Vector[String] = Vector("b38dad", "9f4c46", "996c4c", "a5b575", "87aec0")
  private val codeStyles = Seq(codeFont, fontSize(9), lineHeight(1.4), 
    bgColor(syntaxBaseColors(0)), color(syntaxBaseColors(4)), 
    borderRadius(2), padding(2), marginLeft(6), marginRight(6), spaceAfter(6)) ++ preserveWhitespace

  private val syntaxHighlighting = 
    codeColor(syntaxBaseColors(1), CodeCategory.Comment, CodeCategory.XML.CData, CodeCategory.Markup.Quote) ++
    codeColor(syntaxBaseColors(2), CodeCategory.Tag.Punctuation) ++
    codeColor(syntaxBaseColors(3), CodeCategory.Identifier) ++
    codeColor(syntaxWheelColors(0), CodeCategory.Substitution, CodeCategory.Annotation, CodeCategory.Markup.Emphasized, CodeCategory.XML.ProcessingInstruction) ++
    codeColor(syntaxWheelColors(1), CodeCategory.Keyword, CodeCategory.EscapeSequence, CodeCategory.Markup.Headline) ++
    codeColor(syntaxWheelColors(2), CodeCategory.AttributeName, CodeCategory.DeclarationName, CodeCategory.Markup.LinkTarget) ++
    codeColor(syntaxWheelColors(3), CodeCategory.NumberLiteral, CodeCategory.StringLiteral, CodeCategory.LiteralValue, CodeCategory.BooleanLiteral, CodeCategory.CharLiteral, CodeCategory.SymbolLiteral, CodeCategory.RegexLiteral, CodeCategory.Markup.LinkText) ++
    codeColor(syntaxWheelColors(4), CodeCategory.TypeName, CodeCategory.Tag.Name, CodeCategory.XML.DTDTagName, CodeCategory.Markup.Fence)
  
  
  private val styles = Seq(
    forElement("Paragraph", bodyFont, lineHeight(1.5), fontSize(10), spaceAfter(3)),
    forElement("TitledBlock", bgColor("cccccc"), paddingLeft(20), paddingRight(20), spaceAfter(6)),
    forChildElement("TitledBlock", "title", headerFont, bold, fontSize(12)),
    forElement("QuotedBlock", italic, marginLeft(8), marginRight(8), spaceAfter(3)),
    forChildElement("QuotedBlock", "attribution", rightAlign),
    forElement("LiteralBlock", codeStyles: _*),
    forElement("ParsedLiteralBlock", codeStyles: _*),
    forElement("CodeBlock", codeStyles: _*),
    forElement("Image", spaceAfter(6), "width" -> "100%", "height" -> "100%", "content-width" -> "scale-down-to-fit", "scaling" -> "uniform"),
    forElement("Figure", spaceAfter(6)),
    forElement("Figure", spaceAfter(6)),
    forChildElement("Figure", "caption", fontSize(9), italic),
    forChildElement("Figure", "legend", fontSize(9), italic),
    forElement("Header", headerFont, fontSize(11), bold, spaceAfter(3), spaceBefore(7)),
    forElement("Title", headerFont, fontSize(18), spaceAfter(6), spaceBefore(12)),
    forElementAndStyle("Header", "level1", fontSize(16), spaceAfter(6), spaceBefore(12)),
    forElementAndStyle("Header", "level2", fontSize(14)),
    forElementAndStyle("Header", "level3", fontSize(12)),
    forElement("BulletList", spaceAfter(6), startDistance(5)),
    forElement("EnumList", spaceAfter(6), startDistance(5)),
    forElement("DefinitionList", spaceAfter(6), startDistance(20)),
    forElement("BulletListItem", spaceAfter(3)),
    forElement("EnumListItem", spaceAfter(3)),
    forElement("DefinitionListItem", spaceAfter(3)),
    forElement("Table", spaceAfter(6)),
    forElement("TableHead", bold, "border-bottom-width" -> "1pt", "border-bottom-style" -> "solid"),
    forElement("Cell", paddingTop(2)),
    forElement("PageBreak", "page-break-before" -> "always"),
    forElement("Footnote", fontSize(8)),
    forElement("Citation", fontSize(8)),
    forElement("FootnoteLink", color("3399FF")),
    forElement("CitationLink", color("3399FF")),
    forElement("SpanLink", color(secondaryColor), bold),
    forElement("Emphasized", italic),
    forElement("Strong", bold),
    forStyleName("title", bold),
    forElement("Deleted", "text-decoration" -> "line-through"),
    forElement("Inserted", "text-decoration" -> "underline"),
    forElement("Literal", codeFont, fontSize(9)),
    forElement("InlineCode", codeFont, fontSize(9)),
    forStyleName("subscript", fontSize(8), "vertical-align" -> "sub"),
    forStyleName("superscript", fontSize(8), "vertical-align" -> "super"),
    forStyleName("footnote-label", fontSize(8), "vertical-align" -> "super"),
    forElement("RuntimeMessage", "color" -> "white"),
    forElementAndStyle("RuntimeMessage", "debug", bgColor("888888")),
    forElementAndStyle("RuntimeMessage", "info", bgColor("888888")),
    forElementAndStyle("RuntimeMessage", "warning", bgColor("ffff33")),
    forElementAndStyle("RuntimeMessage", "error", bgColor("ff0000")),
    forElementAndStyle("RuntimeMessage", "fatal", bgColor("ff0000")),
    forElement("LineBlock", marginLeft(20)),
    forElementAndStyle("Paragraph", "toc", fontSize(11), spaceAfter(0), spaceBefore(2), "text-align-last" -> "justify"),
    forElementAndStyle("Paragraph", "nav", fontSize(11), spaceAfter(0), spaceBefore(2), "text-align-last" -> "justify"),
    forElementAndStyles("Paragraph", "toc", "level1", fontSize(12), spaceBefore(5)),
    forElementAndStyles("Paragraph", "toc", "level2", marginLeft(4)),
    forElementAndStyles("Paragraph", "toc", "level3", fontSize(10), marginLeft(6)),
    forElementAndStyles("Paragraph", "nav", "level1", fontSize(12), spaceBefore(5)),
    forElementAndStyles("Paragraph", "nav", "level2", marginLeft(4)),
    forElementAndStyles("Paragraph", "nav", "level3", fontSize(10), marginLeft(6)),
    forStyleName("align-top", "vertical-align" -> "top"),
    forStyleName("align-bottom", "vertical-align" -> "bottom"),
    forStyleName("align-middle", "vertical-align" -> "middle"),
    forStyleName("align-left", "text-align" -> "left"),
    forStyleName("align-right", "text-align" -> "right"),
    forStyleName("align-center", "text-align" -> "center"),
  ) ++ syntaxHighlighting
  
  /** The default styles for PDF and XSL-FO renderers.
    *
    * They can be overridden by placing a custom CSS document
    * with the name `default.fo.css` into the root directory
    * of the input files.
    */
  val default: StyleDeclarationSet = {
    val orderApplied = styles.zipWithIndex.map { case (style, index) =>
      style.copy(selector = style.selector.copy(order = index))
    }
    StyleDeclarationSet(Set.empty[Path], orderApplied.toSet, Precedence.Low)
  }
  
}
