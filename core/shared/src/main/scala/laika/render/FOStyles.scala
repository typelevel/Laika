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

import laika.ast.{ParentSelector, Path, StyleDeclaration, StyleDeclarationSet, StylePredicate, StyleSelector}
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
    forChildElement(ElementType(parentElement), StyleName(styleName), attributes:_*)
  
  private def forChildElement (parentPredicate: StylePredicate, childPredicate: StylePredicate, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(
      Set(childPredicate), 
      Some(ParentSelector(StyleSelector(Set(parentPredicate)), immediate = false))
    ), 
    attributes.toMap)

  private def forElementAndStyle (element: String, styleName: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(Set(ElementType(element), StyleName(styleName)), None), attributes.toMap)

  private def forElementAndStyles (element: String, styleName1: String, styleName2: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(Set(ElementType(element), StyleName(styleName1), StyleName(styleName2)), None), attributes.toMap)
  
  private val bodyFont = fontFamily("serif")
  private val headerFont = fontFamily("sans-serif")
  private val codeFont = fontFamily("monospaced")

//  private val bodyFont = fontFamily("Lato")
//  private val headerFont = fontFamily("Lato")
//  private val codeFont = fontFamily("FiraCode")
  
  private val defaultFontSize = fontSize(10)
  private val codeFontSize = fontSize(9)
  private val superscriptFontSize = fontSize(8)
  
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
  private val justifiedText = "text-align" -> "justify"
  private val rightAlign = "text-align" -> "right"
  private val preserveWhitespace = Seq(
    "linefeed-treatment" -> "preserve", 
    "white-space-treatment" -> "preserve", 
    "white-space-collapse" -> "false"
  )

  private val primaryColor = "007c99"
  private val secondaryColor = "931813"
  private val defaultSpaceAfter = spaceAfter(3)
  private val largeSpaceAfter = spaceAfter(6)
  private val defaultLineHeight = lineHeight(1.5)
  private val codeLineHeight = lineHeight(1.4)
  
  private val syntaxBaseColors: Vector[String] = Vector("F6F1EF", "AF9E84", "937F61", "645133", "362E21")
  private val syntaxWheelColors: Vector[String] = Vector("9A6799", "9F4C46", "A0742D", "7D8D4C", "6498AE")

  private val blockStyles = Seq(
    forElement("Paragraph", bodyFont, justifiedText, defaultLineHeight, defaultFontSize, defaultSpaceAfter),
    forElement("TitledBlock", paddingLeft(20), paddingRight(20), largeSpaceAfter),
    forChildElement("TitledBlock", "title", headerFont, bold, fontSize(12), defaultSpaceAfter),
    forElement("QuotedBlock", italic, marginLeft(8), marginRight(8), defaultSpaceAfter),
    forChildElement("QuotedBlock", "attribution", bodyFont, rightAlign, defaultLineHeight, defaultFontSize),
    forElement("Image", largeSpaceAfter, "width" -> "100%", "height" -> "100%", "content-width" -> "scale-down-to-fit", "scaling" -> "uniform"),
    forElement("Figure", largeSpaceAfter),
    forChildElement("Figure", "caption", bodyFont, fontSize(9), italic, defaultSpaceAfter),
    forChildElement("Figure", "legend", fontSize(9), italic),
    forElement("Footnote", fontSize(8)),
    forElement("Citation", fontSize(8))
  )
  
  private val headerStyles = Seq(
    forElement("Header", headerFont, fontSize(11), bold, defaultSpaceAfter, spaceBefore(7)),
    forElement("Title", headerFont, fontSize(18), bold, largeSpaceAfter, spaceBefore(12)),
    forElementAndStyle("Header", "level1", fontSize(16), largeSpaceAfter, spaceBefore(12)),
    forElementAndStyle("Header", "level2", fontSize(14)),
    forElementAndStyle("Header", "level3", fontSize(12))
  )
  
  private val listStyles = Seq(
    forElement("BulletList", largeSpaceAfter, startDistance(5)),
    forElement("EnumList", largeSpaceAfter, startDistance(5)),
    forElement("DefinitionList", largeSpaceAfter, startDistance(20)),
    forElement("BulletListItem", defaultSpaceAfter),
    forElement("EnumListItem", defaultSpaceAfter),
    forElement("DefinitionListItem", defaultSpaceAfter)
  )

  private val codeProperties = Seq(codeFont, codeFontSize, codeLineHeight,
    bgColor(syntaxBaseColors(0)), color(syntaxBaseColors(4)),
    borderRadius(2), padding(2), marginLeft(2), marginRight(2), largeSpaceAfter) ++ preserveWhitespace
  
  private val codeStyles = Seq(
    forElement("LiteralBlock", codeProperties: _*),
    forElement("ParsedLiteralBlock", codeProperties: _*),
    forElement("CodeBlock", codeProperties: _*),
    forElement("Literal", codeFont, codeFontSize),
    forElement("InlineCode", codeFont, codeFontSize)
  )
  
  private val tableStyles = Seq(
    forElement("Table", largeSpaceAfter),
    forElement("TableHead", bold, "border-bottom-width" -> "1pt", "border-bottom-style" -> "solid"),
    forElement("Cell", paddingTop(2))
  )
  
  private val inlineStyles = Seq(
    forElement("Emphasized", italic),
    forElement("Strong", bold),
    forElement("Deleted", "text-decoration" -> "line-through"),
    forElement("Inserted", "text-decoration" -> "underline"),
    forStyleName("subscript", superscriptFontSize, "vertical-align" -> "sub"),
    forStyleName("superscript", superscriptFontSize, "vertical-align" -> "super"),
    forStyleName("footnote-label", superscriptFontSize, "vertical-align" -> "super")
  )
  
  private val linkStyles = Seq(
    forElement("FootnoteLink", color(secondaryColor)),
    forElement("CitationLink", color(secondaryColor)),
    forElement("SpanLink", color(secondaryColor), bold),
    forChildElement(ElementType("NavigationLink"), ElementType("SpanLink"), color(primaryColor), bold),
  )

  private def codeColor (value: String, categories: CodeCategory*): Seq[StyleDeclaration] =
    categories.map(c => forChildElement("CodeBlock", c.name, color(value)))
  
  private val syntaxHighlighting = 
    codeColor(syntaxBaseColors(1), CodeCategory.Comment, CodeCategory.XML.CData, CodeCategory.Markup.Quote) ++
    codeColor(syntaxBaseColors(2), CodeCategory.Tag.Punctuation) ++
    codeColor(syntaxBaseColors(3), CodeCategory.Identifier) ++
    codeColor(syntaxWheelColors(0), CodeCategory.Substitution, CodeCategory.Annotation, CodeCategory.Markup.Emphasized, CodeCategory.XML.ProcessingInstruction) ++
    codeColor(syntaxWheelColors(1), CodeCategory.Keyword, CodeCategory.EscapeSequence, CodeCategory.Markup.Headline) ++
    codeColor(syntaxWheelColors(2), CodeCategory.AttributeName, CodeCategory.DeclarationName, CodeCategory.Markup.LinkTarget) ++
    codeColor(syntaxWheelColors(3), CodeCategory.NumberLiteral, CodeCategory.StringLiteral, CodeCategory.LiteralValue, CodeCategory.BooleanLiteral, CodeCategory.CharLiteral, CodeCategory.SymbolLiteral, CodeCategory.RegexLiteral, CodeCategory.Markup.LinkText) ++
    codeColor(syntaxWheelColors(4), CodeCategory.TypeName, CodeCategory.Tag.Name, CodeCategory.XML.DTDTagName, CodeCategory.Markup.Fence)
  
  private val navStyles = Seq(
    forElementAndStyle("Paragraph", "nav", marginLeft(8), fontSize(10), spaceAfter(0), spaceBefore(2), "text-align-last" -> "justify"),
    forElementAndStyles("Paragraph", "nav", "level1", fontSize(16), marginLeft(0), spaceBefore(9), "text-transform" -> "uppercase"),
    forElementAndStyles("Paragraph", "nav", "level2", fontSize(14), marginLeft(4), spaceBefore(7)),
    forElementAndStyles("Paragraph", "nav", "level3", fontSize(12), marginLeft(6), spaceBefore(4)),
    forElementAndStyles("Paragraph", "nav", "level4", marginLeft(8))
  )
  
  private val alignStyles = Seq(
    forStyleName("align-top", "vertical-align" -> "top"),
    forStyleName("align-bottom", "vertical-align" -> "bottom"),
    forStyleName("align-middle", "vertical-align" -> "middle"),
    forStyleName("align-left", "text-align" -> "left"),
    forStyleName("align-right", "text-align" -> "right"),
    forStyleName("align-center", "text-align" -> "center")
  )

  private val keepStyles = Seq(
    forStyleName("keepWithPrevious", "keep-with-previous" -> "always"),
    forStyleName("keepWithNext", "keep-with-next" -> "always")
  )
  
  private val specialStyles = Seq(
    forElement("PageBreak", "page-break-before" -> "always"),
    forElement("Rule", defaultSpaceAfter, "leader-length" -> "100%", "rule-style" -> "solid", "rule-thickness" -> "2pt"),
    forElement("RuntimeMessage", "color" -> "white"),
    forElementAndStyle("RuntimeMessage", "debug", bgColor("888888")),
    forElementAndStyle("RuntimeMessage", "info", bgColor("888888")),
    forElementAndStyle("RuntimeMessage", "warning", bgColor("ffff33")),
    forElementAndStyle("RuntimeMessage", "error", bgColor("ff0000")),
    forElementAndStyle("RuntimeMessage", "fatal", bgColor("ff0000"))
  )
  
  private val allStyles =
    blockStyles ++
    headerStyles ++
    listStyles ++
    codeStyles ++
    tableStyles ++
    linkStyles ++
    inlineStyles ++
    syntaxHighlighting ++
    navStyles ++
    alignStyles ++
    keepStyles ++
    specialStyles
  
  /** The default styles for PDF and XSL-FO renderers.
    *
    * They can be overridden by placing a custom CSS document
    * with the name `default.fo.css` into the root directory
    * of the input files.
    */
  val default: StyleDeclarationSet = {
    val orderApplied = allStyles.zipWithIndex.map { case (style, index) =>
      style.copy(selector = style.selector.copy(order = index))
    }
    StyleDeclarationSet(Set.empty[Path], orderApplied.toSet, Precedence.Low)
  }
  
}
