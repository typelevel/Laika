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

package laika.helium

import laika.ast.StylePredicate.{ElementType, StyleName}
import laika.ast._
import LengthUnit._
import laika.bundle.Precedence
import laika.parse.code.CodeCategory

/** The default styles for PDF and XSL-FO output.
  * 
  * @author Jens Halm
  */
class FOStyles (helium: Helium) {

  private def forElement (elementName: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(ElementType(elementName), attributes: _*)

  private def forStyleName (name: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleName(name), attributes: _*)

  private def forStyleNames (name1: String, name2: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(Set[StylePredicate](StyleName(name1), StyleName(name2))), attributes.toMap)

  private def forChildElement (parentElement: String, styleName: String, attributes: (String, String)*): StyleDeclaration =
    forChildElement(ElementType(parentElement), StyleName(styleName), attributes:_*)
  
  private def forChildElement (parentPredicate: StylePredicate, childPredicate: StylePredicate, attributes: (String, String)*): StyleDeclaration =
    forChildElement(Set(parentPredicate), Set(childPredicate), attributes:_*)

  private def forChildElement (parentPredicates: Set[StylePredicate], childPredicates: Set[StylePredicate], attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(
      childPredicates,
      Some(ParentSelector(StyleSelector(parentPredicates), immediate = false))
    ),
      attributes.toMap)

  private def forElementAndStyle (element: String, styleName: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(Set(ElementType(element), StyleName(styleName)), None), attributes.toMap)

  private def forElementAndStyles (element: String, styleName1: String, styleName2: String, attributes: (String, String)*): StyleDeclaration =
    StyleDeclaration(StyleSelector(Set(ElementType(element), StyleName(styleName1), StyleName(styleName2)), None), attributes.toMap)
  
  private val bodyFont = fontFamily(helium.themeFonts.body)
  private val headerFont = fontFamily(helium.themeFonts.headlines)
  private val codeFont = fontFamily(helium.themeFonts.code)
  
  private def fontFamily (name: String) = "font-family" -> name
  private def fontSize (value: Size) = "font-size" -> value.displayValue
  private def lineHeight (value: Double) = "line-height" -> value.toString
  private val defaultFontSize = fontSize(helium.fontSizes.body)
  private val codeFontSize = fontSize(helium.fontSizes.code)
  private val smallFontSize = fontSize(helium.fontSizes.small)
  private val bold = "font-weight" -> "bold"
  private val italic = "font-style" -> "italic"
  private val justifiedText = "text-align" -> "justify"
  private val rightAlign = "text-align" -> "right"
  
  private def bgColor (value: Color) = "background-color" -> value.displayValue
  private def color (value: Color) = "color" -> value.displayValue
  
  private def spaceBefore (value: Int) = "space-before" -> s"${value}mm"
  private def spaceAfter (value: Size) = "space-after" -> value.displayValue
  private def spaceAfter (value: Int) = "space-after" -> s"${value}mm"
  private def padding (value: Int) = "padding" -> s"${value}mm"
  private def paddingHack (value: Int) = "padding" -> s"${value}mm ${value}mm 0.1mm ${value}mm" // preventing space-after collapsing, giving us the bottom padding
  private def paddingLeft (value: Int) = "padding-left" -> s"${value}mm"
  private def paddingRight (value: Int) = "padding-right" -> s"${value}mm"
  private def marginLeft (value: Int) = "margin-left" -> s"${value}mm"
  private def marginRight (value: Int) = "margin-right" -> s"${value}mm"
  
  private def border (points: Int, color: Color) = "border" -> s"${points}pt solid ${color.displayValue}"
  private def borderRadius (value: Int) = "fox:border-radius" -> s"${value}mm"
  
  private val preserveWhitespace = Seq(
    "linefeed-treatment" -> "preserve", 
    "white-space-treatment" -> "preserve", 
    "white-space-collapse" -> "false"
  )

  private val defaultSpaceAfter = spaceAfter(helium.PDFLayout.defaultBlockSpacing)
  private val largeSpaceAfter = spaceAfter(helium.PDFLayout.defaultBlockSpacing.scale(200)) // TODO - 0.16 - review need for this
  private val defaultLineHeight = lineHeight(helium.PDFLayout.defaultLineHeight)
  private val codeLineHeight = lineHeight(1.4)
  
  private val blockStyles = Seq(
    forElement("Paragraph", bodyFont, justifiedText, defaultLineHeight, defaultFontSize, defaultSpaceAfter/*, border(1, "999999")*/),
    forElement("TitledBlock", paddingLeft(20), paddingRight(20), largeSpaceAfter),
    forChildElement("TitledBlock", "title", headerFont, bold, fontSize(helium.fontSizes.header4), defaultSpaceAfter),
    forElement("QuotedBlock", italic, marginLeft(8), marginRight(8), defaultSpaceAfter/*, bgColor("cccccc"), paddingHack(3)*/),
    forChildElement("QuotedBlock", "attribution", bodyFont, rightAlign, defaultLineHeight, defaultFontSize),
    forElement("Image", largeSpaceAfter, "width" -> "85%", "content-width" -> "scale-down-to-fit", "scaling" -> "uniform"),
    forElement("Figure", largeSpaceAfter),
    forChildElement("Figure", "caption", bodyFont, codeFontSize, italic, defaultSpaceAfter),
    forChildElement("Figure", "legend", codeFontSize, italic),
    forElement("Footnote", smallFontSize),
    forElement("Citation", smallFontSize),
    forStyleName("default-space", defaultSpaceAfter)
  )
  
  private val headerStyles = Seq(
    forElement("Header", headerFont, fontSize(helium.fontSizes.header4), bold, defaultSpaceAfter, spaceBefore(7)),
    forElement("Title", headerFont, fontSize(helium.fontSizes.title), bold, color(helium.colors.primary), largeSpaceAfter, spaceBefore(0)),
    forElementAndStyle("Header", "level1", fontSize(helium.fontSizes.header2), largeSpaceAfter, spaceBefore(12)), // TODO - remaining h1 should be demoted to h2
    forElementAndStyle("Header", "level2", fontSize(helium.fontSizes.header2)),
    forElementAndStyle("Header", "level3", fontSize(helium.fontSizes.header3))
  )

  private def startDistance (value: Int) = "provisional-distance-between-starts" -> s"${value}mm"
  private val listStyles = Seq(
    forElement("BulletList", largeSpaceAfter, startDistance(5)),
    forElement("EnumList", largeSpaceAfter, startDistance(5)),
    forElement("DefinitionList", largeSpaceAfter, startDistance(20)),
    forElement("BulletListItem", defaultSpaceAfter),
    forElement("EnumListItem", defaultSpaceAfter),
    forElement("DefinitionListItem", defaultSpaceAfter)
  )

  private val codeProperties = Seq(codeFont, codeFontSize, codeLineHeight,
    bgColor(helium.colors.syntaxHighlighting.base.c1), color(helium.colors.syntaxHighlighting.base.c5),
    borderRadius(2), padding(2), marginLeft(2), marginRight(2), largeSpaceAfter) ++ preserveWhitespace
  
  private val codeStyles = Seq(
    forElement("LiteralBlock", codeProperties: _*),
    forElement("ParsedLiteralBlock", codeProperties: _*),
    forElement("CodeBlock", codeProperties: _*),
    forElement("Literal", codeFont, codeFontSize),
    forElement("InlineCode", codeFont, codeFontSize)
  )
  
  private def calloutProps (borderColor: Color, bgColorValue: Color) = Seq(
    bodyFont, defaultFontSize, defaultLineHeight, bgColor(bgColorValue), 
    "border-left" -> s"3pt solid ${borderColor.displayValue}",
    "fox:border-before-end-radius" -> "2mm", "fox:border-after-end-radius" -> "2mm", 
    paddingHack(3), marginLeft(2), marginRight(2), largeSpaceAfter
  )
  
  private val calloutStyles = Seq(
    forStyleNames("callout", "info", calloutProps(helium.colors.messages.info, helium.colors.messages.infoLight):_*),
    forStyleNames("callout", "warning", calloutProps(helium.colors.messages.warning, helium.colors.messages.warningLight):_*),
    forStyleNames("callout", "error", calloutProps(helium.colors.messages.error, helium.colors.messages.errorLight):_*)
  )

  private val tableStyles = Seq(
    forElement("Table", largeSpaceAfter, border(1, Color.hex("cccccc")), "border-collapse" -> "separate"),
    forChildElement(ElementType("TableHead"), ElementType("Cell"), "border-bottom" -> "1pt solid #cccccc", bold),
    forChildElement(ElementType("TableBody"), StyleName("cell-odd"), bgColor(Color.hex("f2f2f2"))),
    forElement("Cell", padding(2))
  )
  
  private val inlineStyles = Seq(
    forElement("Emphasized", italic),
    forElement("Strong", bold),
    forElement("Deleted", "text-decoration" -> "line-through"),
    forElement("Inserted", "text-decoration" -> "underline"),
    forStyleName("subscript", smallFontSize, "vertical-align" -> "sub"),
    forStyleName("superscript", smallFontSize, "vertical-align" -> "super"),
    forStyleName("footnote-label", smallFontSize, "vertical-align" -> "super")
  )
  
  private val linkStyles = Seq(
    forElement("FootnoteLink", color(helium.colors.secondary)),
    forElement("CitationLink", color(helium.colors.secondary)),
    forElement("SpanLink", color(helium.colors.secondary), bold),
    forChildElement(ElementType("NavigationLink"), ElementType("SpanLink"), color(helium.colors.primary), bold),
    forChildElement(
      Set[StylePredicate](ElementType("Paragraph"), StyleName("level2"), StyleName("nav")), 
      Set[StylePredicate](ElementType("SpanLink")), 
      color(helium.colors.secondary), bold),
  )

  private def codeColor (value: Color, categories: CodeCategory*): Seq[StyleDeclaration] =
    categories.map(c => forChildElement("CodeBlock", c.name, color(value)))
  
  private val syntaxColors = helium.colors.syntaxHighlighting
  private val syntaxHighlighting = 
    codeColor(syntaxColors.base.c2, CodeCategory.Comment, CodeCategory.XML.CData, CodeCategory.Markup.Quote) ++
    codeColor(syntaxColors.base.c3, CodeCategory.Tag.Punctuation) ++
    codeColor(syntaxColors.base.c4, CodeCategory.Identifier) ++
    codeColor(syntaxColors.wheel.c1, CodeCategory.Substitution, CodeCategory.Annotation, CodeCategory.Markup.Emphasized, CodeCategory.XML.ProcessingInstruction) ++
    codeColor(syntaxColors.wheel.c2, CodeCategory.Keyword, CodeCategory.EscapeSequence, CodeCategory.Markup.Headline) ++
    codeColor(syntaxColors.wheel.c3, CodeCategory.AttributeName, CodeCategory.DeclarationName, CodeCategory.Markup.LinkTarget) ++
    codeColor(syntaxColors.wheel.c4, CodeCategory.NumberLiteral, CodeCategory.StringLiteral, CodeCategory.LiteralValue, CodeCategory.BooleanLiteral, CodeCategory.CharLiteral, CodeCategory.SymbolLiteral, CodeCategory.RegexLiteral, CodeCategory.Markup.LinkText) ++
    codeColor(syntaxColors.wheel.c5, CodeCategory.TypeName, CodeCategory.Tag.Name, CodeCategory.XML.DTDTagName, CodeCategory.Markup.Fence)

  private def fontSize (value: Int) = "font-size" -> s"${value}pt" // TODO - replace/align with header styles
  private val navStyles = Seq(
    forElementAndStyle("Paragraph", "nav", marginLeft(8), fontSize(helium.fontSizes.body), spaceAfter(0), spaceBefore(2), 
      "text-align-last" -> "justify"),
    forElementAndStyles("Paragraph", "nav", "level1", fontSize(22), marginLeft(0), spaceBefore(15), color(helium.colors.secondary), 
      bold, "text-transform" -> "uppercase", "text-align-last" -> "center"),
    forElementAndStyles("Paragraph", "nav", "level2", fontSize(17), marginLeft(4), spaceBefore(7), color(helium.colors.secondary)),
    forElementAndStyles("Paragraph", "nav", "level3", fontSize(helium.fontSizes.header3), marginLeft(6), spaceBefore(3)),
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
  
  private def decoratedSpan (textColor: Color, bgColorValue: Color): Seq[(String, String)] = Seq(
    color(textColor), bgColor(bgColorValue), "padding" -> "1pt 2pt", border(1, textColor)
  )
  
  private val specialStyles = Seq(
    forElement("PageBreak", "page-break-before" -> "always"),
    forElement("Rule", defaultSpaceAfter, "leader-length" -> "100%", "rule-style" -> "solid", "rule-thickness" -> "2pt"),
    forElementAndStyle("RuntimeMessage", "debug", decoratedSpan(helium.colors.messages.info, helium.colors.messages.infoLight):_*),
    forElementAndStyle("RuntimeMessage", "info", decoratedSpan(helium.colors.messages.info, helium.colors.messages.infoLight):_*),
    forElementAndStyle("RuntimeMessage", "warning", decoratedSpan(helium.colors.messages.warning, helium.colors.messages.warningLight):_*),
    forElementAndStyle("RuntimeMessage", "error", decoratedSpan(helium.colors.messages.error, helium.colors.messages.errorLight):_*),
    forElementAndStyle("RuntimeMessage", "fatal", decoratedSpan(helium.colors.messages.error, helium.colors.messages.errorLight):_*)
  )
  
  private val allStyles =
    blockStyles ++
    headerStyles ++
    listStyles ++
    codeStyles ++
    calloutStyles ++
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
  val styles: StyleDeclarationSet = {
    val orderApplied = allStyles.zipWithIndex.map { case (style, index) =>
      style.copy(selector = style.selector.copy(order = index))
    }
    StyleDeclarationSet(Set.empty[Path], orderApplied.toSet, Precedence.Low)
  }
  
}
