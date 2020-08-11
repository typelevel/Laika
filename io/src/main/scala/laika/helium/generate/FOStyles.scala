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

package laika.helium.generate

import laika.ast.Path.Root
import laika.ast._
import laika.helium.Helium

/** The default styles for PDF and XSL-FO output.
  * 
  * @author Jens Halm
  */
class FOStyles (helium: Helium) {
  
  /*
  TODO - this string template approach is a temporary solution until Laika's 'CSS for PDF' supports CSS variables
  */

  /** The default styles for PDF and XSL-FO renderers.
    *
    * They can be overridden by placing a custom CSS document
    * with the name `default.fo.css` into the root directory
    * of the input files.
    */
  val input: String = s"""
    |Paragraph {
    |  font-family: ${helium.themeFonts.body};
    |  font-size: ${helium.fontSizes.body.displayValue};
    |  line-height: ${helium.pdfLayout.defaultLineHeight};
    |  text-align: justify;
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |TitledBlock {
    |  padding-left: 20mm;
    |  padding-right: 20mm;
    |  space-after: 6mm;
    |}
    |
    |TitledBlock .title {
    |  font-family: ${helium.themeFonts.headlines};
    |  font-size: ${helium.fontSizes.header4.displayValue};
    |  font-weight: bold;
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |QuotedBlock {
    |  font-style: italic;
    |  margin-left: 8mm;
    |  margin-right: 8mm;
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |QuotedBlock .attribution {
    |  font-family: ${helium.themeFonts.body};
    |  font-size: ${helium.fontSizes.body.displayValue};
    |  text-align: right;
    |  line-height: ${helium.pdfLayout.defaultLineHeight};
    |}
    |
    |Image {
    |  space-after: 6mm;
    |  width: 85%;
    |  content-width: scale-down-to-fit;
    |  scaling: uniform;
    |}
    |
    |Figure {
    |  space-after: 6mm;
    |}
    |
    |Figure .caption {
    |  font-family: ${helium.themeFonts.body};
    |  font-size: ${helium.fontSizes.code.displayValue};
    |  font-style: italic;
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |Figure .legend {
    |  font-size: ${helium.fontSizes.code.displayValue};
    |  font-style: italic;
    |}
    |
    |Footnote, Citation {
    |  font-size: ${helium.fontSizes.small.displayValue};
    |}
    |
    |.default-space {
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |Header {
    |  font-family: ${helium.themeFonts.headlines};
    |  font-size: ${helium.fontSizes.header4.displayValue};
    |  font-weight: bold;
    |  space-before: 7mm;
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |Title {
    |  font-family: ${helium.themeFonts.headlines};
    |  font-size: ${helium.fontSizes.title.displayValue};
    |  font-weight: bold;
    |  color: ${helium.colors.primary.displayValue};
    |  space-before: 0mm;
    |  space-after: 6mm;
    |}
    |
    |Header.level1 {
    |  font-size: ${helium.fontSizes.header2.displayValue};
    |  space-before: 12mm;
    |  space-after: 6mm;
    |}
    |
    |Header.level2 {
    |  font-size: ${helium.fontSizes.header2.displayValue};
    |}
    |
    |Header.level3 {
    |  font-size: ${helium.fontSizes.header3.displayValue};
    |}
    |
    |BulletList, EnumList {
    |  space-after: 6mm;
    |  provisional-distance-between-starts: 5mm;
    |}
    |
    |DefinitionList {
    |  space-after: 6mm;
    |  provisional-distance-between-starts: 20mm;
    |}
    |
    |BulletListItem, EnumListItem, DefinitionListItem {
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |CodeBlock, LiteralBlock, ParsedLiteralBlock {
    |  font-family: ${helium.themeFonts.code};
    |  font-size: ${helium.fontSizes.code.displayValue};
    |  line-height: 1.4;
    |  color: ${helium.colors.syntaxHighlighting.base.c5.displayValue};
    |  background-color: ${helium.colors.syntaxHighlighting.base.c1.displayValue};
    |  fox:border-radius: 2mm;
    |  margin-left: 2mm;
    |  margin-right: 2mm;
    |  padding: 2mm;
    |  white-space-treatment: preserve;
    |  linefeed-treatment: preserve;
    |  white-space-collapse: false;
    |  space-after: 6mm;
    |}
    |
    |Literal, InlineCode {
    |  font-family: ${helium.themeFonts.code};
    |  font-size: ${helium.fontSizes.code.displayValue};
    |}
    |
    |.callout {
    |  font-family: ${helium.themeFonts.body};
    |  font-size: ${helium.fontSizes.body.displayValue};
    |  line-height: ${helium.pdfLayout.defaultLineHeight};
    |  margin-left: 2mm;
    |  margin-right: 2mm;
    |  padding: ${helium.pdfLayout.defaultBlockSpacing.displayValue} ${helium.pdfLayout.defaultBlockSpacing.displayValue} 0.1mm ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |  fox:border-after-end-radius: 2mm;
    |  fox:border-before-end-radius: 2mm;
    |  space-after: 6mm;
    |}
    |
    |.callout.info {
    |  border-left: 3pt solid ${helium.colors.messages.info.displayValue};
    |  background-color: ${helium.colors.messages.infoLight.displayValue};
    |}
    |
    |.callout.warning {
    |  border-left: 3pt solid ${helium.colors.messages.warning.displayValue};
    |  background-color: ${helium.colors.messages.warningLight.displayValue};
    |}
    |
    |.callout.error {
    |  border-left: 3pt solid ${helium.colors.messages.error.displayValue};
    |  background-color: ${helium.colors.messages.errorLight.displayValue};
    |}
    |
    |Table {
    |  border: 1pt solid #cccccc;
    |  border-collapse: separate;
    |  space-after: 6mm;
    |}
    |
    |TableHead Cell {
    |  font-weight: bold;
    |  border-bottom: 1pt solid #cccccc;
    |}
    |
    |TableBody .cell-odd {
    |  background-color: #f2f2f2;
    |}
    |
    |Cell {
    |  padding: 2mm;
    |}
    |
    |FootnoteLink, CitationLink, SpanLink {
    |  color: ${helium.colors.secondary.displayValue};
    |}
    |
    |SpanLink {
    |  font-weight: bold;
    |}
    |
    |NavigationLink SpanLink {
    |  font-weight: bold;
    |  color: ${helium.colors.primary.displayValue};
    |}
    |
    |Paragraph.level2.nav SpanLink {
    |  font-weight: bold;
    |  color: ${helium.colors.secondary.displayValue};
    |}
    |
    |Emphasized {
    |  font-style: italic;
    |}
    |
    |Strong {
    |  font-weight: bold;
    |}
    |
    |Deleted {
    |  text-decoration: line-through;
    |}
    |
    |Inserted {
    |  text-decoration: underline;
    |}
    |
    |.subscript {
    |  font-size: ${helium.fontSizes.small.displayValue};
    |  vertical-align: sub;
    |}
    |
    |.superscript {
    |  font-size: ${helium.fontSizes.small.displayValue};
    |  vertical-align: super;
    |}
    |
    |.footnote-label {
    |  font-size: ${helium.fontSizes.small.displayValue};
    |  vertical-align: super;
    |}
    |
    |CodeBlock .comment, CodeBlock .xml-cdata, CodeBlock .markup-quote {
    |  color: ${helium.colors.syntaxHighlighting.base.c2.displayValue};
    |}
    |
    |CodeBlock .tag-punctuation {
    |  color: ${helium.colors.syntaxHighlighting.base.c3.displayValue};
    |}
    |
    |CodeBlock .identifier {
    |  color: ${helium.colors.syntaxHighlighting.base.c4.displayValue};
    |}
    |
    |CodeBlock .substitution, CodeBlock .annotation, CodeBlock .markup-emphasized, CodeBlock .xml-processing-instruction {
    |  color: ${helium.colors.syntaxHighlighting.wheel.c1.displayValue};
    |}
    |
    |CodeBlock .keyword, CodeBlock .escape-sequence, CodeBlock .markup-headline {
    |  color: ${helium.colors.syntaxHighlighting.wheel.c2.displayValue};
    |}
    |
    |CodeBlock .attribute-name, CodeBlock .declaration-name, CodeBlock .markup-link-target {
    |  color: ${helium.colors.syntaxHighlighting.wheel.c3.displayValue};
    |}
    |
    |CodeBlock .number-literal, CodeBlock .string-literal, CodeBlock .char-literal, CodeBlock .boolean-literal, CodeBlock .regex-literal, CodeBlock .symbol-literal, CodeBlock .literal-value, CodeBlock .markup-link-text {
    |  color: ${helium.colors.syntaxHighlighting.wheel.c4.displayValue};
    |}
    |
    |CodeBlock .type-name, CodeBlock .tag-name, CodeBlock .xml-dtd-tag-name, CodeBlock .markup-fence {
    |  color: ${helium.colors.syntaxHighlighting.wheel.c5.displayValue};
    |}
    |
    |Paragraph.nav {
    |  font-size: ${helium.fontSizes.body.displayValue};
    |  text-align-last: justify;
    |  margin-left: 8mm;
    |  space-before: 2mm;
    |  space-after: 0mm;
    |}
    |
    |Paragraph.nav.level1 {
    |  font-size: 22pt; /* TODO - align with header font sizes */
    |  font-weight: bold;
    |  color: ${helium.colors.secondary.displayValue};
    |  margin-left: 0mm;
    |  text-align-last: center;
    |  text-transform: uppercase;
    |  space-before: 15mm;
    |}
    |
    |Paragraph.nav.level2 {
    |  font-size: 17pt;
    |  color: ${helium.colors.secondary.displayValue};
    |  margin-left: 4mm;
    |  space-before: 7mm;
    |}
    |
    |Paragraph.nav.level3 {
    |  font-size: ${helium.fontSizes.header3.displayValue};
    |  margin-left: 6mm;
    |  space-before: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |Paragraph.nav.level4 {
    |  margin-left: 8mm;
    |}
    |
    |.align-top {
    |  vertical-align: top;
    |}
    |
    |.align-bottom {
    |  vertical-align: bottom;
    |}
    |
    |.align-middle {
    |  vertical-align: middle;
    |}
    |
    |.align-left {
    |  text-align: left;
    |}
    |
    |.align-right {
    |  text-align: right;
    |}
    |
    |.align-center {
    |  text-align: center;
    |}
    |
    |.keepWithPrevious { /* TODO - avoid camel case */
    |  keep-with-previous: always;
    |}
    |
    |.keepWithNext { /* TODO - avoid camel case */
    |  keep-with-next: always;
    |}
    |
    |.keep-together {
    |  page-break-inside: avoid;
    |}
    |
    |PageBreak {
    |  page-break-before: always;
    |}
    |
    |Rule {
    |  leader-length: 100%;
    |  rule-style: solid;
    |  rule-thickness: 2pt;
    |  space-after: ${helium.pdfLayout.defaultBlockSpacing.displayValue};
    |}
    |
    |RuntimeMessage.debug, RuntimeMessage.info {
    |  color: ${helium.colors.messages.info.displayValue};
    |  background-color: ${helium.colors.messages.infoLight.displayValue};
    |  padding: 1pt 2pt;
    |  border: 1pt solid ${helium.colors.messages.info.displayValue};
    |}
    |
    |RuntimeMessage.warning {
    |  color: ${helium.colors.messages.warning.displayValue};
    |  background-color: ${helium.colors.messages.warningLight.displayValue};
    |  padding: 1pt 2pt;
    |  border: 1pt solid ${helium.colors.messages.warning.displayValue};
    |}
    |
    |RuntimeMessage.error, RuntimeMessage.fatal {
    |  color: ${helium.colors.messages.error.displayValue};
    |  background-color: ${helium.colors.messages.errorLight.displayValue};
    |  padding: 1pt 2pt;
    |  border: 1pt solid ${helium.colors.messages.error.displayValue};
    |}
    |""".stripMargin

}

object FOStyles {
  
  val defaultPath: Path = Root / "styles.fo.css"
  
}
