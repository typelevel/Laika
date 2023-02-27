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

private[laika] class FOStyles(helium: Helium) {

  import helium.pdfSettings._

  /*
  TODO - this string template approach is a temporary solution until Laika's 'CSS for PDF' supports CSS variables
   */

  val input: String =
    s"""
       |Paragraph {
       |  font-family: ${themeFonts.body};
       |  font-size: ${fontSizes.body.displayValue};
       |  line-height: ${layout.defaultLineHeight};
       |  text-align: justify;
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |TitledBlock {
       |  padding-left: 20mm;
       |  padding-right: 20mm;
       |  space-after: 6mm;
       |}
       |
       |TitledBlock .title {
       |  font-family: ${themeFonts.headlines};
       |  font-size: ${fontSizes.header4.displayValue};
       |  font-weight: bold;
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |QuotedBlock {
       |  font-style: italic;
       |  margin-left: 8mm;
       |  margin-right: 8mm;
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |QuotedBlock .attribution {
       |  font-family: ${themeFonts.body};
       |  font-size: ${fontSizes.body.displayValue};
       |  text-align: right;
       |  line-height: ${layout.defaultLineHeight};
       |}
       |
       |Image {
       |  space-after: 6mm;
       |  width: 85%;
       |  height: auto;
       |  content-width: scale-down-to-fit;
       |  scaling: uniform;
       |}
       |
       |Figure {
       |  space-after: 6mm;
       |}
       |
       |Figure .caption {
       |  font-family: ${themeFonts.body};
       |  font-size: ${fontSizes.code.displayValue};
       |  font-style: italic;
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |Figure .legend {
       |  font-size: ${fontSizes.code.displayValue};
       |  font-style: italic;
       |}
       |
       |Footnote, Citation {
       |  font-size: ${fontSizes.small.displayValue};
       |}
       |
       |.default-space {
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |Header {
       |  font-family: ${themeFonts.headlines};
       |  font-size: ${fontSizes.header4.displayValue};
       |  font-weight: bold;
       |  space-before: 7mm;
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |Title {
       |  font-family: ${themeFonts.headlines};
       |  font-size: ${fontSizes.title.displayValue};
       |  font-weight: bold;
       |  color: ${colors.theme.primary.displayValue};
       |  space-before: 0mm;
       |  space-after: 6mm;
       |}
       |
       |Header.level1 {
       |  font-size: ${fontSizes.header2.displayValue};
       |  space-before: 12mm;
       |  space-after: 6mm;
       |}
       |
       |Header.level2 {
       |  font-size: ${fontSizes.header2.displayValue};
       |}
       |
       |Header.level3 {
       |  font-size: ${fontSizes.header3.displayValue};
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
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |CodeBlock, LiteralBlock, ParsedLiteralBlock {
       |  font-family: ${themeFonts.code};
       |  font-size: ${fontSizes.code.displayValue};
       |  line-height: 1.4;
       |  color: ${colors.syntaxHighlighting.base.c5.displayValue};
       |  background-color: ${colors.syntaxHighlighting.base.c1.displayValue};
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
       |  font-family: ${themeFonts.code};
       |  font-size: ${fontSizes.code.displayValue};
       |}
       |
       |SpanSequence.icon {
       |  padding-top: -2mm;
       |}
       |
       |IconGlyph {
       |  font-family: IcoFont; 
       |  font-size: 16pt;
       |}
       |
       |InlineSVGIcon {
       |  content-height: 1.5em;
       |  content-width: 1.5em;
       |}
       |
       |.svg-shape {
       |  color: ${colors.theme.primary.displayValue};
       |}
       |
       |.callout.info IconGlyph, .callout.info .svg-shape {
       |  color: ${colors.messages.info.displayValue};
       |}
       |.callout.warning IconGlyph, .callout.warning .svg-shape {
       |  color: ${colors.messages.warning.displayValue};
       |}
       |.callout.error IconGlyph, .callout.error .svg-shape {
       |  color: ${colors.messages.error.displayValue};
       |}
       |
       |.callout {
       |  font-family: ${themeFonts.body};
       |  font-size: ${fontSizes.body.displayValue};
       |  line-height: ${layout.defaultLineHeight};
       |  margin-left: 2mm;
       |  margin-right: 2mm;
       |  padding: ${layout.defaultBlockSpacing.displayValue} ${
        layout.defaultBlockSpacing.displayValue
      } 0.1mm ${layout.defaultBlockSpacing.displayValue};
       |  fox:border-after-end-radius: 2mm;
       |  fox:border-before-end-radius: 2mm;
       |  space-after: 6mm;
       |}
       |
       |.callout.info {
       |  border-left: 3pt solid ${colors.messages.info.displayValue};
       |  background-color: ${colors.messages.infoLight.displayValue};
       |}
       |
       |.callout.warning {
       |  border-left: 3pt solid ${colors.messages.warning.displayValue};
       |  background-color: ${colors.messages.warningLight.displayValue};
       |}
       |
       |.callout.error {
       |  border-left: 3pt solid ${colors.messages.error.displayValue};
       |  background-color: ${colors.messages.errorLight.displayValue};
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
       |  color: ${colors.theme.secondary.displayValue};
       |}
       |
       |SpanLink {
       |  font-weight: bold;
       |}
       |
       |NavigationItem SpanLink {
       |  font-weight: bold;
       |  color: ${colors.theme.primary.displayValue};
       |}
       |
       |Paragraph.level2.nav SpanLink {
       |  font-weight: bold;
       |  color: ${colors.theme.secondary.displayValue};
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
       |  font-size: ${fontSizes.small.displayValue};
       |  vertical-align: sub;
       |}
       |
       |.superscript {
       |  font-size: ${fontSizes.small.displayValue};
       |  vertical-align: super;
       |}
       |
       |.footnote-label {
       |  font-size: ${fontSizes.small.displayValue};
       |  vertical-align: super;
       |}
       |
       |CodeBlock .comment, CodeBlock .xml-cdata, CodeBlock .markup-quote {
       |  color: ${colors.syntaxHighlighting.base.c2.displayValue};
       |}
       |
       |CodeBlock .tag-punctuation {
       |  color: ${colors.syntaxHighlighting.base.c3.displayValue};
       |}
       |
       |CodeBlock .identifier {
       |  color: ${colors.syntaxHighlighting.base.c4.displayValue};
       |}
       |
       |CodeBlock .substitution, CodeBlock .annotation, CodeBlock .markup-emphasized, CodeBlock .xml-processing-instruction {
       |  color: ${colors.syntaxHighlighting.wheel.c1.displayValue};
       |}
       |
       |CodeBlock .keyword, CodeBlock .escape-sequence, CodeBlock .markup-headline {
       |  color: ${colors.syntaxHighlighting.wheel.c2.displayValue};
       |}
       |
       |CodeBlock .attribute-name, CodeBlock .declaration-name, CodeBlock .markup-link-target {
       |  color: ${colors.syntaxHighlighting.wheel.c3.displayValue};
       |}
       |
       |CodeBlock .number-literal, CodeBlock .string-literal, CodeBlock .char-literal, CodeBlock .boolean-literal, CodeBlock .regex-literal, CodeBlock .symbol-literal, CodeBlock .literal-value, CodeBlock .markup-link-text {
       |  color: ${colors.syntaxHighlighting.wheel.c4.displayValue};
       |}
       |
       |CodeBlock .type-name, CodeBlock .tag-name, CodeBlock .xml-dtd-tag-name, CodeBlock .markup-fence {
       |  color: ${colors.syntaxHighlighting.wheel.c5.displayValue};
       |}
       |
       |Paragraph.nav {
       |  font-size: ${fontSizes.body.displayValue};
       |  text-align-last: justify;
       |  margin-left: 8mm;
       |  space-before: 2mm;
       |  space-after: 0mm;
       |}
       |
       |Paragraph.nav.level1 {
       |  font-size: 22pt; /* TODO - align with header font sizes */
       |  font-weight: bold;
       |  color: ${colors.theme.secondary.displayValue};
       |  margin-left: 0mm;
       |  text-align-last: center;
       |  text-transform: uppercase;
       |  space-before: 15mm;
       |}
       |
       |Paragraph.nav.level2 {
       |  font-size: 17pt;
       |  color: ${colors.theme.secondary.displayValue};
       |  margin-left: 4mm;
       |  space-before: 7mm;
       |}
       |
       |Paragraph.nav.level3 {
       |  font-size: ${fontSizes.header3.displayValue};
       |  margin-left: 6mm;
       |  space-before: ${layout.defaultBlockSpacing.displayValue};
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
       |.keep-together.pdf {
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
       |  space-after: ${layout.defaultBlockSpacing.displayValue};
       |}
       |
       |RuntimeMessage.debug, RuntimeMessage.info {
       |  color: ${colors.messages.info.displayValue};
       |  background-color: ${colors.messages.infoLight.displayValue};
       |  padding: 1pt 2pt;
       |  border: 1pt solid ${colors.messages.info.displayValue};
       |}
       |
       |RuntimeMessage.warning {
       |  color: ${colors.messages.warning.displayValue};
       |  background-color: ${colors.messages.warningLight.displayValue};
       |  padding: 1pt 2pt;
       |  border: 1pt solid ${colors.messages.warning.displayValue};
       |}
       |
       |RuntimeMessage.error, RuntimeMessage.fatal {
       |  color: ${colors.messages.error.displayValue};
       |  background-color: ${colors.messages.errorLight.displayValue};
       |  padding: 1pt 2pt;
       |  border: 1pt solid ${colors.messages.error.displayValue};
       |}
       |""".stripMargin

}

object FOStyles {

  val defaultPath: Path = Root / "styles.fo.css"

}
