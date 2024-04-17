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

import cats.effect.IO
import laika.api.builder.OperationConfig
import laika.api.bundle.PathTranslator
import laika.api.config.Config
import laika.api.errors.InvalidDocument
import laika.ast.Path.Root
import laika.ast.*
import laika.ast.sample.TestSourceBuilders
import laika.config.MessageFilters
import laika.format.XSLFO
import laika.io.model.{ RenderedDocument, RenderedTree, RenderedTreeRoot }
import laika.pdf.internal.FOConcatenation
import laika.render.fo.TestTheme
import laika.theme.config.BookConfig
import munit.FunSuite

/** @author Jens Halm
  */
class FOConcatenationSpec extends FunSuite with TestSourceBuilders {

  private val invalidElement = InvalidSpan("WRONG", generatedSource("faulty input"))

  private val result = {
    val tree = new RenderedTreeRoot[IO](
      tree = new RenderedTree(
        Root,
        None,
        Seq(new RenderedDocument(Root / "doc", None, Nil, "content", Config.empty))
      ),
      input = DocumentTreeRoot(DocumentTree.empty).addStyles(Map("fo" -> TestTheme.foStyles)),
      outputContext = OutputContext(XSLFO),
      pathTranslator = PathTranslator.noOp
    )
    tree.withDefaultTemplate(TemplateRoot(TemplateElement(invalidElement)))
  }

  test("fail when there are invalid elements in the template result") {
    val actual   = FOConcatenation(result, BookConfig.empty, OperationConfig.default)
    val expected = Left(InvalidDocument(Root / "merged.fo", invalidElement))
    assertEquals(actual, expected)
  }

  test("succeed when there are errors in the template result, but the filter is None") {
    val config   = OperationConfig.default
      .withMessageFilters(MessageFilters.forVisualDebugging)
    val expected =
      """<fo:inline background-color="#ffe9e3" border="1pt solid #d83030" color="#d83030" padding="1pt 2pt">WRONG</fo:inline> <fo:inline font-family="monospaced" font-size="0.9em">faulty input</fo:inline>"""
    assertEquals(FOConcatenation(result, BookConfig.empty, config), Right(expected))
  }

  test("collect fragments from all documents") {
    val template            = TestTheme.foTemplateWithFragment("foo")
    def fragment(num: Int)  = Map("foo" -> Paragraph(s"fragment $num"))
    val input               = DocumentTree.builder
      .addDocument(Document(Root / "doc-1", RootElement.empty).withFragments(fragment(1)))
      .addDocument(Document(Root / "doc-2", RootElement.empty).withFragments(fragment(2)))
      .buildRoot
      .addStyles(Map("fo" -> TestTheme.foStyles))
    val resultWithFragments = new RenderedTreeRoot[IO](
      tree = result.tree,
      input = input,
      outputContext = OutputContext(XSLFO),
      pathTranslator = PathTranslator.noOp
    ).withDefaultTemplate(template)
    val actual   = FOConcatenation(resultWithFragments, BookConfig.empty, OperationConfig.default)
    val expected =
      """<fo:block font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify">fragment 1</fo:block>
        |<fo:block font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify">fragment 2</fo:block><fo:bookmark-tree>
        |  <fo:bookmark internal-destination="_doc">
        |    <fo:bookmark-title>doc</fo:bookmark-title>
        |  </fo:bookmark>
        |</fo:bookmark-tree><fo:wrapper font-size="10pt">
        |content
        |</fo:wrapper>""".stripMargin
    assertEquals(actual, Right(expected))
  }

}
