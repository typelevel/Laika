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

import cats.data.NonEmptyChain
import cats.effect.IO
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.config.Config
import laika.format.PDF
import laika.io.model.{RenderedDocument, RenderedTree, RenderedTreeRoot}
import laika.parse.markup.DocumentParser.InvalidDocument
import laika.render.fo.TestTheme
import laika.render.pdf.FOConcatenation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author Jens Halm
  */
class FOConcatenationSpec extends AnyFlatSpec with Matchers with ModelBuilder {

  val invalidElement = InvalidSpan("WRONG", generatedSource("faulty input"))
  
  val result = RenderedTreeRoot[IO](
    tree = RenderedTree(Root, None, Seq(RenderedDocument(Root / "doc", None, Nil, "content", Config.empty))),
    defaultTemplate = TemplateRoot(TemplateElement(invalidElement)),
    config = Config.empty,
    styles = TestTheme.foStyles
  )
  
  "The FO concatenation" should "fail when there are invalid elements in the template result" in {
    FOConcatenation(result, PDF.BookConfig(), OperationConfig.default) shouldBe Left(InvalidDocument(NonEmptyChain.one(invalidElement), Root / "merged.fo"))
  }
  
  it should "succeed when there are errors in the template result, but the filter is None" in {
    val config = OperationConfig.default.copy(
      renderMessages = MessageFilter.Warning,
      failOnMessages = MessageFilter.None
    )
    val fo = """<fo:inline background-color="#ffe9e3" border="1pt solid #d83030" color="#d83030" padding="1pt 2pt">WRONG</fo:inline> faulty input"""
    FOConcatenation(result, PDF.BookConfig(), config) shouldBe Right(fo)
  }
  
}
