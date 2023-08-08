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
import fs2.io.file.Files
import laika.api.{ MarkupParser, Renderer }
import laika.ast.{ DocumentTree, DocumentTreeRoot }
import laika.format.{ Markdown, PDF }
import laika.io.FileIO
import laika.io.implicits.*
import laika.io.model.{ InputTree, ParsedTree }
import laika.rewrite.DefaultTemplatePath
import munit.CatsEffectSuite

/** Since there is no straightforward way to validate a rendered PDF document
  *  on the JVM, this Spec merely asserts that a file or OutputStream is non-empty
  *  after rendering. Most of the functionality for PDF rendering that Laika itself
  *  provides lies in its support for XSL-FO rendering and application of CSS to
  *  this render process. This functionality is extensively tested in other Specs.
  *  For the actual creation of the binary PDF format, Laika primarily relies
  *  on Apache FOP for converting the rendered XSL-FO to PDF, therefore having
  *  limited scope in this particular spec is acceptable.
  */
class PDFRendererSpec extends CatsEffectSuite with FileIO with PDFTreeModel {

  private val templateParser = MarkupParser.of(Markdown).parallel[IO].build

  // run a parser with an empty input tree to obtain a parsed default template
  private val emptyTreeWithTemplate = templateParser.use(_.fromInput(InputTree[IO]).parse)

  def buildInputTree(templateTree: ParsedTree[IO], inputTree: DocumentTree): DocumentTreeRoot = {
    val treeWithTemplate = inputTree
      .withTemplates(
        Seq(templateTree.root.tree.selectTemplate(DefaultTemplatePath.forFO.relative).get)
      )
      .withConfig(templateTree.root.config)
    DocumentTreeRoot(treeWithTemplate)
  }

  test("render a tree to a file") {

    val renderer = Renderer.of(PDF)
      .parallel[IO]
      .build

    newTempFile.flatMap { tempFile =>
      val res = renderer.use { r =>
        emptyTreeWithTemplate.flatMap { templateTree =>
          r.from(buildInputTree(templateTree, createTree())).toFile(tempFile).render
        }
      }

      (res >> Files[IO].size(tempFile.toFS2Path)).map(_ > 0).assert
    }
  }

  test("render a tree to an OutputStream") {
    val renderer = Renderer
      .of(PDF)
      .parallel[IO]
      .build

    withByteArrayOutput { out =>
      renderer.use { r =>
        emptyTreeWithTemplate.flatMap { templateTree =>
          r.from(buildInputTree(templateTree, createTree())).toStream(IO.pure(out)).render.void
        }
      }
    }
      .map(_.nonEmpty)
      .assert
  }

}
