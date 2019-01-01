/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.render.epub

import laika.api.Render
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.EPUB
import laika.io
import laika.io.Input
import laika.io.OutputTree.{ResultTree, StringResult}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Jens Halm
  */
class XHTMLRendererSpec extends FlatSpec with Matchers with ModelBuilder {

  trait DocBuilder {

    def markupDoc (num: Int, path: Path = Root) = Document(path / ("doc"+num), root(p("Doc"+num)))

    def staticDoc (num: Int, path: Path = Root) = StaticDocument(Input.fromString(s"p { margin: ${num}px; }", path / s"styles$num.css"))
  }

  trait StringRenderer {

    def renderedDocs (tree: DocumentTree): Seq[StringResult] = {

      def collectDocuments (result: ResultTree): Seq[StringResult] =
        result.results ++ result.subtrees.flatMap(collectDocuments)


      val out = new io.OutputTree.StringOutputTree(Path.Root)
      Render.as(EPUB.XHTML).from(tree).toOutputTree(out)

      collectDocuments(out.result)
    }

    def renderedXhtml (num: Int, style1: String, style2: String): String = s"""<?xml version="1.0" encoding="UTF-8"?>
     |<!DOCTYPE html>
     |<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
     |  <head>
     |    <meta charset="utf-8" />
     |    <meta name="generator" content="laika" />
     |    <title></title>
     |    <link rel="stylesheet" type="text/css" href="$style1" />
     |    <link rel="stylesheet" type="text/css" href="$style2" />
     |  </head>
     |  <body epub:type="bodymatter">
     |    <div class="content">
     |      <p>Doc$num</p>
     |    </div>
     |  </body>
     |</html>""".stripMargin

  }

  "The XHTML Renderer for EPUB" should "render a tree with 2 documents and 2 style sheets" in {
    new DocBuilder with StringRenderer {
      val input = DocumentTree(Root,
        content = List(
          markupDoc(1),
          DocumentTree(Root / "sub",
            content = List(markupDoc(2, Root / "sub")),
            additionalContent = List(staticDoc(2, Root / "sub"))
          )
        ),
        additionalContent = List(staticDoc(1))
      )

      val expected = Seq(
        StringResult(Root / "doc1.xhtml", renderedXhtml(1, "sub/styles2.css", "styles1.css")),
        StringResult(Root / "sub" / "doc2.xhtml", renderedXhtml(2, "styles2.css", "../styles1.css"))
      )

      renderedDocs(input) shouldBe expected
    }
  }


}
