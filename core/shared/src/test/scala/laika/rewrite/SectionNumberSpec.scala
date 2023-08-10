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

package laika.rewrite

import laika.api.builder.OperationConfig
import laika.ast.*
import laika.ast.Path.Root
import laika.ast.sample.{ BuilderKey, DocumentTreeAssertions, SampleTrees }
import laika.config.Config.ConfigResult
import laika.config.{ ConfigParser, Origin }
import laika.parse.GeneratedSource
import munit.FunSuite

class SectionNumberSpec extends FunSuite with DocumentTreeAssertions {

  trait TreeModel {

    def config: String
    def numberSections: Boolean
    def numberDocs: Boolean
    def depth: Option[Int] = None

    def isIncluded(level: Int): Boolean = depth.forall(_ >= level)

    def header(level: Int, title: Int, style: String = "section"): Header =
      Header(level, List(Text(s"Title $title")), Id(s"title$title") + Styles(style))

    def tree(content: RootElement): DocumentTree = {
      val autonumberConfig = ConfigParser
        .parse(config)
        .resolve()
        .toOption
        .get
        .withOrigin(Origin(Origin.TreeScope, Root))
      val docs             = SampleTrees.sixDocuments
        .docContent(content.content)
        .build
        .tree
        .allDocuments
        .toList
      DocumentTree.builder
        .addDocuments(docs)
        .addConfig(autonumberConfig)
        .build
    }

    def numberedHeader(
        level: Int,
        title: Int,
        num: List[Int],
        style: String = "section"
    ): Header = {
      val numbered = isIncluded(
        num.length
      ) && (style == "section" && numberSections) || (style == "title" && numberDocs)
      val number   = if (numbered) List(SectionNumber(num)) else Nil
      Header(level, number ++ List(Text(s"Title $title")), Id(s"title-$title") + Styles(style))
    }

    def numberedSection(level: Int, title: Int, num: List[Int], children: Section*): Section =
      Section(numberedHeader(level, title, num), children)

    val sections = RootElement(
      header(1, 1, "title") ::
        header(2, 2) ::
        header(3, 3) ::
        header(2, 4) ::
        header(3, 5) ::
        Nil
    )

    def resultContent(docNum: List[Int]): Seq[Block] = List(
      Title(numberedHeader(1, 1, docNum, "title").content, Id("title-1") + Style.title),
      numberedSection(2, 2, docNum :+ 1, numberedSection(3, 3, docNum :+ 1 :+ 1)),
      numberedSection(2, 4, docNum :+ 2, numberedSection(3, 5, docNum :+ 2 :+ 1))
    )

    lazy val expected: DocumentTree = {
      val docNums = List(List(1), List(2), List(3, 1), List(3, 2), List(4, 1), List(4, 2))
      def contents(key: BuilderKey): Seq[Block] = {
        val docNum = if (!numberDocs) Nil else docNums(key.num - 1)
        resultContent(docNum)
      }

      SampleTrees.sixDocuments
        .docContent(contents _)
        .build
        .tree
    }

    lazy val result: ConfigResult[DocumentTree] = {
      val docTree = tree(sections)
      docTree.rewrite(
        OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(docTree), RewritePhase.Resolve)
      )
    }

  }

  trait SectionsWithConfigError extends TreeModel {

    override def resultContent(docNum: List[Int]): List[Block] = List(
      InvalidBlock("Invalid value for autonumbering.scope: xxx", GeneratedSource),
      Title(numberedHeader(1, 1, docNum, "title").content, Id("title-1") + Style.title),
      numberedSection(2, 2, docNum :+ 1, numberedSection(3, 3, docNum :+ 1 :+ 1)),
      numberedSection(2, 4, docNum :+ 2, numberedSection(3, 5, docNum :+ 2 :+ 1))
    )

  }

  trait NumberAllConfig {

    val config = """laika.autonumbering { 
                   |  scope: all
                   |}""".stripMargin

    val numberSections = true
    val numberDocs     = true
  }

  trait NumberTwoLevels {

    val config = """laika.autonumbering { 
                   |  scope: all
                   |  depth: 2
                   |}""".stripMargin

    val numberSections = true
    val numberDocs     = true
  }

  trait NumberDocumentsConfig {

    val config = """laika.autonumbering { 
                   |  scope: documents
                   |}""".stripMargin

    val numberSections = false
    val numberDocs     = true
  }

  trait NumberSectionsConfig {

    val config = """laika.autonumbering { 
                   |  scope: sections
                   |}""".stripMargin

    val numberSections = true
    val numberDocs     = false
  }

  trait NumberNothingConfig {

    val config = """laika.autonumbering { 
                   |  scope: none
                   |}""".stripMargin

    val numberSections = false
    val numberDocs     = false
  }

  trait InvalidConfig {

    val config = """laika.autonumbering { 
                   |  scope: xxx
                   |}""".stripMargin

    val numberSections = false
    val numberDocs     = false
  }

  test("number documents, sections and titles") {
    new TreeModel with NumberAllConfig {
      result.assertEquals(expected)
    }
  }

  test("number documents and titles") {
    new TreeModel with NumberDocumentsConfig {
      result.assertEquals(expected)
    }
  }

  test("number sections only") {
    new TreeModel with NumberSectionsConfig {
      result.assertEquals(expected)
    }
  }

  test("number nothing") {
    new TreeModel with NumberNothingConfig {
      result.assertEquals(expected)
    }
  }

  test("number documents and sections two levels deep") {
    new TreeModel with NumberTwoLevels {
      override val depth = Some(2)
      result.assertEquals(expected)
    }
  }

}
