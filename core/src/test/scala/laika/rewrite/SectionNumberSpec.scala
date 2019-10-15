/*
 * Copyright 2013-2016 the original author or authors.
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

import laika.api.config.{Config, ConfigBuilder}
import laika.api.builder.OperationConfig
import laika.ast._
import laika.ast.helper.DocumentViewBuilder.{Documents => Docs, _}
import laika.ast.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class SectionNumberSpec extends FlatSpec
                        with Matchers
                        with ModelBuilder {

  
  trait TreeModel {

    import Path.Root
    
    def header (level: Int, title: Int, style: String = "section") =
      Header(level,List(Text(s"Title $title")),Id(s"title$title") + Styles(style))
      
    def tree (content: RootElement): DocumentTree = {
      def docs (path: Path, nums: Int*) = nums map (n => Document(path / ("doc"+n), content))
      DocumentTree(Root, docs(Root, 1,2) ++ List(
        DocumentTree(Root / "sub1", docs(Root / "sub1",3,4)),
        DocumentTree(Root / "sub2", docs(Root / "sub2",5,6))
      ), config = parseConfig(config))
    }
    
    def numberedHeader (level: Int, title: Int, num: List[Int], style: String = "section"): Header = {
      val numbered = isIncluded(num.length) && (style == "section" && numberSections) || (style == "title" && numberDocs)
      val number = if (numbered) List(SectionNumber(num)) else Nil
      Header(level, number ++ List(Text(s"Title $title")),Id(s"title$title") + Styles(style))
    }
      
    def numberedSection (level: Int, title: Int, num: List[Int], children: Section*): Section =
      Section(numberedHeader(level, title, num), children)
      
    def numberedSectionInfo (level: Int, title: Int, num: List[Int], children: SectionInfo*): SectionInfo =
      SectionInfo(s"title$title", TitleInfo(numberedHeader(level, title, num).content), children)


    def treeView (content: List[Int] => List[DocumentContent]): TreeView = {
      def numbers (titleNums: List[Int]) = if (numberDocs) titleNums else Nil
      def docs (path: Path, nums: (Int, List[Int])*) = nums map { 
        case (fileNum, titleNums) => new DocumentView(path / ("doc"+fileNum), content(numbers(titleNums)))
      }
      TreeView(Root, Docs(DocumentType.Markup, docs(Root, (1,List(1)),(2,List(2)))) :: Subtrees(List(
        TreeView(Root / "sub1", Docs(DocumentType.Markup, docs(Root / "sub1",(3,List(3,1)),(4, List(3,2)))) :: Nil),
        TreeView(Root / "sub2", Docs(DocumentType.Markup, docs(Root / "sub2",(5,List(4,1)),(6, List(4,2)))) :: Nil)
      )) :: Nil)
    }
    
    def parseConfig (source: String): Config = ConfigBuilder.parse(source).build
    
    def config: String
    def numberSections: Boolean
    def numberDocs: Boolean
    
    def depth: Option[Int] = None
    
    def isIncluded (level: Int): Boolean = depth map (_ >= level) getOrElse true
  }
  
  trait NumberAllConfig {
    val config = """autonumbering { 
      |  scope: all
      |}""".stripMargin
    val numberSections = true
    val numberDocs = true
  }
  
  trait NumberTwoLevels {
    val config = """autonumbering { 
      |  scope: all
      |  depth: 2
      |}""".stripMargin
    val numberSections = true
    val numberDocs = true
  }
  
  trait NumberDocumentsConfig {
    val config = """autonumbering { 
      |  scope: documents
      |}""".stripMargin
    val numberSections = false
    val numberDocs = true
  }
  
  trait NumberSectionsConfig {
    val config = """autonumbering { 
      |  scope: sections
      |}""".stripMargin
    val numberSections = true
    val numberDocs = false
  }
  
  trait NumberNothingConfig {
    val config = """autonumbering { 
      |  scope: none
      |}""".stripMargin
    val numberSections = false
    val numberDocs = false
  }
  
  
  trait SectionsWithTitle extends TreeModel {
    val sections = RootElement(
      header(1,1,"title") ::
      header(2,2) ::
      header(3,3) ::
      header(2,4) ::
      header(3,5) ::
      Nil
    )
    
    def resultView (docNum: List[Int]): List[DocumentContent] = List(
      Content(List(
        laika.ast.Title(numberedHeader(1,1, docNum, "title").content,Id("title1") + Styles("title")),
        numberedSection(2,2, docNum:+1, numberedSection(3,3, docNum:+1:+1)),
        numberedSection(2,4, docNum:+2, numberedSection(3,5, docNum:+2:+1))
      )),
      laika.ast.helper.DocumentViewBuilder.Title(numberedHeader(1,1, docNum, "title").content),
      Sections(List(
        numberedSectionInfo(2,2, docNum:+1, numberedSectionInfo(3,3, docNum:+1:+1)),
        numberedSectionInfo(2,4, docNum:+2, numberedSectionInfo(3,5, docNum:+2:+1))
      ))
    )

    lazy val expected: TreeView = treeView(resultView)
    lazy val result: TreeView = viewOf(tree(sections).rewrite(OperationConfig.default.rewriteRules))
  }
  
  trait SectionsWithoutTitle extends TreeModel {
    val sections = RootElement(
      header(1,1) ::
      header(2,2) ::
      header(1,3) ::
      header(2,4) ::
      Nil
    )
    
    def resultView (docNum: List[Int]): List[DocumentContent] = List(
      Content(List(
        numberedSection(1,1, docNum:+1, numberedSection(2,2, docNum:+1:+1)),
        numberedSection(1,3, docNum:+2, numberedSection(2,4, docNum:+2:+1))
      )),
      Sections(List(
        numberedSectionInfo(1,1, docNum:+1, numberedSectionInfo(2,2, docNum:+1:+1)),
        numberedSectionInfo(1,3, docNum:+2, numberedSectionInfo(2,4, docNum:+2:+1))
      ))
    )

    lazy val expected: TreeView = treeView(resultView)
    lazy val result: TreeView = viewOf(tree(sections).rewrite(OperationConfig.default.rewriteRules))
  }
  
  
  "The section numbering" should "number documents, sections and titles for a structure with title" in {
    new SectionsWithTitle with NumberAllConfig {
      result should be (expected)
    }
  } 
  
  it should "number documents and titles for a structure with title" in {
    new SectionsWithTitle with NumberDocumentsConfig {
      result should be (expected)
    }
  } 
  
  it should "number sections only for a structure with title" in {
    new SectionsWithTitle with NumberSectionsConfig {
      result should be (expected)
    }
  } 
  
  it should "number nothing for a structure with title" in {
    new SectionsWithTitle with NumberNothingConfig {
      result should be (expected)
    }
  } 
  
  it should "number documents and sections for a structure without title" in {
    new SectionsWithoutTitle with NumberAllConfig {
      result should be (expected)
    }
  } 
  
  it should "number documents only for a structure without title" in {
    new SectionsWithoutTitle with NumberDocumentsConfig {
      result should be (expected)
    }
  } 
  
  it should "number sections only for a structure without title" in {
    new SectionsWithoutTitle with NumberSectionsConfig {
      result should be (expected)
    }
  } 
  
  it should "number nothing for a structure without title" in {
    new SectionsWithoutTitle with NumberNothingConfig {
      result should be (expected)
    }
  } 
  
  it should "number documents and sections two levels deep for a structure without title" in {
    new SectionsWithoutTitle with NumberTwoLevels {
      override val depth = Some(2)
      result should be (expected)
    }
  } 
  
  
}
