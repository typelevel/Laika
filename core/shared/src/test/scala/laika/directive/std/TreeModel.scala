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

package laika.directive.std

import laika.api.builder.OperationConfig
import laika.ast.RelativePath.CurrentTree
import laika.ast.{Document, DocumentTree, DocumentTreeRoot, Header, Path, RootElement, Style, Styles, TemplateDocument, Text}
import laika.config.{Config, ConfigBuilder, LaikaKeys, Origin}
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}

trait TreeModel extends TemplateParserSetup with MarkupParserSetup {

  import Path.Root

  def hasTitleDocs: Boolean = false

  def header (level: Int, title: Int, style: String = "section") =
    Header(level,List(Text("Section "+title)), Styles(style))

  val sectionsWithoutTitle = RootElement(
    Header(1, List(Text("Title")), Style.title) ::
      header(1,1) ::
      header(2,2) ::
      header(1,3) ::
      header(2,4) ::
      Nil
  )

  def includeTargetFormatConfig: Boolean = false

  def targetFormatsFor (name: String): Option[Seq[String]] =
    if (!includeTargetFormatConfig) None else name match {
      case "doc2" => Some(Nil)
      case "doc3" => Some(Seq("html", "txt"))
      case "doc4" => Some(Seq("epub", "pdf"))
      case _ => None
    }

  def config(path: Path, title: String, scope: Origin.Scope): Config = ConfigBuilder
    .withOrigin(Origin(scope, path))
    .withValue(LaikaKeys.title, title)
    .withValue(LaikaKeys.targetFormats, targetFormatsFor(path.name))
    .build

  def titleDoc (path: Path): Option[Document] =
    if (!hasTitleDocs || path == Root) None
    else Some(Document(path / "title", sectionsWithoutTitle, config = config(path / "title", "TitleDoc", Origin.DocumentScope)))

  def buildTree (templates: List[TemplateDocument] = Nil, docUnderTest: Option[Document] = None): DocumentTree = {
    def docs (parent: Path, nums: Int*): Seq[Document] = nums map { n =>
      val docConfig = config(parent / ("doc"+n), "Doc "+n, Origin.DocumentScope)
      (n, docUnderTest) match {
        case (6, Some(doc)) => doc.copy(config = docConfig)
        case _ => Document(parent / ("doc"+n), sectionsWithoutTitle, config = docConfig)
      }
    }
    DocumentTree(Root, docs(Root, 1,2) ++ List(
      DocumentTree(Root / "sub1", docs(Root / "sub1",3,4), titleDoc(Root / "sub1"), config = config(Root / "sub1", "Tree 1", Origin.TreeScope)),
      DocumentTree(Root / "sub2", docs(Root / "sub2",5,6), titleDoc(Root / "sub2"), config = config(Root / "sub2", "Tree 2", Origin.TreeScope))
    ), templates = templates)
  }
  
  private def rewriteTree (inputTree: DocumentTree) = {
    val rules = OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree))
    val tree  = inputTree.rewrite(rules)
    TemplateRewriter
      .applyTemplates(DocumentTreeRoot(tree), TemplateContext("html"))
      .left.map(_.message)
      .flatMap { res =>
        res.tree
          .selectDocument(CurrentTree / "sub2" / "doc6")
          .map(_.content)
          .toRight("document under test missing")
      }
  }

  def parseTemplateAndRewrite (template: String): Either[String, RootElement] = {
    parseTemplate(template).flatMap { tRoot =>
      val templateDoc = TemplateDocument(DefaultTemplatePath.forHTML, tRoot)
      rewriteTree(buildTree(List(templateDoc)))
    }
  }

  def parseDocumentAndRewrite (markup: String): Either[String, RootElement] = {
    parseUnresolved(markup, Root / "sub2" / "doc6")
      .left.map(_.message)
      .flatMap { markupDoc => 
        rewriteTree(buildTree(Nil, Some(markupDoc)))
      }
  }

}
