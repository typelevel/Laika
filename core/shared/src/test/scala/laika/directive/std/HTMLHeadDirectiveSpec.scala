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

import cats.data.NonEmptySet
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.{DocumentTreeRoot, Path, RawContent, RootElement, StaticDocument, TemplateDocument, TemplateElement, TemplateRoot}
import laika.ast.helper.ModelBuilder
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}
import laika.rewrite.nav.TargetFormats
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLHeadDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder
  with TreeModel {


  val staticDocs = Seq(
    StaticDocument(Root / "doc-1.css", TargetFormats.Selected("html")),
    StaticDocument(Root / "doc-2.epub.css", TargetFormats.Selected("epub", "epub.xhtml")),
    StaticDocument(Root / "doc-3.shared.css", TargetFormats.Selected("epub", "epub.xhtml", "html")),
    StaticDocument(Root / "sub2" / "doc-4.css", TargetFormats.Selected("html")),
  )

  def parseAndRewrite(template: String, 
                      templatePath: Path = DefaultTemplatePath.forHTML, 
                      static: Seq[StaticDocument] = staticDocs): Either[String, RootElement] = {

    val ctx = {
      val templateSuffix = templatePath.suffix.get.stripPrefix("template.")
      val finalFormat = if (templateSuffix == "html") "html" else "epub"
      TemplateContext(templateSuffix, finalFormat)
    }
    
    def applyTemplate(root: TemplateRoot): Either[String, DocumentTreeRoot] = {
      val templateDoc = TemplateDocument(templatePath, root)
      val inputTree = buildTree(List(templateDoc))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      val treeRoot = DocumentTreeRoot(tree, staticDocuments = static)
      TemplateRewriter.applyTemplates(treeRoot, ctx).left.map(_.message)
    }
    
    for {
      templateRoot <- templateParser.parse(template).toEither
      treeRoot     <- applyTemplate(templateRoot)
      docUnderTest <- treeRoot.tree.selectDocument(CurrentTree / "sub2" / "doc6").toRight("document under test missing")
    } yield docUnderTest.content
  }

  def buildResult (content: String): Either[String, RootElement] = {
    Right(root(TemplateRoot(
      t("aaa\n\n"),
      TemplateElement(RawContent(NonEmptySet.of("html","xhtml","epub"), content)),
      t("\n\nbbb")
    )))
  }

  
  "The linkCSS directive" should "pick all CSS documents apart from those for EPUB when used without attributes" in {
    val input =
      """aaa
        |
        |@:linkCSS
        |
        |bbb""".stripMargin

    val generatedHTML = """<link rel="stylesheet" type="text/css" href="../doc-1.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-3.shared.css" />
                          |    <link rel="stylesheet" type="text/css" href="doc-4.css" />""".stripMargin
    parseAndRewrite(input) should be (buildResult(generatedHTML))
  }

  it should "pick matching CSS documents apart from those for EPUB when used with an paths filter" in {
    val input =
      """aaa
        |
        |@:linkCSS { paths = [ /sub2 ] }
        |
        |bbb""".stripMargin
    
    val generatedHTML = """<link rel="stylesheet" type="text/css" href="doc-4.css" />""".stripMargin
    parseAndRewrite(input) should be (buildResult(generatedHTML))
  }

  it should "pick matching CSS documents apart from those for EPUB and respect the order of the specified filters" in {
    val input =
      """aaa
        |
        |@:linkCSS { paths = [ /sub2, / ] }
        |
        |bbb""".stripMargin

    val generatedHTML = """<link rel="stylesheet" type="text/css" href="doc-4.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-1.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-3.shared.css" />""".stripMargin
    parseAndRewrite(input) should be (buildResult(generatedHTML))
  }

  it should "pick all CSS documents for EPUB when used without attributes" in {
    val input =
      """aaa
        |
        |@:linkCSS
        |
        |bbb""".stripMargin

    val generatedHTML = """<link rel="stylesheet" type="text/css" href="../doc-2.epub.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-3.shared.css" />""".stripMargin
    parseAndRewrite(input, DefaultTemplatePath.forEPUB) should be (buildResult(generatedHTML))
  }

  "The linkJS directive" should "pick all JavaScript documents when used without attributes" in {
    val staticDocs = Seq(
      StaticDocument(Root / "doc-1.js", TargetFormats.Selected("html")),
      StaticDocument(Root / "doc-2.foo"),
      StaticDocument(Root / "doc-3.bar"),
      StaticDocument(Root / "sub2" / "doc-4.js", TargetFormats.Selected("html")),
    )
    val input =
      """aaa
        |
        |@:linkJS
        |
        |bbb""".stripMargin

    val generatedHTML = """<script src="../doc-1.js"></script>
                          |    <script src="doc-4.js"></script>""".stripMargin
    parseAndRewrite(input, static = staticDocs) should be (buildResult(generatedHTML))
  }
  
}
