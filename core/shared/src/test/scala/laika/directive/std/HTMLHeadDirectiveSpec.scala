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
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast._
import laika.rewrite.nav.TargetFormats
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLHeadDirectiveSpec extends AnyFlatSpec
  with Matchers
  with RewriteSetup {


  val staticDocs = Seq(
    StaticDocument(Root / "doc-1.css", TargetFormats.Selected("html")),
    StaticDocument(Root / "doc-2.epub.css", TargetFormats.Selected("epub", "epub.xhtml")),
    StaticDocument(Root / "doc-3.shared.css", TargetFormats.Selected("epub", "epub.xhtml", "html")),
    StaticDocument(Root / "tree-2" / "doc-4.css", TargetFormats.Selected("html")),
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
      val inputTree = buildTree(Some(templatePath.name, root.content))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      val treeRoot = DocumentTreeRoot(tree, staticDocuments = static)
      TemplateRewriter.applyTemplates(treeRoot, ctx).left.map(_.message)
    }
    
    for {
      templateRoot <- templateParser.parse(template).toEither
      treeRoot     <- applyTemplate(templateRoot)
      docUnderTest <- treeRoot.tree.selectDocument(CurrentTree / "tree-2" / "doc-6").toRight("document under test missing")
    } yield docUnderTest.content
  }

  def buildResult (content: Seq[TemplateSpan]): Either[String, RootElement] = {
    Right(RootElement(TemplateRoot(
      TemplateString("aaa\n\n"), 
      TemplateSpanSequence(content), 
      TemplateString("\n\nbbb")
    )))
  }
  
  private val cssStart = TemplateString("""<link rel="stylesheet" type="text/css" href="""")
  private val cssEnd = TemplateString("""" />""")
  private val separator = TemplateString("\n    ")
  private def rawLink (url: String): TemplateElement = TemplateElement(RawLink.internal(url))
  
  "The linkCSS directive" should "pick all CSS documents apart from those for EPUB when used without attributes" in {
    val input =
      """aaa
        |
        |@:linkCSS
        |
        |bbb""".stripMargin

    val generatedNodes = Seq(
      cssStart, rawLink("/doc-1.css"), cssEnd, separator,
      cssStart, rawLink("/doc-3.shared.css"), cssEnd, separator,
      cssStart, rawLink("/tree-2/doc-4.css"), cssEnd
    )
    parseAndRewrite(input) should be (buildResult(generatedNodes))
  }

  it should "pick matching CSS documents apart from those for EPUB when used with an paths filter" in {
    val input =
      """aaa
        |
        |@:linkCSS { paths = [ /tree-2 ] }
        |
        |bbb""".stripMargin

    val generatedNodes = Seq(
      cssStart, rawLink("/tree-2/doc-4.css"), cssEnd
    )
    parseAndRewrite(input) should be (buildResult(generatedNodes))
  }

  it should "pick matching CSS documents apart from those for EPUB and respect the order of the specified filters" in {
    val input =
      """aaa
        |
        |@:linkCSS { paths = [ /tree-2, / ] }
        |
        |bbb""".stripMargin

    val generatedNodes = Seq(
      cssStart, rawLink("/tree-2/doc-4.css"), cssEnd, separator,
      cssStart, rawLink("/doc-1.css"), cssEnd, separator,
      cssStart, rawLink("/doc-3.shared.css"), cssEnd
    )
    parseAndRewrite(input) should be (buildResult(generatedNodes))
  }

  it should "pick all CSS documents for EPUB when used without attributes" in {
    val input =
      """aaa
        |
        |@:linkCSS
        |
        |bbb""".stripMargin

    val generatedNodes = Seq(
      cssStart, rawLink("/doc-2.epub.css"), cssEnd, separator,
      cssStart, rawLink("/doc-3.shared.css"), cssEnd
    )
    parseAndRewrite(input, DefaultTemplatePath.forEPUB) should be (buildResult(generatedNodes))
  }

  "The linkJS directive" should "pick all JavaScript documents when used without attributes" in {
    val jsStart = TemplateString("""<script src="""")
    val jsEnd = TemplateString(""""></script>""")
    val staticDocs = Seq(
      StaticDocument(Root / "doc-1.js", TargetFormats.Selected("html")),
      StaticDocument(Root / "doc-2.foo"),
      StaticDocument(Root / "doc-3.bar"),
      StaticDocument(Root / "tree-2" / "doc-4.js", TargetFormats.Selected("html")),
    )
    val input =
      """aaa
        |
        |@:linkJS
        |
        |bbb""".stripMargin

    val generatedNodes = Seq(
      jsStart, rawLink("/doc-1.js"), jsEnd, separator,
      jsStart, rawLink("/tree-2/doc-4.js"), jsEnd
    )
    parseAndRewrite(input, static = staticDocs) should be (buildResult(generatedNodes))
  }
  
}
