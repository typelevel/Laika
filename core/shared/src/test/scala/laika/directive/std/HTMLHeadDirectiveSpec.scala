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

import cats.syntax.all.*
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.*
import laika.rewrite.nav.TargetFormats
import laika.rewrite.{ DefaultTemplatePath, OutputContext }
import munit.FunSuite
import RewriteSetup.*
import laika.config.{ ConfigBuilder, LaikaKeys }
import laika.format.HTML

class HTMLHeadDirectiveSpec extends FunSuite {

  private val staticDocs = Seq(
    StaticDocument(Root / "doc-1.css", TargetFormats.Selected("html")),
    StaticDocument(Root / "doc-2.epub.css", TargetFormats.Selected("epub", "epub.xhtml")),
    StaticDocument(Root / "doc-3.shared.css", TargetFormats.Selected("epub", "epub.xhtml", "html")),
    StaticDocument(Root / "doc-4.page.css", TargetFormats.Selected("html")),
    StaticDocument(Root / "tree-2" / "doc-4.css", TargetFormats.Selected("html"))
  )

  def parseAndRewrite(
      template: String,
      templatePath: Path = DefaultTemplatePath.forHTML,
      static: Seq[StaticDocument] = staticDocs,
      rootConfig: ConfigBuilder => ConfigBuilder = identity,
      docConfig: ConfigBuilder => ConfigBuilder = identity
  ): Either[String, RootElement] = {

    val ctx = {
      val templateSuffix = templatePath.suffix.get.stripPrefix("template.")
      val finalFormat    = if (templateSuffix == "html") "html" else "epub"
      OutputContext(templateSuffix, finalFormat)
    }

    def applyTemplate(root: TemplateRoot): Either[String, DocumentTreeRoot] = {
      val inputTree      =
        buildTree(Some((templatePath.name, root.content)), docConfigUnderTest = docConfig)
      val treeWithConfig = inputTree.withConfig(rootConfig(ConfigBuilder.empty).build)
      val resolveRules   = OperationConfig.default.rewriteRulesFor(
        DocumentTreeRoot(treeWithConfig),
        RewritePhase.Resolve
      )
      val renderRules    = OperationConfig.default.rewriteRulesFor(
        DocumentTreeRoot(treeWithConfig),
        RewritePhase.Render(HTML)
      )
      for {
        tree <- treeWithConfig.rewrite(resolveRules).leftMap(_.message)
        treeRoot = DocumentTreeRoot(tree, staticDocuments = static)
        result <- treeRoot.applyTemplates(renderRules, ctx).leftMap(_.message)
      } yield result
    }

    for {
      templateRoot <- templateParser.parse(template).toEither
      treeRoot     <- applyTemplate(templateRoot)
      docUnderTest <- treeRoot.tree.selectDocument(CurrentTree / "tree-2" / "doc-6").toRight(
        "document under test missing"
      )
    } yield docUnderTest.content
  }

  def buildResult(content: Seq[TemplateSpan]): Either[String, RootElement] = {
    Right(
      RootElement(
        TemplateRoot(
          TemplateString("aaa\n\n"),
          TemplateSpanSequence(content),
          TemplateString("\n\nbbb")
        )
      )
    )
  }

  def run(input: String, expectedResult: Seq[TemplateSpan])(implicit loc: munit.Location): Unit = {
    assertEquals(parseAndRewrite(input), buildResult(expectedResult))
  }

  def runWithDocConfig(
      input: String,
      config: ConfigBuilder => ConfigBuilder,
      expectedResult: Seq[TemplateSpan]
  )(implicit loc: munit.Location): Unit = {
    assertEquals(parseAndRewrite(input, docConfig = config), buildResult(expectedResult))
  }

  private val cssStart  = TemplateString("""<link rel="stylesheet" type="text/css" href="""")
  private val cssEnd    = TemplateString("""" />""")
  private val jsStart   = TemplateString("""<script src="""")
  private val jsEnd     = TemplateString(""""></script>""")
  private val separator = TemplateString("\n    ")
  private def rawLink(url: String): TemplateElement = TemplateElement(RawLink.internal(url))

  private val globalSearchPathsKey = LaikaKeys.site.css.child("globalSearchPaths")
  private val localSearchPathsKey  = LaikaKeys.site.css.child("searchPaths")

  private val linkCSSInput =
    """aaa
      |
      |@:linkCSS
      |
      |bbb""".stripMargin

  test("linkCSS - HTML - defaults") {
    run(
      linkCSSInput,
      Seq(
        cssStart,
        rawLink("/doc-1.css"),
        cssEnd,
        separator,
        cssStart,
        rawLink("/doc-3.shared.css"),
        cssEnd,
        separator,
        cssStart,
        rawLink("/tree-2/doc-4.css"),
        cssEnd
      )
    )
  }

  test("linkCSS - HTML - restrict inputs via global config") {
    runWithDocConfig(
      linkCSSInput,
      _.withValue(globalSearchPathsKey, Seq(Root / "tree-2")),
      Seq(
        cssStart,
        rawLink("/tree-2/doc-4.css"),
        cssEnd
      )
    )
  }

  test("linkCSS - HTML - expand inputs via document config") {
    runWithDocConfig(
      linkCSSInput,
      _.withValue(localSearchPathsKey, Seq(Root / "doc-4.page.css")),
      Seq(
        cssStart,
        rawLink("/doc-1.css"),
        cssEnd,
        separator,
        cssStart,
        rawLink("/doc-3.shared.css"),
        cssEnd,
        separator,
        cssStart,
        rawLink("/tree-2/doc-4.css"),
        cssEnd,
        separator,
        cssStart,
        rawLink("/doc-4.page.css"),
        cssEnd
      )
    )
  }

  test("linkCSS - EPUB - defaults") {
    val input =
      """aaa
        |
        |@:linkCSS
        |
        |bbb""".stripMargin

    val generatedNodes = Seq(
      cssStart,
      rawLink("/doc-2.epub.css"),
      cssEnd,
      separator,
      cssStart,
      rawLink("/doc-3.shared.css"),
      cssEnd
    )
    assertEquals(parseAndRewrite(input, DefaultTemplatePath.forEPUB), buildResult(generatedNodes))
  }

  test(
    "linkJS - pick all JavaScript documents apart from those for EPUB when used without attributes"
  ) {

    val staticDocs = Seq(
      StaticDocument(Root / "doc-1.js", TargetFormats.Selected("html")),
      StaticDocument(Root / "doc-2.epub.js", TargetFormats.Selected("epub")),
      StaticDocument(Root / "doc-2.foo"),
      StaticDocument(Root / "doc-3.bar"),
      StaticDocument(Root / "tree-2" / "doc-4.js", TargetFormats.Selected("html"))
    )
    val input      =
      """aaa
        |
        |@:linkJS
        |
        |bbb""".stripMargin

    val generatedNodes = Seq(
      jsStart,
      rawLink("/doc-1.js"),
      jsEnd,
      separator,
      jsStart,
      rawLink("/tree-2/doc-4.js"),
      jsEnd
    )
    assertEquals(parseAndRewrite(input, static = staticDocs), buildResult(generatedNodes))
  }

  test("linkJS - pick all JavaScript documents for EPUB when used without attributes") {
    val input =
      """aaa
        |
        |@:linkJS
        |
        |bbb""".stripMargin

    val staticDocs     = Seq(
      StaticDocument(Root / "doc-1.js", TargetFormats.Selected("html")),
      StaticDocument(Root / "doc-2.epub.js"),
      StaticDocument(Root / "doc-3.shared.js"),
      StaticDocument(Root / "doc-3.bar"),
      StaticDocument(Root / "tree-2" / "doc-4.js", TargetFormats.Selected("html"))
    )
    val generatedNodes = Seq(
      jsStart,
      rawLink("/doc-2.epub.js"),
      jsEnd,
      separator,
      jsStart,
      rawLink("/doc-3.shared.js"),
      jsEnd
    )
    assertEquals(
      parseAndRewrite(input, DefaultTemplatePath.forEPUB, staticDocs),
      buildResult(generatedNodes)
    )
  }

}
