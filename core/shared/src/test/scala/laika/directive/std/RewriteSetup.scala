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
import laika.ast.sample.{ BuilderKey, SampleConfig, SampleContent, SampleTrees }
import laika.ast.*
import munit.Assertions
import Path.Root
import laika.api.config.ConfigBuilder
import laika.ast.sample.SampleTrees.SampleTreeBuilder
import laika.format.HTML

object RewriteSetup extends TemplateParserSetup with MarkupParserSetup with Assertions {

  import laika.ast.sample.SampleTrees.sixDocuments.*

  private def targetFormats(
      includeTargetFormatConfig: Boolean
  ): SampleTreeBuilder => SampleTreeBuilder = {
    if (!includeTargetFormatConfig) identity
    else
      _
        .docConfig(paths.doc2, SampleConfig.targetFormats())
        .docConfig(paths.tree1_doc3, SampleConfig.targetFormats("html", "txt"))
        .docConfig(paths.tree1_doc4, SampleConfig.targetFormats("epub", "pdf"))
  }

  def buildTree(
      template: Option[(String, Seq[TemplateSpan])] = None,
      docUnderTest: Option[Seq[Block]] = None,
      docConfigUnderTest: ConfigBuilder => ConfigBuilder = identity,
      hasTitleDocs: Boolean = false,
      includeTargetFormatConfig: Boolean = false,
      docUnderTestIsTitle: Boolean = false
  ): DocumentTree = {

    val titleDocPaths = if (hasTitleDocs) List(paths.tree1, paths.tree2) else Nil
    val targetPath    = if (docUnderTestIsTitle) paths.tree2_titleDoc else paths.tree2_doc6
    val docContent    = docUnderTest.getOrElse(SampleContent.fourSections(BuilderKey.Doc(6)))

    val builder = SampleTrees.sixDocuments.builder
      .titleDocuments(titleDocPaths *)
      .docContent(SampleContent.fourSections)
      .docContent(targetPath, docContent)
      .docConfig(targetPath, docConfigUnderTest)
      .treeConfig(paths.tree1, SampleConfig.title("Tree 1"))
      .treeConfig(paths.tree2, SampleConfig.title("Tree 2"))
      .apply(targetFormats(includeTargetFormatConfig))
      .builder

    val builderWithTemplates = template.fold(builder) { case (name, spans) =>
      builder.addTemplate(TemplateDocument(Root / name, TemplateRoot(spans)))
    }

    builderWithTemplates
      .buildRoot
      .tree
  }

  private def rewriteTree(
      inputTree: DocumentTree,
      pathUnderTest: Path = Root / "tree-2" / "doc-6"
  ): Either[String, RootElement] = {
    inputTree
      .rewrite(
        OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree), RewritePhase.Build)
      )
      .flatMap(
        _.rewrite(
          OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree), RewritePhase.Resolve)
        )
      )
      .leftMap(_.message)
      .flatMap { tree =>
        val root  = DocumentTreeRoot(tree)
        val rules = OperationConfig.default.rewriteRulesFor(root, RewritePhase.Render(HTML))
        root
          .applyTemplates(rules, OutputContext(HTML))
          .leftMap(_.message)
          .flatMap { res =>
            res.tree
              .selectDocument(pathUnderTest.relative)
              .map(_.content)
              .toRight("document under test missing")
          }
      }
  }

  def parseTemplateAndRewrite(
      template: String,
      hasTitleDocs: Boolean = false,
      includeTargetFormatConfig: Boolean = false,
      additionalDocuments: Seq[Document] = Nil
  ): Either[String, RootElement] = {
    parseTemplate(template).flatMap { tRoot =>
      rewriteTree(
        buildTree(
          Some((DefaultTemplatePath.forHTML.name, tRoot.content)),
          hasTitleDocs = hasTitleDocs,
          includeTargetFormatConfig = includeTargetFormatConfig
        ).appendContent(additionalDocuments)
      )
    }
  }

  def parseDocumentAndRewrite(
      markup: String,
      hasTitleDocs: Boolean = false,
      includeTargetFormatConfig: Boolean = false,
      docUnderTestIsTitle: Boolean = false
  ): Either[String, RootElement] = {
    val pathUnderTest =
      if (docUnderTestIsTitle) Root / "tree-2" / "README" else Root / "tree-2" / "doc-6"
    parseUnresolved(markup, pathUnderTest)
      .left.map(_.message)
      .flatMap { markupDoc =>
        val tree = buildTree(
          None,
          Some(markupDoc.content.content),
          hasTitleDocs = hasTitleDocs,
          includeTargetFormatConfig = includeTargetFormatConfig,
          docUnderTestIsTitle = docUnderTestIsTitle
        )
        rewriteTree(tree, pathUnderTest)
      }
  }

}
