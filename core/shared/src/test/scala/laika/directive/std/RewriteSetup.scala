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

import cats.syntax.all._
import laika.api.builder.OperationConfig
import laika.ast.sample.{BuilderKey, SampleConfig, SampleContent, SampleSixDocuments, SampleTrees}
import laika.ast._
import laika.rewrite.{DefaultTemplatePath, OutputContext, TemplateRewriter}
import munit.Assertions
import Path.Root
import laika.format.HTML

object RewriteSetup extends TemplateParserSetup with MarkupParserSetup with Assertions {


  private def targetFormats(includeTargetFormatConfig: Boolean): SampleSixDocuments => SampleSixDocuments =
    if (!includeTargetFormatConfig) identity else _
      .doc2.config(SampleConfig.targetFormats())
      .doc3.config(SampleConfig.targetFormats("html", "txt"))
      .doc4.config(SampleConfig.targetFormats("epub", "pdf"))

  def buildTree (template: Option[(String, Seq[TemplateSpan])] = None, 
                 docUnderTest: Option[Seq[Block]] = None, 
                 hasTitleDocs: Boolean = false,
                 includeTargetFormatConfig: Boolean = false,
                 docUnderTestIsTitle: Boolean = false): DocumentTree = {
    
    def templateF = template.fold[SampleSixDocuments => SampleSixDocuments](identity) { 
      case (name, spans) => _.root.template(name, spans) 
    }
    val titleDocs: SampleSixDocuments => SampleSixDocuments = if (hasTitleDocs) _.titleDocuments(includeRoot = false) else identity // TODO - cleanup
    
    val docContent = docUnderTest.getOrElse(SampleContent.fourSections(BuilderKey.Doc(6)))
    val docContentF: SampleSixDocuments => SampleSixDocuments = 
      if (docUnderTestIsTitle) _.tree2.titleDoc.content(docContent)
      else _.doc6.content(docContent)
    
    SampleTrees.sixDocuments
      .docContent(SampleContent.fourSections)
      .apply(docContentF)
      .tree1.config(SampleConfig.title("Tree 1"))
      .tree2.config(SampleConfig.title("Tree 2")) // TODO - generalize
      .apply(titleDocs)
      .apply(templateF)
      .apply(targetFormats(includeTargetFormatConfig))
      .build
      .tree
  }
  
  private def rewriteTree (inputTree: DocumentTree, pathUnderTest: Path = Root / "tree-2" / "doc-6"): Either[String, RootElement] = {
    inputTree
      .rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree), RewritePhase.Build))
      .flatMap(_.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree), RewritePhase.Resolve)))
      .leftMap(_.message)
      .flatMap { tree =>
        val root = DocumentTreeRoot(tree)
        val rules = OperationConfig.default.rewriteRulesFor(root, RewritePhase.Render(HTML))
        TemplateRewriter
          .applyTemplates(root, rules, OutputContext(HTML))
          .leftMap(_.message)
          .flatMap { res =>
            res.tree
              .selectDocument(pathUnderTest.relative)
              .map(_.content)
              .toRight("document under test missing")
          }
        }
  }

  def parseTemplateAndRewrite (template: String, 
                               hasTitleDocs: Boolean = false, 
                               includeTargetFormatConfig: Boolean = false): Either[String, RootElement] = {
    parseTemplate(template).flatMap { tRoot =>
      rewriteTree(buildTree(Some((DefaultTemplatePath.forHTML.name, tRoot.content)), 
        hasTitleDocs = hasTitleDocs, includeTargetFormatConfig = includeTargetFormatConfig))
    }
  }

  def parseDocumentAndRewrite (markup: String, 
                               hasTitleDocs: Boolean = false, 
                               includeTargetFormatConfig: Boolean = false,
                               docUnderTestIsTitle: Boolean = false): Either[String, RootElement] = {
    val pathUnderTest = if (docUnderTestIsTitle) Root / "tree-2" / "README" else Root / "tree-2" / "doc-6"
    parseUnresolved(markup, pathUnderTest)
      .left.map(_.message)
      .flatMap { markupDoc => 
        val tree = buildTree(None, 
          Some(markupDoc.content.content),
          hasTitleDocs = hasTitleDocs, 
          includeTargetFormatConfig = includeTargetFormatConfig, 
          docUnderTestIsTitle = docUnderTestIsTitle
        )
        rewriteTree(tree, pathUnderTest)
      }
  }

}
