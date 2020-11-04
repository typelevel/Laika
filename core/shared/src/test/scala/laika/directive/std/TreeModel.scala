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
import laika.ast.sample.{BuilderKey, SampleConfig, SampleContent, SampleSixDocuments, SampleTrees}
import laika.ast.{Block, Document, DocumentTree, DocumentTreeRoot, Header, InternalLinkTarget, Path, RootElement, Style, Styles, TemplateDocument, TemplateSpan, Text}
import laika.config.{Config, ConfigBuilder, LaikaKeys, Origin}
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}

trait TreeModel extends TemplateParserSetup with MarkupParserSetup {

  import Path.Root

  def hasTitleDocs: Boolean = false

  def includeTargetFormatConfig: Boolean = false

  def targetFormats: SampleSixDocuments => SampleSixDocuments =
    if (!includeTargetFormatConfig) identity else _
      .doc2.config(SampleConfig.targetFormats())
      .doc3.config(SampleConfig.targetFormats("html", "txt"))
      .doc4.config(SampleConfig.targetFormats("epub", "pdf"))

  def buildTree (template: Option[(String, Seq[TemplateSpan])] = None, docUnderTest: Option[Seq[Block]] = None): DocumentTree = {
    def templateF = template.fold[SampleSixDocuments => SampleSixDocuments](identity) { 
      case (name, spans) => _.root.template(name, spans) 
    }
    val titleDocs: SampleSixDocuments => SampleSixDocuments = if (hasTitleDocs) _.titleDocuments(includeRoot = false) else identity // TODO - cleanup
    
    SampleTrees.sixDocuments
      .docContent(SampleContent.fourSections)
      .doc6.content(docUnderTest.getOrElse(SampleContent.fourSections(BuilderKey.Doc(6))))
      .tree1.config(SampleConfig.title("Tree 1"))
      .tree2.config(SampleConfig.title("Tree 2")) // TODO - generalize
      .apply(titleDocs)
      .apply(templateF)
      .apply(targetFormats)
      .build
      .tree
  }
  
  private def rewriteTree (inputTree: DocumentTree) = {
    val rules = OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree))
    val tree  = inputTree.rewrite(rules)
    TemplateRewriter
      .applyTemplates(DocumentTreeRoot(tree), TemplateContext("html"))
      .left.map(_.message)
      .flatMap { res =>
        res.tree
          .selectDocument(CurrentTree / "tree-2" / "doc-6")
          .map(_.content)
          .toRight("document under test missing")
      }
  }

  def parseTemplateAndRewrite (template: String): Either[String, RootElement] = {
    parseTemplate(template).flatMap { tRoot =>
      rewriteTree(buildTree(Some((DefaultTemplatePath.forHTML.name, tRoot.content))))
    }
  }

  def parseDocumentAndRewrite (markup: String): Either[String, RootElement] = {
    parseUnresolved(markup, Root / "sub2" / "doc6")
      .left.map(_.message)
      .flatMap { markupDoc => 
        rewriteTree(buildTree(None, Some(markupDoc.content.content)))
      }
  }

}
