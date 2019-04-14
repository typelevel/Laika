/*
 * Copyright 2016 the original author or authors.
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

import laika.ast._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait TemplateRewriter {
  
  val defaultTemplate: TemplateDocument = TemplateDocument(Path.Root / "default.template", TemplateRoot(List(TemplateContextReference("document.content"))))
  
  /** Selects and applies the templates for the specified output format to all documents within the specified tree cursor recursively.
   */
  def applyTemplates (tree: DocumentTree, format: String): DocumentTree = applyTemplates(TreeCursor(tree), format)
  
  /** Selects and applies the templates for the specified output format to all documents within the specified tree cursor recursively.
   */
  def applyTemplates (cursor: TreeCursor, format: String): DocumentTree = {
    
    val newContent = cursor.children map {
      case doc: DocumentCursor => applyTemplate(doc, format)
      case tree: TreeCursor => applyTemplates(tree, format)
    }
      
    val newAdditionalContent = cursor.target.additionalContent map {
      case doc: TemplateDocument if doc.name.endsWith("."+format) => 
        val dynCursor = DocumentCursor.forEmptyDocument(doc.path.name.replace(".dynamic.", "."), cursor)
        val rewrittenDoc = applyTemplate(dynCursor, doc)
        DynamicDocument(rewrittenDoc.path, rewrittenDoc.content)
      case other => other
    }
    
    cursor.target.copy(
      content = newContent,
      templates = Nil,
      additionalContent = newAdditionalContent
    )
      
  }
  
  /** Selects and applies the template for the specified output format to the target of the specified document cursor.
    */ 
  def applyTemplate (cursor: DocumentCursor, format: String): Document = {
    val template = selectTemplate(cursor, format).getOrElse(defaultTemplate)
    applyTemplate(cursor, template)
  }
  
  def applyTemplate (cursor: DocumentCursor, template: TemplateDocument): Document = {
    val mergedConfig = cursor.config.withFallback(template.config).resolve
    val cursorWithMergedConfig = cursor.copy(
      config = mergedConfig,  
      resolver = ReferenceResolver.forDocument(cursor.target, cursor.parent, mergedConfig)
    )
    val newContent = rewriteRules(cursorWithMergedConfig).rewriteBlock(template.content)
    val newRoot = newContent match {
      case TemplateRoot(List(TemplateElement(root: RootElement, _, _)), _) => root
      case TemplateRoot(List(EmbeddedRoot(content, _, _)), _) => RootElement(content)
      case other => RootElement(Seq(other))
    }
    cursorWithMergedConfig.target.copy(content = newRoot)
  }
  
  /** The (optional) template to use when rendering the target of the specified document cursor.
   */  
  def selectTemplate (cursor: DocumentCursor, format: String): Option[TemplateDocument] = {
    import laika.bundle.ConfigImplicits._
    val config = cursor.config
    val templatePath = config.getRelativePath("template").orElse(config.getRelativePath(format + ".template"))

    templatePath match {
      case Some(path) =>
        cursor.root.target.selectTemplate(path)
          .orElse(throw new IllegalStateException(s"Template not found: $templatePath"))

      case None =>
        val filename = "default.template." + format

        @tailrec def templateForTree(tree: TreeCursor): Option[TemplateDocument] =
          (tree.target.selectTemplate(filename), tree.parent) match {
            case (None, Some(parent)) => templateForTree(parent)
            case (Some(template), _) => Some(template)
            case (None, None) => None
          }

        templateForTree(cursor.parent)
    }
  }
  
  /** The default rewrite rules for template documents,
   *  responsible for replacing all
   *  span and block resolvers with the final resolved
   *  element they produce based on the specified
   *  document cursor.
   */
  def rewriteRules (cursor: DocumentCursor): RewriteRules = {
    
    lazy val rules: RewriteRules = RewriteRules.forBlocks {
      case ph: BlockResolver                => Replace(rewriteBlock(ph resolve cursor))
      case TemplateRoot(spans, opt)         => Replace(TemplateRoot(format(spans), opt))
    } ++ RewriteRules.forSpans {
      case ph: SpanResolver                 => Replace(rewriteSpan(ph resolve cursor))
    } ++ RewriteRules.forTemplates {
      case ph: SpanResolver                 => Replace(rewriteTemplateSpan(asTemplateSpan(ph resolve cursor)))
      case TemplateSpanSequence(spans, opt) => Replace(TemplateSpanSequence(format(spans), opt))
    }
    
    def asTemplateSpan (span: Span) = span match {
      case t: TemplateSpan => t
      case s => TemplateElement(s)
    } 
    def rewriteBlock (block: Block): Block = rules.rewriteBlock(block)
    def rewriteSpan (span: Span): Span = rules.rewriteSpan(span)
    def rewriteTemplateSpan (span: TemplateSpan): TemplateSpan = rules.rewriteTemplateSpan(span)
    
    def format (spans: Seq[TemplateSpan]): Seq[TemplateSpan] = {
      def indentFor(text: String): Int = text.lastIndexOf('\n') match {
        case -1    => 0
        case index => if (text.drop(index).trim.isEmpty) text.length - index - 1 else 0
      }
      if (spans.isEmpty) spans
      else spans.sliding(2).foldLeft(new ListBuffer[TemplateSpan]() += spans.head) { 
        case (buffer, Seq(TemplateString(text, NoOpt), TemplateElement(elem, 0, opt))) => 
          buffer += TemplateElement(elem, indentFor(text), opt)
        case (buffer, Seq(TemplateString(text, NoOpt), EmbeddedRoot(elem, 0, opt))) =>
          buffer += EmbeddedRoot(elem, indentFor(text), opt)
        case (buffer, Seq(_, elem)) => buffer += elem
        case (buffer, _) => buffer
      }.toList
    }
    rules
  }
  
}

object TemplateRewriter extends TemplateRewriter
