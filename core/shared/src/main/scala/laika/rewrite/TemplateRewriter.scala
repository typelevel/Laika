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

import cats.implicits._
import laika.ast._
import laika.config.Origin.TemplateScope
import laika.config.{ConfigError, LaikaKeys, Origin, ValidationError}
import laika.parse.{LineSource, SourceCursor}
import laika.rewrite.ReferenceResolver.CursorKeys
import laika.rewrite.nav.Selections

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait TemplateRewriter {

  private val defaultTemplateRoot: TemplateRoot = {
    val src = s"$${${CursorKeys.documentContent}}"
    TemplateRoot(TemplateContextReference(CursorKeys.documentContent, required = true, LineSource(src, SourceCursor(src))))
  }
  
  private def defaultTemplate (format: String): TemplateDocument = 
    TemplateDocument(DefaultTemplatePath.forSuffix(format), defaultTemplateRoot)
  
  private def shouldRender (formatSelector: String)(content: Cursor): Boolean = 
    content.target.targetFormats.contains(formatSelector) 
  
  /** Selects and applies the templates for the specified output format to all documents 
    * within the specified tree cursor recursively.
   */
  def applyTemplates (tree: DocumentTreeRoot, context: OutputContext): Either[ConfigError, DocumentTreeRoot] = {
    
    for {
      cursor   <- RootCursor(tree, Some(context))
      newCover <- cursor.coverDocument
                    .filter(shouldRender(context.formatSelector))
                    .traverse(applyTemplate(_, context))
      newTree  <- applyTemplates(cursor.tree, context)
    } yield {
      cursor.target.copy(
        coverDocument = newCover,
        tree = newTree
      )
    }
    
  }
  
  private def applyTemplates (cursor: TreeCursor, context: OutputContext): Either[ConfigError, DocumentTree] = {

    for {
      newTitle   <- cursor.titleDocument.filter(shouldRender(context.formatSelector)).traverse(applyTemplate(_, context))
      newContent <- cursor.children.filter(shouldRender(context.formatSelector)).toList.traverse {
                      case doc: DocumentCursor => applyTemplate(doc, context)
                      case tree: TreeCursor    => applyTemplates(tree, context)
                    }
    } yield {
      cursor.target.copy(
        titleDocument = newTitle,
        content = newContent,
        templates = Nil
      )
    }
    
  }
  
  private def applyTemplate (cursor: DocumentCursor, context: OutputContext): Either[ConfigError, Document] =
    selectTemplate(cursor, context.fileSuffix)
      .map(_.getOrElse(defaultTemplate(context.fileSuffix)))
      .flatMap(applyTemplate(cursor, _))

  /** Applies the specified template to the target of the specified document cursor.
    */
  def applyTemplate (cursor: DocumentCursor, template: TemplateDocument): Either[ConfigError, Document] = {
    template.config.resolve(Origin(TemplateScope, template.path), cursor.config, cursor.root.target.includes).map { mergedConfig =>
      val cursorWithMergedConfig = cursor.copy(
        config = mergedConfig,
        resolver = ReferenceResolver.forDocument(cursor.target, cursor.parent, mergedConfig, cursor.position),
        templatePath = Some(template.path)
      )
      val newContent = rewriteRules(cursorWithMergedConfig).rewriteBlock(template.content)
      val newRoot = newContent match {
        case TemplateRoot(List(TemplateElement(root: RootElement, _, _)), _) => root
        case TemplateRoot(List(EmbeddedRoot(content, _, _)), _) => RootElement(content)
        case other => RootElement(other)
      }
      cursorWithMergedConfig.target.copy(content = newRoot, config = mergedConfig)
    }
  }
  
  private[laika] def selectTemplate (cursor: DocumentCursor, format: String): Either[ConfigError, Option[TemplateDocument]] = {
    val config = cursor.config
    val templatePath  =  config.getOpt[Path](LaikaKeys.template).flatMap {
      case None       => config.getOpt[Path](LaikaKeys.template(format))
      case Some(path) => Right(Some(path))
    }

    templatePath.flatMap {
      case Some(path) =>
        cursor.root.target.tree
          .selectTemplate(path.relative)
          .map(Some(_))
          .toRight(ValidationError(s"Template with path '$path' not found"))

      case None =>
        val templatePath = DefaultTemplatePath.forSuffix(format).relative
        @tailrec def templateForTree(tree: TreeCursor): Option[TemplateDocument] =
          (tree.target.selectTemplate(templatePath), tree.parent) match {
            case (None, Some(parent)) => templateForTree(parent)
            case (Some(template), _) => Some(template)
            case (None, None) => None
          }

        Right(templateForTree(cursor.parent))
    }
  }
  
  /** The default rewrite rules for template documents,
    * responsible for replacing all span and block resolvers with the final resolved element they produce 
    * based on the specified document cursor and its configuration.
    */
  def rewriteRules (cursor: DocumentCursor): RewriteRules = {

    lazy val rules: RewriteRules = RewriteRules.forBlocks {

      case ph: BlockResolver => Replace(rewriteBlock(ph.resolve(cursor)))

      case unresolved: Unresolved => Replace(InvalidBlock(unresolved.unresolvedMessage, unresolved.source))

      case nl: NavigationList if !nl.hasStyle("breadcrumb") => Replace(cursor.root.outputContext.fold(nl)(ctx => nl.forFormat(ctx.formatSelector)))

    } ++ RewriteRules.forSpans {

      case ph: SpanResolver => Replace(rewriteSpan(ph.resolve(cursor)))

      case unresolved: Unresolved => Replace(InvalidSpan(unresolved.unresolvedMessage, unresolved.source))

    } ++ RewriteRules.forTemplates {

      case ph: SpanResolver => Replace(rewriteTemplateSpan(asTemplateSpan(ph.resolve(cursor))))

      case unresolved: Unresolved => Replace(TemplateElement(InvalidSpan(unresolved.unresolvedMessage, unresolved.source)))
    } ++
      Selections.rewriteRules(cursor).getOrElse(RewriteRules.empty) ++
      TemplateFormatter(cursor).getOrElse(RewriteRules.empty)

    def asTemplateSpan (span: Span) = span match {
      case t: TemplateSpan => t
      case s => TemplateElement(s)
    }

    def rewriteBlock (block: Block): Block = rules.rewriteBlock(block)

    def rewriteSpan (span: Span): Span = rules.rewriteSpan(span)

    def rewriteTemplateSpan (span: TemplateSpan): TemplateSpan = rules.rewriteTemplateSpan(span)

    rules

  }
  
}

object TemplateRewriter extends TemplateRewriter

case class OutputContext (fileSuffix: String, formatSelector: String)

object OutputContext {
  def apply (format: String): OutputContext = apply(format, format)
}
