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

import cats.implicits.*
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.ast.*
import laika.config.{ ConfigError, LaikaKeys, ValidationError }
import laika.factory.{ RenderFormat, TwoPhaseRenderFormat }
import laika.parse.{ LineSource, SourceCursor }
import laika.rewrite.ReferenceResolver.CursorKeys

import scala.annotation.tailrec

private[laika] trait TemplateRewriter {

  private val defaultTemplateRoot: TemplateRoot = {
    val src = s"$${${CursorKeys.documentContent}}"
    TemplateRoot(
      TemplateContextReference(
        CursorKeys.documentContent,
        required = true,
        LineSource(src, SourceCursor(src))
      )
    )
  }

  private def defaultTemplate(format: String): TemplateDocument =
    TemplateDocument(DefaultTemplatePath.forSuffix(format), defaultTemplateRoot)

  private def shouldRender(formatSelector: String)(content: Cursor): Boolean =
    content.target.targetFormats.contains(formatSelector)

  /** Selects and applies the templates for the specified output format to all documents
    * within the specified tree cursor recursively.
    */
  def applyTemplates(
      tree: DocumentTreeRoot,
      rules: RewriteRulesBuilder,
      context: OutputContext
  ): Either[ConfigError, DocumentTreeRoot] = {
    for {
      cursor   <- RootCursor(tree, Some(context))
      newCover <- cursor.coverDocument
        .filter(shouldRender(context.formatSelector))
        .traverse(applyTemplate(_, rules, context))
      newTree  <- applyTemplates(cursor.tree, rules, context)
    } yield {
      cursor.target.copy(
        coverDocument = newCover,
        tree = newTree
      )
    }
  }

  private def applyTemplates(
      cursor: TreeCursor,
      rules: RewriteRulesBuilder,
      context: OutputContext
  ): Either[ConfigError, DocumentTree] = {
    for {
      newTitle   <- cursor.titleDocument.filter(shouldRender(context.formatSelector)).traverse(
        applyTemplate(_, rules, context)
      )
      newContent <- cursor.children.filter(shouldRender(context.formatSelector)).toList.traverse {
        case doc: DocumentCursor => applyTemplate(doc, rules, context)
        case tree: TreeCursor    => applyTemplates(tree, rules, context)
      }
    } yield {
      cursor.target.replaceContent(newContent).withTitleDocument(newTitle).withoutTemplates
    }
  }

  private def applyTemplate(
      cursor: DocumentCursor,
      rules: RewriteRulesBuilder,
      context: OutputContext
  ): Either[ConfigError, Document] = for {
    template <- selectTemplate(cursor, context.fileSuffix).map(
      _.getOrElse(defaultTemplate(context.fileSuffix))
    )
    doc      <- applyTemplate(cursor, rules, template)
  } yield doc

  /** Applies the specified template to the target of the specified document cursor.
    */
  def applyTemplate(
      cursor: DocumentCursor,
      rules: RewriteRulesBuilder,
      template: TemplateDocument
  ): Either[ConfigError, Document] = {
    cursor.applyTemplate(template).flatMap { mergedCursor =>
      rules(mergedCursor).map { docRules =>
        val newContent = docRules.rewriteBlock(template.content)
        val newRoot    = newContent match {
          case TemplateRoot(List(TemplateElement(root: RootElement, _, _)), _) => root
          case TemplateRoot(List(EmbeddedRoot(content, _, _)), _) => RootElement(content)
          case other                                              => RootElement(other)
        }
        mergedCursor.target.withContent(newRoot)
      }
    }
  }

  private[laika] def selectTemplate(
      cursor: DocumentCursor,
      format: String
  ): Either[ConfigError, Option[TemplateDocument]] = {
    val config       = cursor.config
    val templatePath = config.getOpt[Path](LaikaKeys.template).flatMap {
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
            case (Some(template), _)  => Some(template)
            case (None, None)         => None
          }

        Right(templateForTree(cursor.parent))
    }
  }

}

private[laika] object TemplateRewriter extends TemplateRewriter

/** Describes the output for a render operation.
  *
  * The format selector is used by any configuration elements that allows to restrict
  * the output of documents to certain target formats.
  * It is not always identical to the fileSuffix used for the specific format.
  */
case class OutputContext(fileSuffix: String, formatSelector: String)

object OutputContext {
  def apply(format: String): OutputContext = apply(format, format)

  def apply(format: RenderFormat[_]): OutputContext =
    apply(format.fileSuffix, format.description.toLowerCase)

  def apply(format: TwoPhaseRenderFormat[_, _]): OutputContext =
    apply(format.interimFormat.fileSuffix, format.description.toLowerCase)

}
