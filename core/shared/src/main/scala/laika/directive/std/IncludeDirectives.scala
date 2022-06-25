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
import laika.ast.{Block, BlockSequence, DocumentCursor, Element, Path, TemplateSpan, TemplateSpanSequence}
import laika.config.{ASTValue, Config, Field, ObjectConfig, ObjectValue, Origin}
import laika.directive.{Blocks, Templates}
import laika.rewrite.TemplateRewriter

/** Provides the implementation for the standard include and embed directives.
  *
  * This includes the template and markup-block variants of these directives, 
  * which allow to embed one template or markup document inside another.
  * 
  * For full documentation see the section about
  * [[https://planet42.github.io/Laika/07-reference/01-standard-directives.html#inclusions Include and Embed Directives]]
  * in the manual.
  * 
  * @author Jens Halm
  */
object IncludeDirectives {

  private def config (attributes: Config, body: Option[Element], path: Path): ObjectValue = {
    val attributeValues = attributes match {
      case oc: ObjectConfig => oc.root.values
      case _ => Nil
    }
    val bodyValue = body.map(b => Field("embeddedBody", ASTValue(b), Origin(Origin.DirectiveScope, path)))
    ObjectValue(attributeValues ++ bodyValue.toSeq)
  }

  private def resolvePath (literalPath: Path,
                           pathConfigKey: String,
                           config: Config): Either[String, Path] =
    if (config.hasKey(pathConfigKey)) config.get[Path](pathConfigKey).leftMap(_.message)
    else Right(literalPath)
  
  private def resolveTemplateReference (path: Path, 
                                        attributes: Config, 
                                        cursor: DocumentCursor, 
                                        body: Option[Seq[TemplateSpan]] = None): Either[String, TemplateSpan] = {
    cursor.root.target.tree.selectTemplate(path.relative)
      .map { doc =>
        val context = config(attributes, body.map(TemplateSpanSequence(_)), cursor.templatePath.getOrElse(cursor.path))
        val rules = TemplateRewriter.rewriteRules(cursor.withReferenceContext(context))
        TemplateSpanSequence(doc.content.rewriteChildren(rules).content)
      }
      .toRight(s"Unresolved reference to template '${path.toString}'")
  }

  private def resolveDocumentReference (path: Path, 
                                        attributes: Config, 
                                        cursor: DocumentCursor, 
                                        body: Option[Seq[Block]] = None): Either[String, Block] = {
    cursor.root.target.tree.selectDocument(path.relative)
      .map { doc =>
        val context = config(attributes, body.map(BlockSequence(_)), cursor.path)
        val rules = TemplateRewriter.rewriteRules(cursor.withReferenceContext(context))
        BlockSequence(doc.content.rewriteChildren(rules).content)
      }
      .toRight(s"Unresolved reference to template '${path.toString}'")
  }

  /** Implementation of the `include` directive for templates.
    */
  lazy val templateInclude: Templates.Directive = Templates.eval("include") {

    import Templates.dsl._

    (attribute(0).as[Path], attribute(0).as[String], allAttributes, cursor).mapN { 
      case (literalPath, pathKey, attributes, cursor) =>
        resolvePath(literalPath, pathKey, cursor.resolver.config)
          .flatMap(resolveTemplateReference(_, attributes, cursor))
    }
  }

  /** Implementation of the `embed` directive for templates.
    */
  lazy val templateEmbed: Templates.Directive = Templates.eval("embed") {

    import Templates.dsl._

    (attribute(0).as[Path], attribute(0).as[String], allAttributes, parsedBody, cursor).mapN { 
      case (literalPath, pathKey, attributes, body, cursor) =>
        resolvePath(literalPath, pathKey, cursor.resolver.config)
          .flatMap(resolveTemplateReference(_, attributes, cursor, Some(body)))
    }
  }

  /** Implementation of the `include` directive for text markup documents.
    */
  lazy val blockInclude: Blocks.Directive = Blocks.eval("include") {

    import Blocks.dsl._

    (attribute(0).as[Path], attribute(0).as[String], allAttributes, cursor).mapN { 
      case (literalPath, pathKey, attributes, cursor) =>
        resolvePath(literalPath, pathKey, cursor.resolver.config)
          .flatMap(resolveDocumentReference(_, attributes, cursor))
    }
  }

  /** Implementation of the `embed` directive for text markup documents.
    */
  lazy val blockEmbed: Blocks.Directive = Blocks.eval("embed") {

    import Blocks.dsl._

    (attribute(0).as[Path], attribute(0).as[String], allAttributes, parsedBody, cursor).mapN { 
      case (literalPath, pathKey, attributes, body, cursor) =>
        resolvePath(literalPath, pathKey, cursor.resolver.config)
          .flatMap(resolveDocumentReference(_, attributes, cursor, Some(body)))
    }
  }
  
}
