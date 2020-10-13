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

/**
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
  
  lazy val templateInclude: Templates.Directive = Templates.eval("include") {

    import Templates.dsl._

    (attribute(0).as[Path], allAttributes, cursor).mapN { case (path, attributes, cursor) =>
      resolveTemplateReference(path, attributes, cursor)
    }
  }

  lazy val templateEmbed: Templates.Directive = Templates.eval("embed") {

    import Templates.dsl._

    (attribute(0).as[Path], allAttributes, parsedBody, cursor).mapN { case (path, attributes, body, cursor) =>
      resolveTemplateReference(path, attributes, cursor, Some(body))
    }
  }

  lazy val blockInclude: Blocks.Directive = Blocks.eval("include") {

    import Blocks.dsl._

    (attribute(0).as[Path], allAttributes, cursor).mapN { case (path, attributes, cursor) =>
      resolveDocumentReference(path, attributes, cursor)
    }
  }

  lazy val blockEmbed: Blocks.Directive = Blocks.eval("embed") {

    import Blocks.dsl._

    (attribute(0).as[Path], allAttributes, parsedBody, cursor).mapN { case (path, attributes, body, cursor) =>
      resolveDocumentReference(path, attributes, cursor, Some(body))
    }
  }
  
}
