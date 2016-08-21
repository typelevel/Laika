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

import laika.tree.Documents2.Document
import laika.tree.Documents2.DocumentTree
import laika.tree.Documents2.DynamicTemplate
import laika.tree.Elements.RootElement
import laika.tree.Templates
import laika.tree.Templates.EmbeddedRoot
import laika.tree.Templates.TemplateDocument
import laika.tree.Templates.TemplateElement
import laika.tree.Templates.TemplateRoot
import laika.tree.Paths.Path
import laika.tree.Paths.Root
import com.typesafe.config.Config
import scala.annotation.tailrec

trait TemplateRewriter {
  
  /** Selects and applies the templates for the specified output format to all documents within the specified tree cursor recursively.
   */
  def applyTemplates (cursor: TreeCursor, format: String): DocumentTree = {
    
    val newContent = cursor.children map {
      case doc: DocumentCursor => applyTemplate(doc, format)
      case tree: TreeCursor => applyTemplates(tree, format)
    }
      
    val newAdditionalContent = cursor.target.additionalContent map {
      case doc: DynamicTemplate if doc.name.endsWith("."+format) => 
        val emptyDoc = Document(doc.path.parent / doc.path.name.replace(".dynamic.", "."), RootElement(Nil))
        val dynCursor = DocumentCursor(emptyDoc, cursor)
        /* applyTemplate(dynCursor, doc) */ // TODO - after merge: use TemplateDocument instead of DynamicTemplate 
        doc
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
    val template = selectTemplate(cursor, format).getOrElse(null) // TODO - after merge: get defaultTemplate
    applyTemplate(cursor, template)
  }
  
  def applyTemplate (cursor: DocumentCursor, template: TemplateDocument): Document = {
    val cursorWithMergedConfig = cursor.copy(
      config = cursor.config.withFallback(template.config)  
    )
    val newContent = template.content rewrite Templates.rewriteRules(null) // TODO - after merge: pass cursor
    val newRoot = newContent match {
      case TemplateRoot(List(TemplateElement(root: RootElement, _, _)), _) => root
      case TemplateRoot(List(EmbeddedRoot(content, _, _)), _) => RootElement(content)
      case other => RootElement(Seq(other))
    }
    cursorWithMergedConfig.target.copy(content = newRoot)
  }
  
  // TODO - after merge: move rewriteRules method here, use TemplateDocument instead of DynamicTemplate and move it to Documents
  
  /** The (optional) template to use when rendering the target of the specified document cursor.
   */  
  def selectTemplate (cursor: DocumentCursor, format: String): Option[TemplateDocument] = {
    val config = cursor.config
    if (config.hasPath("template") || config.hasPath(format + ".template")) {
      val key = if (config.hasPath(format + ".template")) format+".template" else "template" 
      val value = config.getValue(key)
      val desc = value.origin().description()
      val basePath = if (desc.startsWith("path:")) Path(desc.take(desc.lastIndexOf(":")).drop(5)).parent else Root
      val templatePath = (basePath / Path(value.unwrapped().toString)).relativeTo(Root)
      cursor.root.target.selectTemplate(templatePath).orElse(throw new IllegalStateException(s"Template not found: $templatePath"))
    }
    else {
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
  
}

object TemplateRewriter extends TemplateRewriter
