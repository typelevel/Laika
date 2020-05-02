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

import laika.config.Config.ConfigResult
import laika.config.{ASTValue, Config, ConfigBuilder, ConfigValue, Field, Key, ObjectValue, StringValue}
import laika.ast.{Document, DocumentTree, Path, SpanSequence, TreeCursor, TreePosition}

/** A resolver for context references in templates or markup documents.
 *  
 *  @author Jens Halm
 */
case class ReferenceResolver (config: Config) {
  
  def resolve (key: Key): ConfigResult[Option[ConfigValue]] = config.getOpt[ConfigValue](key)
  
}

/** Companion for constructing ReferenceResolvers for a particular
 *  target Document.
 */
object ReferenceResolver {
  
  object CursorKeys {
    
    val documentContent = Key("cursor", "currentDocument", "content")
    val documentTitle = Key("cursor", "currentDocument", "title")
    
    def fragment (name: String): Key = Key("cursor", "currentDocument", "fragments", name)
    
  }
  
  private val emptyTitle: SpanSequence = SpanSequence.empty

  // cannot use existing cursor-base sibling navigation here as the cursor hierarchy is under construction when this is called
  private class Siblings (documents: Vector[Document], refPath: Path) {
    val currentIndex: Int = documents.indexWhere(_.path == refPath)
    def previousDocument: Option[Document] = if (currentIndex <= 0) None else Some(documents(currentIndex - 1))
    def nextDocument: Option[Document] = 
      if (documents.isEmpty || currentIndex + 1 == documents.size) None else Some(documents(currentIndex + 1))
  }
  
  /** Creates a new ReferenceResolver for the specified document and its parent and configuration.
   */
  def forDocument(document: Document, parent: TreeCursor, config: Config, position: TreePosition): ReferenceResolver = {

    val rootKey = Key("cursor")
    
    val baseBuilder = ConfigBuilder
      .withFallback(config)
      .withValue(rootKey.child("currentDocument"), ObjectValue(Seq(
        Field("path", StringValue(document.path.toString)),
        Field("content", ASTValue(document.content), config.origin),
        Field("title", ASTValue(document.title.getOrElse(emptyTitle)), config.origin),
        Field("fragments", ObjectValue(document.fragments.toSeq.map {
          case (name, element) => Field(name, ASTValue(element), config.origin)
        }), config.origin)
      )))
      .withValue(rootKey.child("root"), ObjectValue(Seq(
        Field("title", ASTValue(parent.root.target.title.getOrElse(emptyTitle)))
      )))

    def collectSiblings (tree: DocumentTree): Vector[Document] = tree.content.toVector.flatMap { 
      case d: Document => Some(d)
      case t: DocumentTree => t.titleDocument
    }
    
    val flattenedSiblings = new Siblings(parent.root.target.allDocuments.toVector, document.path)
    val hierarchicalSiblings =
      if (parent.target.titleDocument.map(_.path).contains(document.path))
        new Siblings(parent.parent.map(_.target).toVector.flatMap(collectSiblings), document.path)
      else 
        new Siblings(collectSiblings(parent.target), document.path)

    def addDocConfig (key: Key, doc: Option[Document])(builder: ConfigBuilder): ConfigBuilder =
      doc.fold(builder) { doc =>
        builder.withValue(key, ObjectValue(Seq(
          Field("absolutePath", StringValue(doc.path.toString)),
          Field("relativePath", StringValue(doc.path.relativeTo(document.path).toString)),
          Field("title", ASTValue(doc.title.getOrElse(emptyTitle)))
        )))
      }
    
    val addSiblings = (addDocConfig(rootKey.child("parentDocument"), parent.target.titleDocument)(_))
      .andThen(addDocConfig(rootKey.child("previousDocument"), hierarchicalSiblings.previousDocument))
      .andThen(addDocConfig(rootKey.child("nextDocument"), hierarchicalSiblings.nextDocument))
      .andThen(addDocConfig(rootKey.child(Key("flattenedSiblings", "previousDocument")), flattenedSiblings.previousDocument))
      .andThen(addDocConfig(rootKey.child(Key("flattenedSiblings", "nextDocument")), flattenedSiblings.nextDocument))

    apply(addSiblings(baseBuilder).build)
  }
  
}
