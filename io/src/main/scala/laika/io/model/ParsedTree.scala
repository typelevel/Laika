/*
 * Copyright 2012-2022 the original author or authors.
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

package laika.io.model

import laika.ast.{ DocumentTree, DocumentTreeRoot, Path, StaticDocument }

/** The result of a parsing operation for an entire document tree.
  *
  * The `DocumentTreeRoot` is the recursive structure of parsed inputs, like markup
  * document, templates or other file types, represented by their AST.
  *
  * The static documents are merely a sequence of unprocessed inputs that have
  * been discovered via directory scanning (or have been passed programmatically).
  * The actual processing of these inputs is left to the render step, which
  * might copy them into a target directory, or embed them into an output format
  * like EPUB.
  */
class ParsedTree[F[_]](val root: DocumentTreeRoot, val staticDocuments: Seq[BinaryInput[F]]) {

  /** Creates a new instance by applying the specified function to the root tree.
    */
  def modifyRoot(f: DocumentTreeRoot => DocumentTreeRoot): ParsedTree[F] =
    new ParsedTree(f(root), staticDocuments)

  /** Creates a new instance by applying the specified function to the nested tree.
    */
  def modifyTree(f: DocumentTree => DocumentTree): ParsedTree[F] =
    new ParsedTree(root.modifyTree(f), staticDocuments)

  /** Removes all static documents of this instance that match the specified filter.
    */
  def removeStaticDocuments(filter: Path => Boolean): ParsedTree[F] = new ParsedTree(
    root = root.copy(staticDocuments = root.staticDocuments.filterNot(doc => filter(doc.path))),
    staticDocuments = staticDocuments.filterNot(doc => filter(doc.path))
  )

  /** Removes all static documents of this instance and replaces them with the specified alternatives.
    */
  def replaceStaticDocuments(newStaticDocs: Seq[BinaryInput[F]]): ParsedTree[F] = new ParsedTree(
    root =
      root.copy(staticDocuments = newStaticDocs.map(doc => StaticDocument(doc.path, doc.formats))),
    staticDocuments = newStaticDocs
  )

  /** Adds the specified static documents to this instance.
    */
  def addStaticDocuments(newStaticDocs: Seq[BinaryInput[F]]): ParsedTree[F] = new ParsedTree(
    root = root.copy(staticDocuments =
      root.staticDocuments ++ newStaticDocs.map(doc => StaticDocument(doc.path, doc.formats))
    ),
    staticDocuments = staticDocuments ++ newStaticDocs
  )

}
