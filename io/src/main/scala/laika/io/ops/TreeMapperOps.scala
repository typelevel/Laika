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

package laika.io.ops

import cats.Monad
import cats.syntax.all.*
import laika.ast.{ Document, DocumentTree, TreeContent }
import laika.io.model.ParsedTree

/** Operations for builders of transformers that allow to modify the tree
  * or documents between parsing and rendering.
  *
  * This hook differs from the rewrite rules in two ways: first it does not
  * only operate on the AST nodes of a document, therefore it can also change
  * its path or config. Secondly it allows for effectful transformations
  * which are not available in the pure laika-core module.
  *
  * @author Jens Halm
  */
private[laika] abstract class TreeMapperOps[F[_]: Monad] {

  type MapRes

  /** Creates a new transformer that applies the specified function to each document in the parsed tree
    * before rendering.
    */
  def mapDocuments(f: Document => Document): MapRes = evalMapDocuments(
    f.andThen(Monad[F].pure)
  )

  /** Creates a new transformer that applies the specified effectful function to each document in the parsed tree
    * before rendering.
    */
  def evalMapDocuments(f: Document => F[Document]): MapRes = evalMapTree { parsed =>
    def mapTree(tree: DocumentTree): F[DocumentTree] = for {
      newContent <- tree.content.toList.traverse {
        case doc: Document      => f(doc).widen[TreeContent]
        case tree: DocumentTree => mapTree(tree).widen[TreeContent]
      }
      newTitle   <- tree.titleDocument.traverse(f)
    } yield tree.replaceContent(newContent).withTitleDocument(newTitle)

    for {
      mappedTree  <- mapTree(parsed.root.tree)
      mappedCover <- parsed.root.coverDocument.traverse(f)
    } yield parsed.modifyRoot(_.copy(tree = mappedTree, coverDocument = mappedCover))

  }

  /** Creates a new transformer that applies the specified function to the parsed tree
    * before rendering.
    */
  def mapTree(f: ParsedTree[F] => ParsedTree[F]): MapRes = evalMapTree(
    f.andThen(Monad[F].pure)
  )

  /** Creates a new transformer that applies the specified effectful function to the parsed tree
    * before rendering.
    */
  def evalMapTree(f: ParsedTree[F] => F[ParsedTree[F]]): MapRes

}
