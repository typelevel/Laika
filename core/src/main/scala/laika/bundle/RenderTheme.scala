/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.bundle

import laika.ast._
import laika.collection.TransitionalCollectionOps._

/** Collects templates, styles and custom render functions to form
  * a theme for a specific output format.
  *
  * @author Jens Halm
  */
trait RenderTheme {

  /** The type of the Writer API a custom render function
    * for this theme uses.
    */
  type Writer

  /** Specifies a custom render function that overrides one or more of the default
    * renderers for the output format this instance uses.
    *
    * This method expects a function that returns a partial function as the result.
    * The outer function allows to capture the writer instance to write to and will
    * only be invoked once. The partial function will then be invoked for each
    * element it is defined at.
    */
  def customRenderer: Writer => RenderFunction

  /** The default template to embed the nodes of the parsed markup documents in,
    * in case no user-defined template overwrites the default.
    */
  def defaultTemplate: Option[TemplateRoot]

  /** The default styles to apply in addition to any user-defined styles.
    *
    * These styles are only processed for output formats where the transformer
    * processes the CSS to adjust the rendered output. This is only the case
    * for PDF and XSL-FO.
    *
    * Styling for other formats like HTML has to happen via static files
    * in the input directory that are merely copied over to the target directory
    * by the transformer.
    */
  def defaultStyles: StyleDeclarationSet

  /** Specifies any additional static documents to be copied over to the
    * target directory in each transform operation.
    *
    * This is allows for a central collection of CSS, JavaScript, image and
    * other files needed for a specific theme.
    */
  def staticDocuments: StaticDocuments

  /** Returns the default template specified by this theme or the
    * system-wide default in case the default is empty.
    */
  def defaultTemplateOrFallback: TemplateRoot = defaultTemplate.getOrElse(TemplateRoot.fallback)

}

/** A collection of static documents to be copied over to the
  * target directory in each transform operation.
  *
  * This is allows for a central collection of CSS, JavaScript, image and
  * other files needed for a specific theme.
  */
case class StaticDocuments (tree: DocumentTree) {

  /** Merges this instance with the specified base recursively.
    */
  def merge (base: DocumentTree): DocumentTree = {

    def mergeContent (content: Seq[TreeContent]): Seq[TreeContent] = {
      val trees = content.collect{ case t: DocumentTree => t }.groupBy(_.path).mapValuesStrict(_.reduceLeft(mergeTrees)).values.toList
      (content.filter(_.isInstanceOf[Document]) ++ trees).sortBy(_.position)
    }

    def mergeTrees (left: DocumentTree, right: DocumentTree): DocumentTree = {
      right.copy(
        content = mergeContent(left.content ++ right.content),
        additionalContent = left.additionalContent ++ right.additionalContent
      )
    }

    mergeTrees(tree, base)

  }

}

/** API for defining a collection of static documents
  * based on one or more directories.
  *
  * Like with all Laika IO, these may be actual file system directories
  * or virtual in-memory trees of input documents.
  */
object StaticDocuments {

  val empty = StaticDocuments(DocumentTree(Path.Root, Nil))

  // TODO - 0.12 - should now just be a `Seq[BinaryInput]`
  
//  override type InputTreeResult = StaticDocuments
//
//  override def config: OperationConfig = OperationConfig(Seq(new ExtensionBundle {
//    override def docTypeMatcher: PartialFunction[Path, DocumentType] = { case _ => DocumentType.Static }
//  }))
//
//  override def fromTreeInput (input: TreeInput): StaticDocuments = {
//    val static = input.binaryInputs map StaticDocument
//    def buildTree (path: Path, content: Seq[StaticDocument], subTrees: Seq[DocumentTree]): DocumentTree =
//      DocumentTree(path, subTrees, additionalContent = content, sourcePaths = input.sourcePaths)
//    StaticDocuments(TreeBuilder.build(static, buildTree))
//  }

}
