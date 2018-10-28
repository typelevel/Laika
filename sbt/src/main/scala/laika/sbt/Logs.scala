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

package laika.sbt

import laika.ast._
import laika.io.InputTree
import sbt.Logger

/** Provides detailed logs for the tasks of the sbt plugin.
  *
  * @author Jens Halm
  */
object Logs {

  def s (num: Int): String = if (num == 1) "" else "s"

  /** Create a string containing detailed information about
    * the number and types of documents found in the specified input
    * tree.
    */
  def inputs (tree: InputTree): String = {

    def count (tree: InputTree): (Int, Int, Int, Int) = {
      val docs = tree.markupDocuments.length
      val tmpl = tree.dynamicDocuments.length + tree.templates.length
      val styles = tree.styleSheets.size
      val conf = tree.configDocuments.length
      val all = (tree.subtrees map count) :+ (docs, tmpl, styles, conf)
      all.foldLeft((0, 0, 0, 0)) {
        case ((d1, t1, s1, c1), (d2, t2, s2, c2)) => (d1 + d2, t1 + t2, s1 + s2, c1 + c2)
      }
    }

    val (docs, tmpl, styles, conf) = count(tree)

    s"Parsing $docs markup document${s(docs)}, $tmpl template${s(tmpl)}, $conf configuration${s(conf)} ..."
  }

  /** Create a string containing detailed information about
    * the number and types of documents processed for the specified
    * output tree.
    */
  def outputs (tree: DocumentTree, format: String): String = {

    def count (tree: DocumentTree): (Int, Int) = {

      val (render, copy) = tree.content.foldLeft((0,0)) {
        case ((render, copy), _: Document) => (render + 1, copy)
        case ((render, copy), tree: DocumentTree) =>
          val (childRender, childCopy) = count(tree)
          (render + childRender, copy + childCopy)
      }

      tree.additionalContent.foldLeft((render, copy)) {
        case ((render, copy), _: DynamicDocument) => (render + 1, copy)
        case ((render, copy), _: StaticDocument) => (render, copy + 1)
        case _ => (render, copy)
      }

    }

    val (render, copy) = count(tree)

    s"Rendering $render $format document${s(render)}, copying $copy static file${s(copy)} ..."
  }

  /** Logs all system messages found in the specified document tree that
    * have the given minimum log level.
    *
    * @param logger the logger to write to
    * @param tree the document tree to extract system messages from
    * @param level the minimum log level for a system message to be included in the log
    */
  def systemMessages (logger: Logger, tree: DocumentTree, level: MessageLevel): Unit = {

    def logMessage (inv: Invalid[_], path: Path): Unit = {
      val source = inv.fallback match {
        case tc: TextContainer => tc.content
        case other => other.toString
      }
      val text = s"$path: ${inv.message.content}\nsource: $source"
      inv.message.level match {
        // we do not log above warn level as the build will still succeed with invalid nodes
        case MessageLevel.Debug => logger.debug(text)
        case MessageLevel.Info => logger.info(text)
        case MessageLevel.Warning | MessageLevel.Error | MessageLevel.Fatal => logger.warn(text)
      }
    }

    def log (tree: DocumentTree): Unit = {

      def logRoot (e: ElementTraversal[_], path: Path) = {
        val nodes = e collect {
          case i: Invalid[_] if i.message.level >= level => i
        }
        nodes foreach { logMessage(_, path) }
      }

      tree.content foreach {
        case doc: Document => logRoot(doc.content, doc.path)
        case tree: DocumentTree => log(tree)
      }
      tree.additionalContent foreach {
        case doc: DynamicDocument => logRoot(doc.content, doc.path)
        case _ => ()
      }
    }

    log(tree)

  }

}
