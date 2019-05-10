/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.render.epub

import com.typesafe.config.Config
import laika.ast.{DocumentMetadata, DocumentTree}
import laika.format.{EPUB, EPUB2}

/** Creates the EPUB configuration for a document tree.
  *
  * @author Jens Halm
  */
object ConfigFactory {

  /** Creates the EPUB configuration for the specified document tree.
    * It looks for `metadata` and `epub` sections in the config header
    * of the title document or in the `directory.conf` file in the root
    * directory, or uses defaults if both do not exist.
    */
  def forTree (tree: DocumentTree): EPUB.Config = {

    val defaults = EPUB.Config.default

    def getOpt [T](key: String, read: String => T): Option[T] =
      if (tree.config.hasPath(key)) Some(read(key)) else None

    val tocDepth = getOpt("epub.toc.depth", tree.config.getInt).getOrElse(defaults.tocDepth)
    val tocTitle = getOpt("epub.toc.title", tree.config.getString).orElse(defaults.tocTitle)
    val coverImage = getOpt("epub.coverImage", tree.config.getString).orElse(defaults.coverImage)

    val configForMetadata = tree.titleDocument.fold(tree.config)(_.config)
    val metadata = DocumentMetadata.fromConfig(configForMetadata)

    EPUB.Config(metadata, tocDepth, tocTitle, coverImage)
  }

  def forTreeConfig (config: Config): EPUB2.Config = {

    val defaults = EPUB2.Config.default

    def getOpt [T](key: String, read: String => T): Option[T] =
      if (config.hasPath(key)) Some(read(key)) else None

    val tocDepth = getOpt("epub.toc.depth", config.getInt).getOrElse(defaults.tocDepth)
    val tocTitle = getOpt("epub.toc.title", config.getString).orElse(defaults.tocTitle)
    val coverImage = getOpt("epub.coverImage", config.getString).orElse(defaults.coverImage)

    val metadata = DocumentMetadata.fromConfig(config)

    EPUB2.Config(metadata, tocDepth, tocTitle, coverImage)
  }

}
