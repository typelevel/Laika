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

package laika.render.epub

import laika.config.Config
import laika.ast.{DocumentMetadata, Path}
import laika.config.Config.ConfigResult
import laika.format.EPUB

/** Creates the EPUB configuration for a document tree.
  *
  * @author Jens Halm
  */
object ConfigFactory {

  /** Creates the EPUB configuration for the specified document tree configuration.
    * It looks for `metadata` and `epub` sections in the config header
    * of the title document or in the `directory.conf` file in the root
    * directory, or uses defaults if both do not exist.
    */
  def forTreeConfig (config: Config): ConfigResult[EPUB.Config] = {

    for {
      tocDepth   <- config.get[Int]("epub.toc.depth", EPUB.Config.default.tocDepth)
      tocTitle   <- config.getOpt[String]("epub.toc.title")
      coverImage <- config.getOpt[Path]("epub.coverImage")
      metadata   <- DocumentMetadata.fromConfig(config)
    } yield {
      EPUB.Config(metadata, tocDepth, tocTitle, coverImage)
    }
  }

}
