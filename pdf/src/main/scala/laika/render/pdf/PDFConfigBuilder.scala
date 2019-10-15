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

package laika.render.pdf

import laika.api.config.Config
import laika.format.PDF

/** Builds PDF configuration instances from configuration embedded in document trees.
  * 
  * @author Jens Halm
  */
object PDFConfigBuilder {

  /** Builds PDF configuration instances from configuration embedded in the specified document trees.
    */
  def fromTreeConfig (treeConfig: Config): PDF.Config = {
    val defaults = PDF.Config.default

    val res = for {
      bookmarkDepth <- treeConfig.get[Int]("pdf.bookmarks.depth", defaults.bookmarkDepth)
      tocDepth      <- treeConfig.get[Int]("pdf.toc.depth", defaults.tocDepth)
      tocTitle      <- treeConfig.getOpt[String]("pdf.toc.title")
    } yield {
      PDF.Config(bookmarkDepth, tocDepth, tocTitle.orElse(defaults.tocTitle))
    }

    res.toOption.getOrElse(defaults) // TODO - 0.12 - error handling
  }
  
}
