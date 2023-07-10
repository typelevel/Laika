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

package laika.io.config

import laika.ast.Path
import laika.ast.Path.Root
import laika.config.Config.ConfigResult
import laika.config.{ Config, LaikaKeys }

/** Configuration for special folders in Laika's site output.
  *
  * @author Jens Halm
  */
private[laika] object SiteConfig {

  private val defaultDownloadPath: Path = Root / "downloads"
  private val defaultApiPath: Path      = Root / "api"

  /** The path within the virtual tree to place downloads (EPUB and PDF) in.
    */
  def downloadPath(config: Config): ConfigResult[Path] =
    config.get[Path](LaikaKeys.site.downloadPath, defaultDownloadPath)

  /** The path within the virtual tree where API documentation will be generated or copied into.
    */
  def apiPath(config: Config): ConfigResult[Path] =
    config.get[Path](LaikaKeys.site.apiPath, defaultApiPath)

}
