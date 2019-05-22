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

package laika.api

import laika.ast.Path
import laika.ast.Path.Root
import laika.config.OperationConfig
import laika.factory.{MarkupFormat, RenderFormat}

class Transformer (val parser: Parser, val renderer: Renderer) {

  def transform (input: String): String = transform(input, Root)

  def transform (input: String, path: Path): String = {
    val doc = parser.parse(input, path)
    renderer.render(doc)
  }

}

/** Serves as an entry point to the Transform API.
  *
  *  @author Jens Halm
  */
object Transform {

  /** Step in the setup for a transform operation where the
    *  renderer must be specified.
    */
  class Builder private[Transform] (parser: MarkupFormat, config: OperationConfig) {

    /** Creates and returns a new Transform instance for the specified renderer and the
      *  previously specified parser. The returned instance is stateless and reusable for
      *  multiple transformations.
      *
      *  @param format the render format to use for the transformation
      *  @return a new Transform instance
      */
    def to [FMT] (format: RenderFormat[FMT]): Transform[FMT] = new Transform(parser, format, config)

  }

  /** Returns a new Builder instance for the specified parser factory.
    *  This factory is usually an object provided by the library
    *  or a plugin that is capable of parsing a specific markup
    *  format like Markdown or reStructuredText. The returned builder
    *  can then be used to specifiy the renderer to create the actual
    *  Transform instance.
    *
    *  @param parser the parser factory to use
    *  @return a new Builder instance for specifying the renderer
    */
  def from (parser: MarkupFormat): Builder = new Builder(parser, OperationConfig.default.withBundlesFor(parser))

}