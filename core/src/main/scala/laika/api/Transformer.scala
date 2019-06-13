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

import laika.api.builder.{OperationConfig, TransformerBuilder, TwoPhaseTransformerBuilder}
import laika.ast.Path
import laika.ast.Path.Root
import laika.factory.{MarkupFormat, RenderFormat, TwoPhaseRenderFormat}

class Transformer (val parser: MarkupParser, val renderer: Renderer) {

  def transform (input: String): String = transform(input, Root)

  def transform (input: String, path: Path): String = {
    val doc = parser.parse(input, path)
    renderer.render(doc)
  }

}

/** Serves as an entry point for building a Transformer instance.
  *
  *  @author Jens Halm
  */
object Transformer {

  /** Step in the setup for a transform operation where the
    *  renderer must be specified.
    */
  class Builder private[Transformer] (parser: MarkupFormat, config: OperationConfig) {

    /** Creates and returns a new builder instance for the specified renderer and the
      *  previously specified parser.
      *
      *  @param format the render format to use for the transformation
      *  @return a new builder instance for a Transformer
      */
    def to [FMT] (format: RenderFormat[FMT]): TransformerBuilder[FMT] = new TransformerBuilder(parser, format, config)

    /** Returns a new builder instance for the specified two-phase render format and the
      * previously specified parser.
      *
      * The format is usually an object provided by the library
      * or a plugin that is capable of producing a specific output format like EPUB or PDF.
      *
      * While the builder API is defined as part of the laika-core module, the concrete
      * implementations of this renderer type that this library provides (EPUB and PDF) 
      * reside in sub-modules as they require the functionality of the laika-io module.
      */
    def to [FMT, PP] (format: TwoPhaseRenderFormat[FMT, PP]): TwoPhaseTransformerBuilder[FMT, PP] =
      new TwoPhaseTransformerBuilder[FMT, PP](parser, format, config)

  }

  /** Returns a new Builder instance for the specified markup format.
    * 
    *  This factory is usually an object provided by the library
    *  or a plugin that is capable of parsing a specific markup
    *  format like Markdown or reStructuredText. The returned builder
    *  can then be used to specify the render format to create the actual
    *  builder instance.
    *
    *  @param format the markup format to use
    *  @return a new Builder instance for specifying the render format
    */
  def from (format: MarkupFormat): Builder = new Builder(format, OperationConfig.default.withBundlesFor(format))

}