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

package laika.api

import laika.api.builder.{OperationConfig, RendererBuilder}
import laika.factory.RenderFormat

@deprecated("use Renderer instead", "0.12.0")
object Render {

  @deprecated("use Renderer.of(...) instead", "0.12.0")
  def as [FMT] (format: RenderFormat[FMT]): RendererBuilder[FMT] = 
    new RendererBuilder[FMT](format, OperationConfig.default)
  
}
