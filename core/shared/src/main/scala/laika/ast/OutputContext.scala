/*
 * Copyright 2012-2023 the original author or authors.
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

package laika.ast

import laika.api.format.{ RenderFormat, TwoPhaseRenderFormat }

/** Describes the output for a render operation.
  *
  * The format selector is used by any configuration elements that allows to restrict
  * the output of documents to certain target formats.
  * It is not always identical to the fileSuffix used for the specific format.
  */
sealed abstract class OutputContext {

  /** The suffix to be used for file names for this output format.
    */
  def fileSuffix: String

  /** Identifier that matches configured formats in `TargetFormats`,
    * used to filter content for specific output formats only.
    *
    * @return
    */
  def formatSelector: String
}

object OutputContext {

  private final case class Impl(fileSuffix: String, formatSelector: String)
      extends OutputContext {
    override def productPrefix: String = "OutputContext"
  }

  private[laika] def apply(fileSuffix: String, formatSelector: String): OutputContext =
    Impl(fileSuffix, formatSelector)

  def apply(format: RenderFormat[_]): OutputContext =
    Impl(format.fileSuffix, format.description.toLowerCase)

  def apply(format: TwoPhaseRenderFormat[_, _]): OutputContext =
    Impl(format.interimFormat.fileSuffix, format.description.toLowerCase)

}
