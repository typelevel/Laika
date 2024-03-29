/*
 * Copyright 2012-2024 the original author or authors.
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

import laika.api.format.{ BinaryPostProcessor, RenderFormat, TwoPhaseRenderFormat }

/** Base trait for the configuration of renderers where the execution is not directly triggered by the user.
  * Examples for such a scenario are the preview server and the sbt plugin
  */
sealed abstract class RendererConfig private[config] {

  /** The alias that triggers the execution of this renderer when invoked from a command line.
    *
    * In the context of Laika's sbt plugin this would be in the format `laikaGenerate <alias>`.
    */
  def alias: String

  /** Indicates whether this renderer should be executed alongside the HTML renderer when a site is generated.
    *
    * In the context of Laika's sbt plugin this means when `true`` the renderer will execute when `laikaSite` is invoked,
    * and if `false` the renderer can only be invoked explicitly by using `laikaGenerate <alias>`.
    */
  def includeInSite: Boolean

}

/** Represents the configuration for a text renderer (of type `laika.api.format.RenderFormat`)
  * to be used with the `laikaGenerate` and `laikaSite` tasks.
  */
sealed abstract class TextRendererConfig private extends RendererConfig {

  /** The render format to be used with this renderer.
    */
  def format: RenderFormat[?]
}

/** Represents the configuration for a binary renderer
  * (of type `laika.api.format.TwoPhaseRenderFormat`)
  * to be used with the `laikaGenerate` and `laikaSite` tasks.
  */
sealed abstract class BinaryRendererConfig private extends RendererConfig {

  /** The binary render format to be used with this renderer.
    */
  def format: TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder]

  /** The artifact to be produced by this renderer.
    */
  def artifact: Artifact

  /** Indicates whether multiple different versions of the output with different
    * classifiers in their name should be written in case the user
    * has used the `@:select` directive in the input sources.
    *
    * For details about this directive, see
    * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html#select Select Directive]]
    * in the manual.
    */
  def supportsSeparations: Boolean
}

object TextRendererConfig {

  private final case class Impl(
      alias: String,
      format: RenderFormat[?],
      includeInSite: Boolean
  ) extends TextRendererConfig {
    override def productPrefix = "TextRendererConfig"
  }

  def apply(
      alias: String,
      format: RenderFormat[?],
      includeInSite: Boolean
  ): TextRendererConfig = Impl(
    alias,
    format,
    includeInSite
  )

}

object BinaryRendererConfig {

  private final case class Impl(
      alias: String,
      format: TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder],
      artifact: Artifact,
      includeInSite: Boolean,
      supportsSeparations: Boolean
  ) extends BinaryRendererConfig {
    override def productPrefix = "BinaryRendererConfig"
  }

  def apply(
      alias: String,
      format: TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder],
      artifact: Artifact,
      includeInSite: Boolean,
      supportsSeparations: Boolean
  ): BinaryRendererConfig = Impl(
    alias,
    format,
    artifact,
    includeInSite,
    supportsSeparations
  )

}
