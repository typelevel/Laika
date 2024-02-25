package laika.sbt

import laika.api.format.{ BinaryPostProcessor, RenderFormat, TwoPhaseRenderFormat }

import java.io.File

/** Base trait for the configuration of renderers to be used with
  * the `laikaGenerate` and `laikaSite` tasks.
  */
sealed abstract class RendererConfig private[sbt] {

  /** The alias that triggers the execution of this renderer when
    * passed to the `laikaGenerate` task in the format `laikaGenerate <alias>`.
    */
  def alias: String

  /** Indicates whether this renderer should be executed when the `laikaSite` task is run.
    * When set to false, the renderer can still get executed by using `laikaGenerate <alias>`.
    */
  def includeInSite: Boolean

  /** The target directory the file(s) should be written into.
    */
  def targetDirectory: File
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

  /** The base name of the output file.
    *
    * The full path of the generated file will be
    * `<targetDirectory>/<artifactBaseName><classifiers>.<fileSuffix>`
    * where `classifiers` will be empty when `supportsSeparations` is `false`
    * or the user did not use the `@:select` directive.
    */
  def artifactBaseName: String

  /** The file suffix (without dot) for the output file.
    */
  def fileSuffix: String

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
      targetDirectory: File,
      includeInSite: Boolean
  ) extends TextRendererConfig {
    override def productPrefix = "TextRendererConfig"
  }

  def apply(
      alias: String,
      format: RenderFormat[?],
      targetDirectory: File,
      includeInSite: Boolean
  ): TextRendererConfig = Impl(
    alias,
    format,
    targetDirectory,
    includeInSite
  )

}

object BinaryRendererConfig {

  private final case class Impl(
      alias: String,
      format: TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder],
      targetDirectory: File,
      artifactBaseName: String,
      fileSuffix: String,
      includeInSite: Boolean,
      supportsSeparations: Boolean
  ) extends BinaryRendererConfig {
    override def productPrefix = "BinaryRendererConfig"
  }

  def apply(
      alias: String,
      format: TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder],
      targetDirectory: File,
      artifactBaseName: String,
      fileSuffix: String,
      includeInSite: Boolean,
      supportsSeparations: Boolean
  ): BinaryRendererConfig = Impl(
    alias,
    format,
    targetDirectory,
    artifactBaseName,
    fileSuffix,
    includeInSite,
    supportsSeparations
  )

}
