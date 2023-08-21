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

package laika.sbt

import laika.api.builder.BundleFilter
import laika.ast.MessageFilter
import laika.config.{ ConfigBuilder, ConfigEncoder, DefaultKey, Key }

import scala.io.Codec

/** A subset of the configuration API of the library that is tailored for use as a setting in the sbt plugin.
  *
  * It does not include those options for which there is a dedicated sbt setting for convenience, like the settings
  * for registering an AST rewrite rule, a custom renderer or an extension bundle.
  *
  * @author Jens Halm
  */
sealed abstract class LaikaConfig private {

  /** The file encoding to use for input and output files.
    */
  def encoding: Codec

  /** Indicates whether strict mode has been turned on for the target parser,
    * switching off any features not part of the original markup syntax.
    * This includes the registration of markup directives (custom tags)
    * as well as configuration sections at the start of the document.
    */
  def isStrict: Boolean

  /** Indicates whether all extensions that process raw content embedded into the host markup language should be enabled.
    * These are disabled by default as Laika is designed to render to multiple output formats from a single input document.
    * With raw content embedded the markup document is tied to a specific output format.
    */
  def acceptsRawContent: Boolean

  /** The configuration builder, populated with all values passed via `withConfigValue`.
    */
  def configBuilder: ConfigBuilder

  /** Indicates the filter to apply to runtime messages that should cause a transformation to fail.
    *
    * The default is to fail transformations on messages of level `Error` or higher.
    */
  def failOnMessages: MessageFilter

  /** Indicates the filter to apply to runtime messages that should get rendered to the output.
    *
    * The default is not to render any messages to the output.
    */
  def renderMessages: MessageFilter

  /** Indicates the filter to apply to runtime messages that should be logged to the console when running transformation tasks.
    *
    * The default is to log messages of level `Warning` or higher.
    */
  def logMessages: MessageFilter

  /** The file encoding to use for input and output files.
    */
  def encoding(codec: Codec): LaikaConfig

  /**  Turns strict mode on for the target parser, switching off any features not part of the original markup syntax.
    *  This includes the registration of markup directives (custom tags)
    *  as well as configuration sections at the start of the document.
    */
  def strict: LaikaConfig

  /**  Enables all extensions that process raw content embedded into the host markup language.
    *  These are disabled by default as Laika is designed to render to multiple output formats from a single input document.
    *  With raw content embedded the markup document is tied to a specific output format.
    */
  def withRawContent: LaikaConfig

  /** Specifies the filter to apply to runtime messages that should cause a transformation to fail.
    *
    * The default is to fail transformations on messages of level `Error` or higher.
    */
  def failOnMessages(filter: MessageFilter): LaikaConfig

  /** Specifies the filter to apply to runtime messages that should get rendered to the output.
    *
    * The default is not to render any messages to the output.
    */
  def renderMessages(filter: MessageFilter): LaikaConfig

  /** Specifies the filter to apply to runtime messages that should be logged to the console when running transformation tasks.
    *
    * The default is to log messages of level `Warning` or higher.
    */
  def logMessages(filter: MessageFilter): LaikaConfig

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder: DefaultKey](value: T): LaikaConfig

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder](key: String, value: T): LaikaConfig

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder](key: Key, value: T): LaikaConfig

  private[sbt] def bundleFilter: BundleFilter

}

object LaikaConfig {

  private final case class Impl(
      encoding: Codec,
      private[sbt] val bundleFilter: BundleFilter,
      configBuilder: ConfigBuilder,
      failOnMessages: MessageFilter,
      renderMessages: MessageFilter,
      logMessages: MessageFilter
  ) extends LaikaConfig {
    override def productPrefix = "LaikaConfig"

    private def copy(
        newEncoding: Codec = encoding,
        newBundleFilter: BundleFilter = bundleFilter,
        newConfigBuilder: ConfigBuilder = configBuilder,
        newFailOnMessages: MessageFilter = failOnMessages,
        newRenderMessages: MessageFilter = renderMessages,
        newLogMessages: MessageFilter = logMessages
    ): LaikaConfig = Impl(
      newEncoding,
      newBundleFilter,
      newConfigBuilder,
      newFailOnMessages,
      newRenderMessages,
      newLogMessages
    )

    val acceptsRawContent: Boolean = bundleFilter.acceptRawContent
    val isStrict: Boolean          = bundleFilter.strict

    def encoding(codec: Codec): LaikaConfig = copy(newEncoding = codec)
    def strict: LaikaConfig = copy(newBundleFilter = bundleFilter.copy(strict = true))

    def withRawContent: LaikaConfig =
      copy(newBundleFilter = bundleFilter.copy(acceptRawContent = true))

    def failOnMessages(filter: MessageFilter): LaikaConfig = copy(newFailOnMessages = filter)

    def renderMessages(filter: MessageFilter): LaikaConfig = copy(newRenderMessages = filter)

    def logMessages(filter: MessageFilter): LaikaConfig = copy(newLogMessages = filter)

    def withConfigValue[T: ConfigEncoder: DefaultKey](value: T): LaikaConfig =
      copy(newConfigBuilder = configBuilder.withValue(value))

    def withConfigValue[T: ConfigEncoder](key: String, value: T): LaikaConfig =
      copy(newConfigBuilder = configBuilder.withValue(key, value))

    def withConfigValue[T: ConfigEncoder](key: Key, value: T): LaikaConfig =
      copy(newConfigBuilder = configBuilder.withValue(key, value))

  }

  /** The default settings for the Laika plugin that can be modified with the methods of the returned instance.
    */
  def defaults: LaikaConfig = Impl(
    encoding = Codec.UTF8,
    bundleFilter = BundleFilter(),
    configBuilder = ConfigBuilder.empty,
    failOnMessages = MessageFilter.Error,
    renderMessages = MessageFilter.None,
    logMessages = MessageFilter.Warning
  )

  private def unapply(conf: LaikaConfig) = conf

}
