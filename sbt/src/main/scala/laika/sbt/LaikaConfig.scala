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
import laika.ast.MessageLevel
import laika.config.{ConfigBuilder, ConfigEncoder, DefaultKey}

import scala.io.Codec

/** A subset of the configuration API of the library that is tailored for use as a setting in the sbt plugin.
  * 
  * It does not include those options for which there is a dedicated sbt setting for convenience, like the settings
  * for registering an AST rewrite rule, a custom renderer or an extension bundle.
  * 
  * @author Jens Halm
  */
case class LaikaConfig(encoding: Codec = Codec.UTF8,
                       bundleFilter: BundleFilter = BundleFilter(),
                       configBuilder: ConfigBuilder = ConfigBuilder.empty,
                       renderMessageLevel: MessageLevel = MessageLevel.Warning,
                       logMessageLevel: MessageLevel = MessageLevel.Warning) {

  /** The file encoding to use for input and output files.
    */
  def encoding (codec: Codec): LaikaConfig = copy(encoding = codec)
  
  /**  Turns strict mode on for the target parser, switching off any features not part of the original markup syntax.
    *  This includes the registration of directives (custom tags), custom templates with directives, 
    *  as well as configuration sections at the start of the document.
    *
    *  Technically it removes all `ExtensionBundle` instances which do not have the `useInStrictMode` flag set to true.
    */
  def strict: LaikaConfig = copy(bundleFilter = bundleFilter.copy(strict = true))

  /**  Enables all extensions that process raw content embedded into the host markup language.
    *  These are disabled by default as Laika is designed to render to multiple output formats from a single input document. 
    *  With raw content embedded the markup document is tied to a specific output format.
    *
    *  Technically it activates all `ExtensionBundle` instances which have the `acceptRawContent` flag set to true.
    */
  def withRawContent: LaikaConfig = copy(bundleFilter = bundleFilter.copy(acceptRawContent = true))

  /**  Specifies the minimum required level for a runtime message to get included into the output by this renderer.
    */
  def renderMessageLevel (level: MessageLevel): LaikaConfig = copy(renderMessageLevel = level)

  /**  Specifies the minimum required level for a runtime message to be logged to the console when running transformation tasks.
    */
  def logMessageLevel (level: MessageLevel): LaikaConfig = copy(logMessageLevel = level)
  
  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles, 
    * but lower precedence than any value with the same key specified in a configuration file for a directory 
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder: DefaultKey](value: T): LaikaConfig = copy(configBuilder = configBuilder.withValue(value))

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles, 
    * but lower precedence than any value with the same key specified in a configuration file for a directory 
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder](key: String, value: T): LaikaConfig = copy(configBuilder = configBuilder.withValue(key, value))
  
}

object LaikaConfig {

  /** The default settings for the Laika plugin that can be modified with the methods of the returned instance.
    */
  def defaults: LaikaConfig = LaikaConfig()
  
}
