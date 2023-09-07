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

package laika.theme.config

/** Configuration API for specifying attributes to set for script tags
  * in conjunction with the Helium API for script includes.
  */
sealed abstract class ScriptAttributes private {

  def isDefer: Boolean
  def isAsync: Boolean
  def isModule: Boolean

  /** The value to be set for the `integrity` attribute.
    * If empty the attribute will not be included.
    */
  def integrity: Option[String]

  /** The value to be set for the `crossorigin` attribute.
    * In case of `CrossOrigin.Unspecified` the attribute will not be included.
    */
  def crossOrigin: CrossOrigin

  /** Indicates that the `defer` attribute should be included in the script tag.
    * Overrides `async` in case that had been set previously.
    */
  def defer: ScriptAttributes

  /** Indicates that the `async` attribute should be included in the script tag.
    * Overrides `defer` in case that had been set previously.
    */
  def async: ScriptAttributes

  /** Indicates that the `type` attribute should be set to `module`.
    */
  def asModule: ScriptAttributes

  /** Specifies a value to be set for the `integrity` attribute.
    */
  def withIntegrity(value: String): ScriptAttributes

  /** Specifies a value to be set for the `crossorigin` attribute.
    */
  def withCrossOrigin(value: CrossOrigin): ScriptAttributes

}

object ScriptAttributes {

  val defaults: ScriptAttributes =
    Impl(LoadType.Default, isModule = false, integrity = None, CrossOrigin.Unspecified)

  private sealed trait LoadType

  private object LoadType {
    case object Default extends LoadType
    case object Defer   extends LoadType
    case object Async   extends LoadType
  }

  private final case class Impl(
      loadType: LoadType,
      isModule: Boolean,
      integrity: Option[String],
      crossOrigin: CrossOrigin
  ) extends ScriptAttributes {

    def isDefer: Boolean = loadType == LoadType.Defer
    def isAsync: Boolean = loadType == LoadType.Async

    def defer: ScriptAttributes = copy(
      loadType = if (isModule) loadType else LoadType.Defer
    )

    def async: ScriptAttributes = copy(
      loadType = LoadType.Async
    )

    def asModule: ScriptAttributes = copy(
      isModule = true,
      loadType = if (loadType == LoadType.Defer) LoadType.Default else loadType
    )

    def withIntegrity(value: String): ScriptAttributes        = copy(integrity = Some(value))
    def withCrossOrigin(value: CrossOrigin): ScriptAttributes = copy(crossOrigin = value)
  }

}

sealed trait CrossOrigin

object CrossOrigin {
  object Unspecified    extends CrossOrigin
  object Anonymous      extends CrossOrigin
  object UseCredentials extends CrossOrigin

  def fromString(value: String): Option[CrossOrigin] = value match {
    case "anonymous"       => Some(Anonymous)
    case "use-credentials" => Some(UseCredentials)
    case _                 => None
  }

}
