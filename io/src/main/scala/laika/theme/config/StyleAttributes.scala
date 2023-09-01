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
sealed abstract class StyleAttributes private {

  /** The value to be set for the `integrity` attribute.
    * If empty the attribute will not be included.
    */
  def integrity: Option[String]

  /** The value to be set for the `crossorigin` attribute.
    * In case of `CrossOrigin.Unspecified` the attribute will not be included.
    */
  def crossOrigin: CrossOrigin

  /** Specifies a value to be set for the `integrity` attribute.
    */
  def withIntegrity(value: String): StyleAttributes

  /** Specifies a value to be set for the `crossorigin` attribute.
    */
  def withCrossOrigin(value: CrossOrigin): StyleAttributes

}

object StyleAttributes {

  val defaults: StyleAttributes =
    Impl(integrity = None, CrossOrigin.Unspecified)

  private final case class Impl(
      integrity: Option[String],
      crossOrigin: CrossOrigin
  ) extends StyleAttributes {

    def withIntegrity(value: String): StyleAttributes        = copy(integrity = Some(value))
    def withCrossOrigin(value: CrossOrigin): StyleAttributes = copy(crossOrigin = value)
  }

}
