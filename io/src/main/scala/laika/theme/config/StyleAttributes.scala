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

sealed abstract class StyleAttributes private {

  def integrity: Option[String]
  def crossOrigin: CrossOrigin

  def withIntegrity(value: String): StyleAttributes
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
