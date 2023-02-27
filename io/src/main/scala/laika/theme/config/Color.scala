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

package laika.theme.config

import laika.parse.text.CharGroup

/** Represent a color value in a format compatible with CSS.
  *
  * Use the constructors of the companion for creating instances.
  *
  * @author Jens Halm
  */
sealed abstract class Color(val displayValue: String) {

  /** Validates the color value, providing an error message if invalid, otherwise `None`.
    *
    * Implementation note: future versions of the library, once based on Scala 3, will hopefully offer
    * compile-time validation of color and length values.
    * But we want to avoid a dependency on a library like refined which is based on Scala 2 macros
    * which will not be supported in Scala 3.
    */
  def validate: Option[String]
}

object Color {

  /** Constructs a Color value with the specified red, green and blue components.
    */
  def rgb(red: Int, green: Int, blue: Int): Color = rgba(red, green, blue, 1)

  /** Constructs a Color value with the specified red, green and blue components and the corresponding alpha value.
    */
  def rgba(red: Int, green: Int, blue: Int, alpha: Float): Color = {
    val display = if (alpha == 1) s"rgb($red,$green,$blue)" else s"rgba($red,$green,$blue,$alpha)"
    new Color(display) {
      def validate: Option[String] = {
        def unsignedByte(value: Int): Boolean = value >= 0 && value <= 255
        if (
          !unsignedByte(red) || !unsignedByte(green) || !unsignedByte(
            blue
          ) || alpha < 0 || alpha > 1
        )
          Some(
            s"red, green and blue value must be between 0 and 255, alpha must be between 0.0 and 1.0"
          )
        else None
      }
    }
  }

  /** Constructs a Color value based on the specified hex string which needs to consist of
    * either 3 or 6 hex digits.
    */
  def hex(hexValue: String): Color = new Color(s"#$hexValue") {

    def validate: Option[String] =
      if (
        (hexValue.length != 3 && hexValue.length != 6) || hexValue.forall(
          CharGroup.hexDigit.contains
        )
      )
        Some("value must be 3 or 6 hexadecimal digits")
      else None

  }

}
