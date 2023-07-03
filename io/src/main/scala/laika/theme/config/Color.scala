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

  /** Value between 0 (darkest) and roughly 100 (lightest). */
  private[laika] def approximatePerceptualLuminance: Double

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

      private[laika] def approximatePerceptualLuminance: Double = {
        Color.approximatePerceptualLuminance(
          ((1 - alpha) * 255 + alpha * red).toInt,
          ((1 - alpha) * 255 + alpha * green).toInt,
          ((1 - alpha) * 255 + alpha * blue).toInt
        )
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

    private def toInt(hex: String): Int = Integer.parseInt(hex, 16)

    private[laika] def approximatePerceptualLuminance: Double = {
      val (r, g, b) = {
        if (hexValue.length == 3) (hexValue(0).toString, hexValue(1).toString, hexValue(2).toString)
        else (hexValue.substring(0, 2), hexValue.substring(2, 4), hexValue.substring(4, 6))
      }
      Color.approximatePerceptualLuminance(toInt(r), toInt(g), toInt(b))
    }

  }

  private[laika] def approximatePerceptualLuminance(red: Int, green: Int, blue: Int): Double = {
    /* Using https://stackoverflow.com/a/60126653 which is a simplified version 
     * of https://stackoverflow.com/a/56678483 (same author).
     * For our purposes approximation is sufficient.
     * The scenarios where it might yield unsatisfactory results are only when the user configured
     * a very low-contrast color set, which will most likely not look good anyway, 
     * no matter what we do with it.
     * This calculation is used to determine whether we use the default or inverted colors for
     * the component set on the landing page, but might at some point find additional applications
     * for determining sensible defaults.
     */
    val r    = Math.pow(red / 255.0, 2.218) * 0.2126
    val g    = Math.pow(green / 255.0, 2.218) * 0.7156
    val b    = Math.pow(blue / 255.0, 2.218) * 0.0722
    val yLum = r + g + b
    Math.pow(yLum, 0.43) * 100
  }

}
