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

package laika.ast

/** Encapsulates size information with a CSS-compatible length unit.
  */
case class Length(amount: Double, unit: LengthUnit) {
  def scale(percent: Double): Length = copy(amount * percent / 100)
  def displayValue: String           = amount.toString.stripSuffix(".0") + unit.displayValue
}

/** A base for builder of CSS-compatible length units.
  */
sealed abstract class LengthUnit(val displayValue: String) extends (Double => Length) {
  def apply(amount: Double): Length = Length(amount, this)
}

object LengthUnit {
  object px      extends LengthUnit("px")
  object mm      extends LengthUnit("mm")
  object cm      extends LengthUnit("cm")
  object in      extends LengthUnit("in")
  object pc      extends LengthUnit("pc")
  object pt      extends LengthUnit("pt")
  object ch      extends LengthUnit("ch")
  object em      extends LengthUnit("em")
  object ex      extends LengthUnit("ex")
  object rem     extends LengthUnit("rem")
  object vh      extends LengthUnit("vh")
  object vw      extends LengthUnit("vw")
  object vmin    extends LengthUnit("vmin")
  object vmax    extends LengthUnit("vmax")
  object percent extends LengthUnit("%")

  private val all: Map[String, LengthUnit] =
    Seq(px, mm, cm, in, pc, pt, ch, em, ex, rem, vh, vw, vmin, vmax, percent).map(u =>
      (u.displayValue, u)
    ).toMap

  def fromString(value: String): Option[LengthUnit] = all.get(value)
}
