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

/** Options for customizable elements.
  */
sealed abstract class Options {
  /** The id of this element. Has to be unique
    *  across all element types of a document,
    *  including the ids of `LinkTarget` instances.
    */
  def id: Option[String]
  /** Style names that may have an influence
    *  on rendering of this element.
    */
  def styles: Set[String]
  /** Merges these options with the specified
    *  options. If the id has been set in both
    *  instances, the other instance overrides
    *  this one.
    */
  def + (other: Options): Options
}

/** `Options` implementation for non-empty instances.
  *
  *  For creating new instances it is usually more convenient to use the various factory objects.
  *  Example for creating an instance with an id and two styles applied:
  *
  *  {{{
  *  val options = Id("myId") + Styles("style1","style2")
  *  }}}
  *
  *  Likewise it is also often more convenient to use the corresponding extractors for pattern matching.
  */
case class SomeOpt (id: Option[String] = None, styles: Set[String] = Set()) extends Options {
  def + (other: Options): Options = SomeOpt(other.id.orElse(id), styles ++ other.styles)
}

/** Empty `Options` implementation.
  */
case object NoOpt extends Options {
  val id: Option[String] = None
  val styles: Set[String] = Set()
  def + (other: Options): Options = other
}

/** Companion for the Options trait.
  */
object Options {
  def apply (id: Option[String] = None, styles: Set[String] = Set()): Options =
    if (id.isEmpty && styles.isEmpty) NoOpt
    else SomeOpt(id,styles)
}

/** Factory and extractor for an `Options` instance
  *  with an id.
  */
object Id {
  def apply (value: String): Options = SomeOpt(id = Some(value))
  def unapply (value: Options): Option[String] = value.id
}

/** Factory and extractor for an `Options` instance
  *  with style names.
  */
object Styles {
  def apply (values: String*): Options = SomeOpt(styles = values.toSet)
  def apply (values: Set[String]): Options = SomeOpt(styles = values)
  def unapplySeq (value: Options): Option[Seq[String]] = Some(value.styles.toSeq)
}


/** Constants for style names wrapped in Options instances which are commonly used by Laika's core parsers and rewrite rules. */
object Style {

  val title: Options = Styles("title")
  val section: Options = Styles("section")
  val sectionNumber: Options = Styles("section-number") // TODO - is camel case right now
  val nav: Options = Styles("nav")
  val navHeader: Options = Styles("nav-header")
  val navList: Options = Styles("nav-list")
  val active: Options = Styles("active") // TODO - rename? self-link?
  val breadcrumb: Options = Styles("breadcrumb")
  val bookmark: Options = Styles("bookmark")
  def level(lev: Int): Options = Styles("level" + lev)

  val alignCenter: Options = Styles("align-center")
  val alignLeft: Options = Styles("align-left")
  val alignRight: Options = Styles("align-right")
  val keepTogether: Options = Styles("keep-together")

  val citation: Options = Styles("citation")
  val footnote: Options = Styles("footnote")
  val footnoteLabel: Options = Styles("footnote-label")

  val label: Options = Styles("label")
  val attribution: Options = Styles("attribution")
  val caption: Options = Styles("caption")
  val legend: Options = Styles("legend")
  val figure: Options = Styles("figure")

  val runtimeMessage: Options = Styles("runtime-message")
  val noHighlight: Options = Styles("nohighlight")

}
