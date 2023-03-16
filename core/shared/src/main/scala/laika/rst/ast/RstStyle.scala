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

package laika.rst.ast

import laika.ast.{ Options, Styles }

/** Constants for style names wrapped in Options instances which are commonly used by Laika's reStructuredText parsers and rewrite rules.
  *
  * @author Jens Halm
  */
object RstStyle {

  val line: Options           = Styles("line")
  val lineBlock: Options      = Styles("line-block")
  val target: Options         = Styles("target")
  val option: Options         = Styles("option")
  val optionList: Options     = Styles("option-list")
  val description: Options    = Styles("description")
  val fieldName: Options      = Styles("field-name")
  val fieldBody: Options      = Styles("field-body")
  val fieldList: Options      = Styles("field-list")
  val compound: Options       = Styles("compound")
  val admonition: Options     = Styles("admonition")
  val topic: Options          = Styles("topic")
  val rubric: Options         = Styles("rubric")
  val subtitle: Options       = Styles("subtitle")
  val sidebar: Options        = Styles("sidebar")
  val subscript: Options      = Styles("subscript")
  val superscript: Options    = Styles("superscript")
  val titleReference: Options = Styles("title-reference")

}
