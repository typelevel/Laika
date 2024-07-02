/*
 * Copyright 2012-2024 the original author or authors.
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

package laika.theme.config.internal

import laika.ast.{ Document, Path }
import laika.theme.config.{ ScriptAttributes, StyleAttributes }

private[config] case class InternalJS(
    searchPath: Path,
    attributes: ScriptAttributes,
    condition: Document => Boolean
)

private[config] case class ExternalJS(
    url: String,
    attributes: ScriptAttributes,
    condition: Document => Boolean
)

private[config] case class InlineJS(
    content: String,
    isModule: Boolean,
    condition: Document => Boolean
)

private[config] case class ScriptIncludes(
    internal: Seq[InternalJS],
    external: Seq[ExternalJS],
    inlined: Seq[InlineJS]
) {
  def add(value: InlineJS): ScriptIncludes   = copy(inlined = inlined :+ value)
  def add(value: InternalJS): ScriptIncludes = copy(internal = internal :+ value)
  def add(value: ExternalJS): ScriptIncludes = copy(external = external :+ value)

  def applyConditions(doc: Document): ScriptIncludes = {
    copy(
      internal = internal.filter(_.condition(doc)),
      external = external.filter(_.condition(doc)),
      inlined = inlined.filter(_.condition(doc))
    )
  }

  def isEmpty: Boolean = internal.isEmpty && external.isEmpty && inlined.isEmpty
}

private[config] object ScriptIncludes {
  val empty: ScriptIncludes = ScriptIncludes(Nil, Nil, Nil)
}

private[config] case class InternalCSS(
    searchPath: Path,
    attributes: StyleAttributes,
    condition: Document => Boolean
)

private[config] case class ExternalCSS(
    url: String,
    attributes: StyleAttributes,
    condition: Document => Boolean
)

private[config] case class InlineCSS(
    content: String,
    condition: Document => Boolean
)

private[config] case class StyleIncludes(
    internal: Seq[InternalCSS],
    external: Seq[ExternalCSS],
    inlined: Seq[InlineCSS]
) {
  def add(value: InlineCSS): StyleIncludes   = copy(inlined = inlined :+ value)
  def add(value: InternalCSS): StyleIncludes = copy(internal = internal :+ value)
  def add(value: ExternalCSS): StyleIncludes = copy(external = external :+ value)

  def applyConditions(doc: Document): StyleIncludes = {
    copy(
      internal = internal.filter(_.condition(doc)),
      external = external.filter(_.condition(doc)),
      inlined = inlined.filter(_.condition(doc))
    )
  }

  def isEmpty: Boolean = internal.isEmpty && external.isEmpty && inlined.isEmpty

}

private[config] object StyleIncludes {
  val empty: StyleIncludes = StyleIncludes(Nil, Nil, Nil)
}
