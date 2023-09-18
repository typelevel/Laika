package laika.helium.internal.config

import laika.ast.{ Document, Path }
import laika.theme.config.{ ScriptAttributes, StyleAttributes }

private[helium] case class InternalJS(
    searchPath: Path,
    attributes: ScriptAttributes,
    condition: Document => Boolean
)

private[helium] case class ExternalJS(
    url: String,
    attributes: ScriptAttributes,
    condition: Document => Boolean
)

private[helium] case class InlineJS(
    content: String,
    isModule: Boolean,
    condition: Document => Boolean
)

private[helium] case class ScriptIncludes(
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

private[helium] object ScriptIncludes {
  val empty: ScriptIncludes = ScriptIncludes(Nil, Nil, Nil)
}

private[helium] case class InternalCSS(
    searchPath: Path,
    attributes: StyleAttributes,
    condition: Document => Boolean
)

private[helium] case class ExternalCSS(
    url: String,
    attributes: StyleAttributes,
    condition: Document => Boolean
)

private[helium] case class InlineCSS(
    content: String,
    condition: Document => Boolean
)

private[helium] case class StyleIncludes(
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

private[helium] object StyleIncludes {
  val empty: StyleIncludes = StyleIncludes(Nil, Nil, Nil)
}
