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

package laika.directive.std

import cats.syntax.all._
import laika.api.bundle.{ BlockDirectives, SpanDirectives }
import laika.ast.{
  BlockSequence,
  Image,
  InternalTarget,
  LengthUnit,
  VirtualPath,
  SpanSequence,
  Styles
}

/** Provides the implementation for the image directives included in Laika.
  *
  * This includes the block-level and span-level image directives.
  *
  * For full documentation see the section about
  * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html#image Image Directives]]
  * in the manual.
  *
  * @author Jens Halm
  */
private[laika] object ImageDirectives {

  /** Markup directive for inserting an image as a block level element.
    *
    * The image source must be set with a positional attribute (in parenthesis).
    * It is the only required attribute and can be a local path (absolute or relative) in the virtual tree
    * of your input sources, or an external URL (`http:` or `https:`).
    *
    * The optional `intrinsicWidth` and `intrinsicHeight` attributes can be used to describe the dimensions
    * of the image to avoid layout shifts when loading the page.
    *
    * For controlling the actual display size you can use the `style` attribute together with a matching declaration
    * in one of your site's CSS documents.
    * If omitted the theme in use will usually have a sensible default size.
    */
  val forBlocks: BlockDirectives.Directive = BlockDirectives.create("image") {
    import BlockDirectives.dsl._
    (
      attribute(0).as[String].widen,
      attribute("intrinsicWidth").as[Double].optional,
      attribute("intrinsicHeight").as[Double].optional,
      attribute("style").as[String].optional,
      attribute("alt").as[String].optional,
      attribute("title").as[String].optional,
      cursor
    ).mapN { (src, width, height, style, alt, title, cursor) =>
      val img     = Image(
        InternalTarget(VirtualPath.parse(src)).relativeTo(cursor.path),
        width.map(LengthUnit.px(_)),
        height.map(LengthUnit.px(_)),
        alt,
        title
      )
      val options = Styles(style.getOrElse("default-image-block"))
      BlockSequence(Seq(SpanSequence(img)), options = options)
    }
  }

  /** Markup directive for inserting an image as an inline element.
    *
    * The image source must be set with a positional attribute (in parenthesis).
    * It is the only required attribute and can be a local path (absolute or relative) in the virtual tree
    * of your input sources, or an external URL (`http:` or `https:`).
    *
    * The optional `intrinsicWidth` and `intrinsicHeight` attributes can be used to describe the dimensions
    * of the image to avoid layout shifts when loading the page.
    *
    * For controlling the actual display size you can use the `style` attribute together with a matching declaration
    * in one of your site's CSS documents.
    * If omitted the theme in use will usually have a sensible default size.
    */
  val forSpans: SpanDirectives.Directive = SpanDirectives.create("image") {
    import SpanDirectives.dsl._
    (
      attribute(0).as[String].widen,
      attribute("intrinsicWidth").as[Double].optional,
      attribute("intrinsicHeight").as[Double].optional,
      attribute("style").as[String].optional,
      attribute("alt").as[String].optional,
      attribute("title").as[String].optional,
      cursor
    ).mapN { (src, width, height, style, alt, title, cursor) =>
      val options = Styles(style.getOrElse("default-image-span"))
      Image(
        InternalTarget(VirtualPath.parse(src)).relativeTo(cursor.path),
        width.map(LengthUnit.px(_)),
        height.map(LengthUnit.px(_)),
        alt,
        title,
        options
      )
    }
  }

}
