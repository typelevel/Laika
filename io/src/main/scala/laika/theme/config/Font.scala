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

import java.io.File

import laika.ast.Path.Root
import laika.ast.{Path, PathBase}
import laika.config._

/** Represents a font resource, either based on a local classpath or file system resource,
  * or a web font URL, or both.
  * 
  * E-book formats like EPUB or PDF require a local font file to be available for embedding.
  * A web font URL can only be used for website generation.
  * 
  * @author Jens Halm
  */
case class Font private (embedResource: Option[EmbeddedFont], webCSS: Option[String]) {

  /** Specifies a font file that can be used for embedding in EPUB or PDF.
    * This is one of the few places in Laika where a real file-system path needs to be provided,
    * not a path from the library's virtual path.
    */
  def embedFile (name: String): Font = embedFile(new File(name))

  /** Specifies a font file that can be used for embedding in EPUB or PDF.
    * This is one of the few places in Laika where a real file-system path needs to be provided,
    * not a path from the library's virtual path.
    */
  def embedFile (file: File): Font = new Font(Some(EmbeddedFontFile(file)), webCSS)

  /** Specifies a font file as a classpath resource that can be used for embedding in EPUB or PDF.
    * For theme authors classpath resources are the recommended way of providing default fonts.
    */
  def embedResource (name: String): Font = new Font(Some(EmbeddedFontResource(name)), webCSS)

  /** Specifies a URL for a web font which can be auto-linked in a theme template.
    */
  def webCSS (url: String): Font = new Font(embedResource, Some(url))

   /** Creates the final `FontDefinition` instance that can be used in Laika's and Helium's configuration
    *  by mapping the font resource to a family name, font weight and style.
    */
  def definedAs (family: String, weight: FontWeight, style: FontStyle): FontDefinition =
    FontDefinition(this, family, weight, style)
  
}

/** Companion for creating Font instances.
  * 
  * Operations can be chained, e.g. by calling `Font.embedFile(...).webCSS(...)` if both need to be provided.
  * The final `FontDefinition` instance that can be used in Laika's and Helium's configuration can be obtained
  * by calling `definedAs` in the end, where the font resource is mapped to a family name, font weight and style.
  */
object Font {

  /** Specifies a font file that can be used for embedding in EPUB or PDF.
    * This is one of the few places in Laika where a real file-system path needs to be provided,
    * not a path from the library's virtual path.
    */
  def embedFile (name: String): Font = embedFile(new File(name))

  /** Specifies a font file that can be used for embedding in EPUB or PDF.
    * This is one of the few places in Laika where a real file-system path needs to be provided,
    * not a path from the library's virtual path.
    */
  def embedFile (file: File): Font = new Font(Some(EmbeddedFontFile(file)), None)

  /** Specifies a font file as a classpath resource that can be used for embedding in EPUB or PDF.
    * For theme authors classpath resources are the recommended way of providing default fonts.
    */
  def embedResource (name: String): Font = new Font(Some(EmbeddedFontResource(name)), None)

  /** Specifies a URL for a web font which can be auto-linked in a theme template.
    */
  def webCSS (url: String): Font = new Font(None, Some(url))
  
  private[laika] def create (embedResource: Option[String], embedFile: Option[String], webCSS: Option[String]): Option[Font] =
    (embedResource, embedFile, webCSS) match {
      case (None, None, None) => None
      case (_, Some(file), _) => Some(new Font(Some(EmbeddedFontFile(new File(file))), webCSS))
      case (Some(res), _, _) => Some(new Font(Some(EmbeddedFontResource(res)), webCSS))
      case _ => Some(new Font(None, webCSS))
    }
}

/** Base trait for the types of embedded fonts Laika supports, which are either a file-system or classpath resource.
  */
sealed trait EmbeddedFont {
  def path: Path
}

/** Represent a font resource from the file system.
  */
case class EmbeddedFontFile (file: File) extends EmbeddedFont {
  val path: Path = Root / "laika" / "fonts" / file.getName
}
/** Represent a font files as a classpath resource.
  */
case class EmbeddedFontResource (name: String) extends EmbeddedFont {
  val path: Path = Root / "laika" / "fonts" / PathBase.parse(name).name
}

/** Enumeration for the valid font weights that can be assigned to a font resource, compatible with CSS properties.
  */
sealed abstract class FontWeight (val value: String)

/** Enumeration for the valid font weights that can be assigned to a font resource, compatible with CSS properties.
  */
object FontWeight {
  
  object Bold extends FontWeight("bold")
  object Normal extends FontWeight("normal")
  object `100` extends FontWeight("100")
  object `200` extends FontWeight("200")
  object `300` extends FontWeight("300")
  object `400` extends FontWeight("400")
  object `500` extends FontWeight("500")
  object `600` extends FontWeight("600")
  object `700` extends FontWeight("700")
  object `800` extends FontWeight("800")
  object `900` extends FontWeight("900")

  private val all: Map[String, FontWeight] =
    Seq(Bold, Normal, `100`, `200`, `300`, `400`, `500`, `600`, `700`, `800`, `900`).map(u => (u.value, u)).toMap
  def fromString (value: String): Option[FontWeight] = all.get(value)
  
}

/** Enumeration for the valid font styles that can be assigned to a font resource, compatible with CSS properties.
  */
sealed abstract class FontStyle (val value: String)

/** Enumeration for the valid font styles that can be assigned to a font resource, compatible with CSS properties.
  */
object FontStyle {
  
  object Normal extends FontStyle("normal")
  object Italic extends FontStyle("italic")
  
  def fromString (value: String): Option[FontStyle] = value match {
    case "normal" => Some(Normal)
    case "italic" => Some(Italic)
    case _ => None
  } 
  
}

/** Represent a full font definition, combining the font resource (file and/or web URL) plus the 
  * mapping to a family name, font weight and style.
  * 
  * When providing multiple different weights and styles for the same font, 
  * a separate `FontDefinition` instance needs to be built for each of them.
  */
case class FontDefinition (resource: Font, family: String, weight: FontWeight, style: FontStyle)

object FontDefinition {

  implicit val decoder: ConfigDecoder[FontDefinition] = ConfigDecoder.config.flatMap { config =>
    for {
      family    <- config.get[String]("family")
      weightStr <- config.get[String]("weight")
      weight    <- FontWeight.fromString(weightStr).toRight(DecodingError(s"Invalid value for fontWeight: '$weightStr'"))
      styleStr  <- config.get[String]("style")
      style     <- FontStyle.fromString(styleStr).toRight(DecodingError(s"Invalid value for fontStyle: '$styleStr'"))
      embedRes  <- config.getOpt[String]("embedResource")
      embedFile <- config.getOpt[String]("embedFile")
      webCSS    <- config.getOpt[String]("webCSS")
      font      <- Font.create(embedRes, embedFile, webCSS).toRight(DecodingError("At least one of embedFile, embedResource or webCSS must be defined for Font"))
    } yield {
      FontDefinition(font, family, weight, style)
    }
  }
  implicit val encoder: ConfigEncoder[FontDefinition] = ConfigEncoder[FontDefinition] { fd =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("family", fd.family)
      .withValue("weight", fd.weight.value)
      .withValue("style", fd.style.value)
      .withValue("embedResource", fd.resource.embedResource.collect { case EmbeddedFontResource(r) => r })
      .withValue("embedFile", fd.resource.embedResource.collect { case EmbeddedFontFile(f) => f.getPath })
      .withValue("webCSS", fd.resource.webCSS)
      .build
  }
  implicit val defaultKey: DefaultKey[Seq[FontDefinition]] = DefaultKey(LaikaKeys.root.child("fonts"))
  
}
