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

package laika.io.theme

import java.io.File

import laika.ast.Size
import laika.config._

/**
  * @author Jens Halm
  */
case class Font private (embedResource: Option[EmbeddedFont], webCSS: Option[String]) {

  def embedFile (name: String): Font = embedFile(new File(name))
  def embedFile (file: File): Font = new Font(Some(EmbeddedFontFile(file)), webCSS)
  def embedResource (name: String): Font = new Font(Some(EmbeddedFontResource(name)), webCSS)
  def webCSS (url: String): Font = new Font(embedResource, Some(url))
  
  def definedAs (family: String, weight: FontWeight, style: FontStyle): FontDefinition =
    FontDefinition(this, family, weight, style)
  
}

object Font {
  
  def embedFile (name: String): Font = embedFile(new File(name))
  def embedFile (file: File): Font = new Font(Some(EmbeddedFontFile(file)), None)
  def embedResource (name: String): Font = new Font(Some(EmbeddedFontResource(name)), None)
  def webCSS (url: String): Font = new Font(None, Some(url))
  
  private[laika] def create (embedResource: Option[String], embedFile: Option[String], webCSS: Option[String]): Option[Font] =
    (embedResource, embedFile, webCSS) match {
      case (None, None, None) => None
      case (_, Some(file), _) => Some(new Font(Some(EmbeddedFontFile(new File(file))), webCSS))
      case (Some(res), _, _) => Some(new Font(Some(EmbeddedFontResource(res)), webCSS))
      case _ => Some(new Font(None, webCSS))
    }
}

sealed trait EmbeddedFont
case class EmbeddedFontFile (file: File) extends EmbeddedFont
case class EmbeddedFontResource (name: String) extends EmbeddedFont

sealed abstract class FontWeight (val value: String) 

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

sealed abstract class FontStyle (val value: String)

object FontStyle {
  
  object Normal extends FontStyle("normal")
  object Italic extends FontStyle("italic")
  
  def fromString (value: String): Option[FontStyle] = value match {
    case "normal" => Some(Normal)
    case "italic" => Some(Italic)
    case _ => None
  } 
  
}

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
  implicit val defaultKey: DefaultKey[FontDefinition] = DefaultKey(LaikaKeys.root.child("fonts"))
  
}

case class ThemeFonts private (body: String, headlines: String, code: String)

case class FontSizes (body: Size, code: Size, title: Size, header2: Size, header3: Size, header4: Size, small: Size)
