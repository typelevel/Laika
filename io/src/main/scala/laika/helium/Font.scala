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

package laika.helium

import java.io.File

/**
  * @author Jens Halm
  */
class Font private (val embedResource: Option[EmbeddedFont], val webCSS: Option[String]) {

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
  
}

sealed trait EmbeddedFont

case class EmbeddedFontFile (file: File) extends EmbeddedFont
case class EmbeddedFontResource (name: String) extends EmbeddedFont

sealed abstract class FontWeight (val value: String) 

object FontWeight {
  
  object bold extends FontWeight("bold")
  object normal extends FontWeight("normal")
  object `100` extends FontWeight("100")
  object `200` extends FontWeight("200")
  object `300` extends FontWeight("300")
  object `400` extends FontWeight("400")
  object `500` extends FontWeight("500")
  object `600` extends FontWeight("600")
  object `700` extends FontWeight("700")
  object `800` extends FontWeight("800")
  object `900` extends FontWeight("900")
  
}

sealed abstract class FontStyle (val value: String)

object FontStyle {
  
  object normal extends FontStyle("normal")
  object italic extends FontStyle("italic")
  
}

case class FontDefinition (resource: Font, family: String, weight: FontWeight, style: FontStyle)
