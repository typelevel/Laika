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

package laika.helium.config

import laika.ast.{Icon, IconGlyph, InlineSVGIcon, Styles}
import laika.helium.builder.SVGIcons
import laika.rewrite.link.IconRegistry

/** Enumeration for using any of the icons provided by the Helium theme out of the box in the theme configuration.
  * 
  * Several options like those for the landing page or the top navigation bar allow the addition of icon links
  * where these pre-built selection can be used.
  * 
  * They are based on the `icofont` which is licensed under the Open Font license and part of the `laika-io`
  * artifact.
  * 
  * @author Jens Halm
  */
object HeliumIcon {
  
  private val iconFontStyle = Styles("icofont-laika")
  
  val navigationMenu: Icon = IconGlyph('\uefa2', Some("Navigation"), iconFontStyle)
  val home: Icon           = IconGlyph('\uef47', Some("Home"), iconFontStyle)
  val link: Icon           = IconGlyph('\uef71', None, iconFontStyle)
  val close: Icon          = IconGlyph('\ueedd', Some("Close"), iconFontStyle)
  val check: Icon          = IconGlyph('\ueed7', None, iconFontStyle)
  val chat: Icon           = IconGlyph('\ueed5', Some("Chat"), iconFontStyle)
  val settings: Icon       = IconGlyph('\uefb0', Some("Settings"), iconFontStyle)
  val edit: Icon           = IconGlyph('\uef10', Some("Edit"), iconFontStyle)
  val demo: Icon           = IconGlyph('\ueeea', Some("Demo"), iconFontStyle)
  val download: Icon       = IconGlyph('\uef08', Some("Download"), iconFontStyle)
  val info: Icon           = IconGlyph('\uef4e', None, iconFontStyle)
  val warning: Icon        = IconGlyph('\uf026', None, iconFontStyle)
  val error: Icon          = IconGlyph('\ueedd', None, iconFontStyle)
  val twitter: Icon        = IconGlyph('\ued7a', Some("Twitter"), iconFontStyle)
  val api: Icon            = InlineSVGIcon(SVGIcons.apiIcon, Some("API"))
  val github: Icon         = InlineSVGIcon(SVGIcons.githubIcon, Some("Source Code"))
  
  val registry: IconRegistry = IconRegistry(
    "navigationMenu" -> navigationMenu,
    "home"           -> home,
    "link"           -> link,
    "close"          -> close,
    "check"          -> check,
    "chat"           -> chat,
    "settings"       -> settings,
    "edit"           -> edit,
    "demo"           -> demo,
    "download"       -> download,
    "info"           -> info,
    "warning"        -> warning,
    "error"          -> error,
    "twitter"        -> twitter,
    "api"            -> api,
    "github"         -> github,
  )
}
