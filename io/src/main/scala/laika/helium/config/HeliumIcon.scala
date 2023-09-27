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

import laika.ast.{ Icon, IconGlyph, InlineSVGIcon, Styles }
import laika.config.IconRegistry
import laika.helium.internal.builder.SVGIcons

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

  private def glyphStyles(iconRef: String) = Styles("icofont-laika", iconRef)

  val navigationMenu: Icon = IconGlyph('\uefa2', Some("Navigation"), glyphStyles("navigationMenu"))
  val home: Icon           = IconGlyph('\uef47', Some("Home"), glyphStyles("home"))
  val link: Icon           = IconGlyph('\uef71', None, glyphStyles("link"))
  val close: Icon          = IconGlyph('\ueedd', Some("Close"), glyphStyles("close"))
  val check: Icon          = IconGlyph('\ueed7', None, glyphStyles("check"))
  val chat: Icon           = IconGlyph('\ueed5', Some("Chat"), glyphStyles("chat"))
  val settings: Icon       = IconGlyph('\uefb0', Some("Settings"), glyphStyles("settings"))
  val edit: Icon           = IconGlyph('\uef10', Some("Edit"), glyphStyles("edit"))
  val demo: Icon           = IconGlyph('\ueeea', Some("Demo"), glyphStyles("demo"))
  val download: Icon       = IconGlyph('\uef08', Some("Download"), glyphStyles("download"))
  val info: Icon           = IconGlyph('\uef4e', None, glyphStyles("info"))
  val warning: Icon        = IconGlyph('\uf026', None, glyphStyles("warning"))
  val error: Icon          = IconGlyph('\ueedd', None, glyphStyles("error"))
  val twitter: Icon        = IconGlyph('\ued7a', Some("Twitter"), glyphStyles("twitter"))
  val api: Icon            = InlineSVGIcon(SVGIcons.apiIcon, Some("API"), Styles("api"))
  val github: Icon   = InlineSVGIcon(SVGIcons.githubIcon, Some("Source Code"), Styles("github"))
  val mastodon: Icon = InlineSVGIcon(SVGIcons.mastodonIcon, Some("Mastodon"), Styles("mastodon"))

  private[helium] val registry: IconRegistry = IconRegistry(
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
    "mastodon"       -> mastodon
  )

}
