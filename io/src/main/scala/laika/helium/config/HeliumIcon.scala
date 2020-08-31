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

import laika.ast.{Icon, Styles}

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
  private val options = Styles("icofont-laika")
  val navigationMenu: Icon = Icon('\uefa2', Some("Navigation"), options)
  val home: Icon           = Icon('\uef47', Some("Home"), options)
  val link: Icon           = Icon('\uef71', None, options)
  val close: Icon          = Icon('\ueedd', Some("Close"), options)
  val check: Icon          = Icon('\ueed7', None, options)
  val chat: Icon           = Icon('\ueed5', Some("Chat"), options)
  val settings: Icon       = Icon('\ueed5', Some("Settings"), options)
  val edit: Icon           = Icon('\uef10', Some("Edit"), options)
  val demo: Icon           = Icon('\ueeea', Some("Demo"), options)
  val download: Icon       = Icon('\uef08', Some("Download"), options)
  val info: Icon           = Icon('\uef4e', None, options)
  val warning: Icon        = Icon('\uefb0', None, options)
  val error: Icon          = Icon('\ueedd', None, options)
  val twitter: Icon        = Icon('\ued7a', Some("Twitter"), options)
}
