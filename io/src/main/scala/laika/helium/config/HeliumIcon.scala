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

/**
  * @author Jens Halm
  */
object HeliumIcon {
  private val options = Styles("icofont-laika")
  val navigationMenu: Icon = Icon('\uefa2', options)
  val home: Icon = Icon('\uef47', options)
  val link: Icon = Icon('\uef71', options)
  val close: Icon = Icon('\ueedd', options)
  val check: Icon = Icon('\ueed7', options)
  val chat: Icon = Icon('\ueed5', options)
  val settings: Icon = Icon('\ueed5', options)
  val edit: Icon = Icon('\uef10', options)
  val demo: Icon = Icon('\ueeea', options)
  val download: Icon = Icon('\uef08', options)
  val info: Icon = Icon('\uef4e', options)
  val warning: Icon = Icon('\uefb0', options)
  val error: Icon = Icon('\ueedd', options)
  val twitter: Icon = Icon('\ued7a', options)
}
