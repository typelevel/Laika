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

import laika.ast.Span

/**
  * @author Jens Halm
  */
sealed trait WebLink {

  def target: LandingPageLink
  
  def asSpan: Span
  
}

case class Icon (styles: Set[String])

case class IconLink (target: LandingPageLink, icon: Icon, text: Option[String] = None) extends WebLink {
  def asSpan: Span = ???
}

case class ButtonLink (target: LandingPageLink, text: String, icon: Option[Icon] = None) extends WebLink {
  def asSpan: Span = ???
}

case class TextLink (target: LandingPageLink, text: String) extends WebLink {
  def asSpan: Span = ???
}
