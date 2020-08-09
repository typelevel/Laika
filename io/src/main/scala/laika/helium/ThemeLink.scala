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

import laika.ast._

/**
  * @author Jens Halm
  */
sealed trait ThemeLink {

  def target: ThemeTarget
  
  def asSpan: Span
  
}

case class Icon (codePoint: Char, options: Options = NoOpt) extends Span {
  type Self = Icon
  def withOptions(newOptions: Options): Icon = copy(options = newOptions)
}

case class IconLink (target: ThemeTarget, icon: Icon, text: Option[String] = None) extends ThemeLink {
  def asSpan: Span = SpanLink(icon +: text.map(Text(_)).toSeq, target.fullTarget)
}

case class ButtonLink (target: ThemeTarget, text: String, icon: Option[Icon] = None) extends ThemeLink {
  def asSpan: Span = SpanLink(icon.toSeq :+ Text(text), target.fullTarget, options = HeliumStyles.button)
}

case class TextLink (target: ThemeTarget, text: String) extends ThemeLink {
  def asSpan: Span = SpanLink(Seq(Text(text)), target.fullTarget)
}

sealed trait ThemeTarget {
  def fullTarget: Target
}
object ThemeTarget {
  def internal (target: Path): ThemeTarget = new ThemeTarget {
    def fullTarget = InternalTarget(target, target.relative)
  }
  def external (url: String): ThemeTarget = new ThemeTarget {
    def fullTarget = ExternalTarget(url)
  }
}
