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

import laika.ast._

/**
  * @author Jens Halm
  */
sealed trait ThemeLink extends SpanResolver {

  type Self <: ThemeLink
  
  def target: ThemeTarget
  
  def resolve (cursor: DocumentCursor): Span = target.resolve(cursor) match {
    case Left(msg) => InvalidElement(msg, s"<ThemeLink: $this>").asSpan
    case Right(target) => createLink(target)
  }

  def unresolvedMessage: String = s"Unresolved theme link: $this"

  protected def createLink (target: Target): Span
  
}

case class IconLink (target: ThemeTarget, icon: Icon, text: Option[String] = None, options: Options = NoOpt) extends ThemeLink {
  type Self = IconLink
  protected def createLink (target: Target): Span = SpanLink(icon +: text.map(Text(_)).toSeq, target)
  def withOptions(newOptions: Options): IconLink = copy(options = newOptions)
}

case class ButtonLink (target: ThemeTarget, text: String, icon: Option[Icon] = None, options: Options = NoOpt) extends ThemeLink {
  type Self = ButtonLink
  protected def createLink (target: Target): Span = SpanLink(icon.toSeq :+ Text(text), target, options = HeliumStyles.button)
  def withOptions(newOptions: Options): ButtonLink = copy(options = newOptions)
}

case class TextLink (target: ThemeTarget, text: String, options: Options = NoOpt) extends ThemeLink {
  type Self = TextLink
  protected def createLink (target: Target): Span = SpanLink(Seq(Text(text)), target)
  def withOptions(newOptions: Options): TextLink = copy(options = newOptions)
}

sealed trait ThemeTarget {
  def description: String
  def resolve (cursor: DocumentCursor): Either[String, Target]
}
object ThemeTarget {
  def internal (target: Path): ThemeTarget = new ThemeTarget {
    val description = s"internal target: '${target.toString}'"
    def resolve  (cursor: DocumentCursor): Either[String, Target] = {
      val valid = cursor.root.target.tree.selectDocument(target.withoutFragment.relative).nonEmpty || 
        cursor.root.target.staticDocuments.contains(target.withoutFragment)
      if (valid) Right(InternalTarget.fromPath(target, cursor.path))
      else Left(s"Theme Link to unresolved target: $target")
    }
  }
  def external (url: String): ThemeTarget = new ThemeTarget {
    val description = s"external target: '$url'"
    def resolve  (cursor: DocumentCursor): Either[String, Target] = Right(ExternalTarget(url))
  }
}
