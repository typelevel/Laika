/*
 * Copyright 2013 the original author or authors.
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

package laika.tree

import laika.tree.Documents.DocumentContext
import laika.tree.Documents.Path
import laika.tree.Elements._
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

object Templates { // TODO - maybe move to laika.template.Elements
  
  trait PlaceholderSpan extends Span { // TODO - maybe rename
    def resolve (context: DocumentContext): Span
  }

  trait PlaceholderBlock extends Block {
    def resolve (context: DocumentContext): Block
  }
  
  trait PlaceholderSpanFactory extends Span { // TODO - the factories are probably not needed
    def create: PlaceholderSpan
  }
  
  trait PlaceholderBlockFactory extends Block {
    def create: PlaceholderBlock
  }
  
  
  abstract class ContextReference[T <: Span] (ref: String) extends PlaceholderSpan {
    def result (value: Option[Any]): T
    def resolve (context: DocumentContext): Span = context.resolveReference(ref) match {
      case Some(s: ElementTraversal[_]) => result(Some(s rewrite rewriteRules(context)))
      case other => result(other)
    }
  }
  
  case class TemplateContextReference (ref: String, options: Options = NoOpt) extends ContextReference[TemplateSpan](ref) with TemplateSpan {
    def result (value: Option[Any]): TemplateSpan = value match {
      case Some(s: TemplateSpan) => s
      case Some(e: Element)      => TemplateElement(e)
      case Some(other)           => TemplateString(other.toString)
      case None                  => TemplateString("")
    }
  }
  
  case class MarkupContextReference (ref: String, options: Options = NoOpt) extends ContextReference[Span](ref) {
    def result (value: Option[Any]): Span = value match {
      case Some(s: Span)    => s
      case Some(e: Element) => TemplateElement(e)
      case Some(other)      => Text(other.toString)
      case None             => Text("")
    }
  }

  
  trait TemplateSpan extends Span
  
  case class TemplateElement (element: Element, options: Options = NoOpt) extends TemplateSpan with ElementTraversal[TemplateElement]

  case class TemplateSpanSequence (content: Seq[TemplateSpan], options: Options = NoOpt) extends TemplateSpan with SpanContainer[TemplateSpanSequence]

  case class TemplateString (content: String, options: Options = NoOpt) extends TemplateSpan with TextContainer
  
  case class TemplateRoot (content: Seq[TemplateSpan], options: Options = NoOpt) extends Block with SpanContainer[TemplateRoot]
  
  case class TemplateDocument (path: Path, content: TemplateRoot, config: Config = ConfigFactory.empty) {
    
    val name = path.name
    
    def rewrite (context: DocumentContext) = {
      val newContent = content rewrite rewriteRules(context)
      context.document.withRewrittenContent(RootElement(Seq(newContent)), context.document.fragments, context.autonumbering.number)
    }
    
  }
  
  def rewriteRules (context: DocumentContext) = {
    lazy val rule: PartialFunction[Element, Option[Element]] = {
      case ph: PlaceholderBlock => Some(rewriteChild(ph resolve context))
      case ph: PlaceholderSpan  => Some(rewriteChild(ph resolve context))
    }
    def rewriteChild (e: Element) = e match {
      case et: ElementTraversal[_] => et rewrite rule
      case other => other
    }
    rule
  }
  

}