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
  
  
  case class ContextReference (ref: String, options: Options = NoOpt) extends TemplateSpan with PlaceholderSpan {
    def resolve (context: DocumentContext): Span = Text(ref) // TODO - implement
  }

  
  trait TemplateSpan extends Span
  
  case class TemplateElement (element: Element, options: Options = NoOpt) extends TemplateSpan with ElementTraversal[TemplateElement]

  case class TemplateString (content: String, options: Options = NoOpt) extends TemplateSpan with TextContainer
  
  case class TemplateRoot (content: Seq[TemplateSpan], options: Options = NoOpt) extends Block with SpanContainer[TemplateRoot]
  
  case class TemplateDocument (path: Path, content: TemplateRoot, config: Config) {
    
    val name = path.name
    
    def rewrite (context: DocumentContext) = {
      lazy val rule: PartialFunction[Element, Option[Element]] = {
        case ph: PlaceholderBlock => Some(rewriteChild(ph resolve context))
        case ph: PlaceholderSpan  => Some(rewriteChild(ph resolve context))
      }
      def rewriteChild (e: Element) = e match {
        case et: ElementTraversal[_] => et rewrite rule
        case other => other
      }
      val newContent = content rewrite rule
      context.document.withRewrittenContent(RootElement(Seq(newContent)))
    }
    
  }
  

}