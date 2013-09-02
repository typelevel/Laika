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

object Templates {
  
  trait PlaceholderSpan extends Span { // TODO - maybe rename
    def resolve (context: DocumentContext): Span
  }

  trait PlaceholderBlock extends Block { // TODO - maybe rename
    def resolve (context: DocumentContext): Block
  }

  
  trait TemplateSpan extends Span
  
  case class TemplateElement (element: Element, options: Options = NoOpt) extends TemplateSpan // TODO - does not get rewritten

  case class TemplateString (content: String, options: Options = NoOpt) extends TemplateSpan with TextContainer
  
  case class TemplateRoot (content: Seq[TemplateSpan], options: Options = NoOpt) extends Block with SpanContainer[TemplateRoot]
  
  case class Template (path: Path, content: TemplateRoot) {
    
    val name = path.name
    
  }
  

}