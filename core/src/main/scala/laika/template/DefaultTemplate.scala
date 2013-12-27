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

package laika.template

import laika.io.Input
import laika.tree.Templates.TemplateDocument
import laika.directive.Directives.Templates
import laika.directive.StandardDirectives

/** The default (and currently only) parser implementation for templates.
 *  It supports the parsing of directives (starting with `@:`), of configuration
 *  sections at the start of the document (enclosed between `{%` and `%}`) and of
 *  context references (enclosed between `{{` and `}}`). Everything else is
 *  treated as normal text and rendered unchanged.
 *  
 *  @author Jens Halm
 */
class DefaultTemplate private (
    directives: List[Templates.Directive]) extends (Input => TemplateDocument) {

  
  private lazy val parser = new TemplateParsers.Templates with StandardDirectives {
    lazy val directiveMap  = Templates.toMap(stdTemplateDirectives) ++ Templates.toMap(directives)
    def getTemplateDirective (name: String) = directiveMap.get(name)
  }
  
  /** Adds the specified Laika directives and returns a new instance of the parser.
   * 
   *  Example:
   * 
   *  {{{
   *  val templates = DefaultTemplate withDirectives (
   *    Templates.create("ticket") {
   *      (attribute(Default) ~ attribute("param").optional) { (ticketNo, param) =>
   *        val base = "http://tickets.service.com/"+ticketNo
   *        val url = base + (param map (p => "&param="+p) getOrElse "")
   *        TemplateElement(ExternalLink(Seq(Text("Ticket "+ticketNo)), url, options = Styles("ticket")))
   *      }
   *    }
   *  )    
   * 
   *  Transform from Markdown to HTML withConfig RootDirectory("my-home")
   *      .withTemplates(ParseTemplate as templates)   
   *  }}}
   *  
   *  The code above registers a template directive that detects markup like
   *  `@:ticket 2356.` and turns it into an external link node for the
   *  URL `http://tickets.service.com/2356`.
   * 
   *  For more details on implementing Laika directives see [[laika.directive.Directives]].
   */ 
  def withDirectives (directives: Templates.Directive*) =
    new DefaultTemplate(this.directives ++ directives)      
  
  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  def apply (input: Input) = parser.parseTemplate(input.asParserInput, input.path)
  
}

/** The default template parser, with all standard directives but no
 *  custom directives installed .
 * 
 *  @author Jens Halm
 */
object DefaultTemplate extends DefaultTemplate(Nil)