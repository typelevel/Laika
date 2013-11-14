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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.tree.helper.ModelBuilder
import laika.tree.Elements._
import laika.tree.Templates._

class TemplateParsersSpec extends FlatSpec 
                          with ShouldMatchers 
                          with TemplateParsers.Templates
                          with ParseResultHelpers 
                          with DefaultParserHelpers[List[Span]] 
                          with ModelBuilder {

  
  val defaultParser: Parser[List[Span]] = spans(any, spanParsers)
  
  def getTemplateDirective (name: String) = None
  
  
  
  // TODO - Test should use templateSpans as default parser

  
  "The template parser" should "parse content without any markup as plain text" in {
    
    Parsing ("some text") should produce (spans(txt("some text")))
    
  }
  
  
  "The context reference parser" should "parse a reference as the only template content" in {
    
    Parsing ("{{document.content}}") should produce (spans(TemplateContextReference("document.content")))
    
  }
  
  it should "parse a reference at the beginning of a template" in {
    
    Parsing ("{{document.content}} some text") should produce (spans(TemplateContextReference("document.content"), Text(" some text")))
    
  }
  
  it should "parse a reference at the end of a template" in {
    
    Parsing ("some text {{document.content}}") should produce (spans(Text("some text "), TemplateContextReference("document.content")))
    
  }
  
  it should "parse a reference in the middle of a template" in {
    
    Parsing ("some text {{document.content}} some more") should produce (spans(Text("some text "), TemplateContextReference("document.content"), Text(" some more")))
    
  }
  
  
}