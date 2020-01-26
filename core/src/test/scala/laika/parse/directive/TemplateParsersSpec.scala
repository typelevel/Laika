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

package laika.parse.directive

import cats.instances.string
import laika.config.Key
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import org.scalatest.{FlatSpec, Matchers}

class TemplateParsersSpec extends FlatSpec 
                          with Matchers 
                          with ParseResultHelpers
                          with DefaultParserHelpers[List[Span]] 
                          with ModelBuilder {


  val templateParsers = new TemplateParsers(Map())

  val defaultParser: Parser[List[Span]] = templateParsers.templateSpans
  

  
  "The template parser" should "parse content without any markup as plain text" in {
    
    Parsing ("some text") should produce (spans(t("some text")))
    
  }
  
  
  "The context reference parser" should "parse a reference as the only template content" in {
    
    Parsing ("${document.content}") should produce (spans(TemplateContextReference(Key("document","content"), required = true)))
    
  }
  
  it should "parse a reference at the beginning of a template" in {
    
    Parsing ("${document.content} some text") should produce (spans(TemplateContextReference(Key("document","content"), required = true), t(" some text")))
    
  }
  
  it should "parse a reference at the end of a template" in {
    
    Parsing ("some text ${document.content}") should produce (spans(t("some text "), TemplateContextReference(Key("document","content"), required = true)))
    
  }
  
  it should "parse a reference in the middle of a template" in {
    
    Parsing ("some text ${document.content} some more") should produce (spans(t("some text "), TemplateContextReference(Key("document","content"), required = true), t(" some more")))
    
  }

  it should "parse an optional reference" in {

    Parsing ("some text ${?document.content} some more") should produce (spans(t("some text "), TemplateContextReference(Key("document","content"), required = false), t(" some more")))

  }

  it should "detect an invalid reference" in {
    
    val errorMsg = """Invalid HOCON reference: 'document = content': [1.22] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
                    |
                    |some text ${document = content} some more
                    |                     ^""".stripMargin

    Parsing ("some text ${document = content} some more") should produce (spans(t("some text "), 
      TemplateElement(InvalidElement(errorMsg, "${document = content}").asSpan), 
      t(" some more")
    ))

  }

  it should "parse a legacy reference as the only template content" in {

    Parsing ("{{document.content}}") should produce (spans(TemplateContextReference(Key("document","content"), required = true)))

  }

  it should "parse a legacy reference at the beginning of a template" in {

    Parsing ("{{document.content}} some text") should produce (spans(TemplateContextReference(Key("document","content"), required = true), t(" some text")))

  }

  it should "parse a legacy reference at the end of a template" in {

    Parsing ("some text {{document.content}}") should produce (spans(t("some text "), TemplateContextReference(Key("document","content"), required = true)))

  }

  it should "parse a legacy reference in the middle of a template" in {

    Parsing ("some text {{document.content}} some more") should produce (spans(t("some text "), TemplateContextReference(Key("document","content"), required = true), t(" some more")))

  }
  
  
}
