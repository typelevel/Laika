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

import laika.config.Key
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TemplateParsersSpec extends AnyFlatSpec 
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
    val input = "${document.content}"
    Parsing (input) should produce (spans(TemplateContextReference(Key("document","content"), required = true, generatedSource(input))))
    
  }
  
  it should "parse a reference at the beginning of a template" in {
    val ref = "${document.content}"
    val input = s"$ref some text"
    Parsing (input) should produce (spans(
      TemplateContextReference(Key("document","content"), required = true, source(ref, input)), 
      t(" some text")
    ))
  }
  
  it should "parse a reference at the end of a template" in {
    val ref = "${document.content}"
    val input = s"some text $ref"
    Parsing (input) should produce (spans(
      t("some text "), 
      TemplateContextReference(Key("document","content"), required = true, source(ref, input))
    ))
    
  }
  
  it should "parse a reference in the middle of a template" in {
    val ref = "${document.content}"
    val input = s"some text $ref some more"
    Parsing (input) should produce (spans(
      t("some text "), 
      TemplateContextReference(Key("document","content"), required = true, source(ref, input)), 
      t(" some more")
    ))
  }

  it should "parse an optional reference" in {
    val ref = "${?document.content}"
    val input = s"some text $ref some more"
    Parsing (input) should produce (spans(
      t("some text "), 
      TemplateContextReference(Key("document","content"), required = false, source(ref, input)), 
      t(" some more")
    ))
  }

  it should "detect an invalid reference" in {
    val errorMsg = """Invalid HOCON reference: '${document = content}': [1.22] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
                    |
                    |some text ${document = content} some more
                    |                     ^""".stripMargin

    val ref = "${document = content}"
    val input = s"some text $ref some more"
    Parsing (input) should produce (spans(t("some text "), 
      TemplateElement(InvalidElement(errorMsg, source(ref, input)).asSpan), 
      t(" some more")
    ))

  }

}
