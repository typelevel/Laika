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

import laika.ast._
import laika.ast.sample.TestSourceBuilders
import laika.config.Key
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec

class TemplateParsersSpec extends MigrationFlatSpec
                             with TestSourceBuilders {


  val defaultParser: Parser[List[Span]] = new TemplateParsers(Map()).templateSpans

  def parse (input: String): Either[String, List[Span]] = defaultParser.parse(input).toEither

  def result (spans: Span*): Either[String, List[Span]] = Right(spans.toList)

  
  "The template parser" should "parse content without any markup as plain text" in {
    assertEquals(parse("some text"), result(TemplateString("some text")))
  }
  
  
  "The context reference parser" should "parse a reference as the only template content" in {
    val input = "${document.content}"
    assertEquals(parse(input), result(TemplateContextReference(Key("document","content"), required = true, generatedSource(input))))
    
  }
  
  it should "parse a reference at the beginning of a template" in {
    val ref = "${document.content}"
    val input = s"$ref some text"
    assertEquals(parse(input), result(
      TemplateContextReference(Key("document","content"), required = true, source(ref, input)), 
      TemplateString(" some text")
    ))
  }
  
  it should "parse a reference at the end of a template" in {
    val ref = "${document.content}"
    val input = s"some text $ref"
    assertEquals(parse(input), result(
      TemplateString("some text "), 
      TemplateContextReference(Key("document","content"), required = true, source(ref, input))
    ))
    
  }
  
  it should "parse a reference in the middle of a template" in {
    val ref = "${document.content}"
    val input = s"some text $ref some more"
    assertEquals(parse(input), result(
      TemplateString("some text "), 
      TemplateContextReference(Key("document","content"), required = true, source(ref, input)), 
      TemplateString(" some more")
    ))
  }

  it should "parse an optional reference" in {
    val ref = "${?document.content}"
    val input = s"some text $ref some more"
    assertEquals(parse(input), result(
      TemplateString("some text "), 
      TemplateContextReference(Key("document","content"), required = false, source(ref, input)), 
      TemplateString(" some more")
    ))
  }

  it should "detect an invalid reference" in {
    val errorMsg = """Invalid HOCON reference: '${document = content}': [1.22] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
                    |
                    |some text ${document = content} some more
                    |                     ^""".stripMargin

    val ref = "${document = content}"
    val input = s"some text $ref some more"
    assertEquals(parse(input), result(TemplateString("some text "), 
      TemplateElement(InvalidSpan(errorMsg, source(ref, input))), 
      TemplateString(" some more")
    ))

  }

}
