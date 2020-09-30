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

package laika.api

import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.Markdown
import laika.parse.markup.DocumentParser.ParserError
import laika.rewrite.TemplateRewriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class ParseAPISpec extends AnyFlatSpec 
                   with Matchers
                   with ModelBuilder {
  
  
  val parser: MarkupParser = MarkupParser.of(Markdown).build

  
  "The Parse API" should "allow parsing Markdown from a string" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    parser.parse(input).toOption.get.content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown with all link references resolved through the default rewrite rules" in {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    parser.parse(input).toOption.get.content should be (root(p(link(Text("link")).url("http://foo/"))))
  }
  
  it should "allow to set a config value programmatically" in {
    val input = "aa ${prop} bb"
    MarkupParser.of(Markdown).withConfigValue("prop", "foo").build.parse(input).map(_.content) should be (Right(root(p(
      Text("aa foo bb")
    ))))
  }
  
  it should "allow parsing Markdown into a raw document, without applying the default rewrite rules" in {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    MarkupParser.of(Markdown).build.parseUnresolved(input).toOption.get.document.content should be (root(
      p(LinkIdReference(List(Text("link")), "id", source("[link][id]", input))), 
      LinkDefinition("id", ExternalTarget("http://foo/"), None)
    ))
  }
  
  it should "collect errors from runtime messages" in {
    val input = """[invalid1]
                  |
                  |Text
                  |
                  |[invalid2]""".stripMargin
    val msg = """One or more error nodes in result:
                |  [1]: unresolved link id reference: invalid1
                |
                |  [invalid1]
                |  ^
                |
                |  [5]: unresolved link id reference: invalid2
                |
                |  [invalid2]
                |  ^""".stripMargin
    MarkupParser.of(Markdown).build.parse(input) shouldBe Left(ParserError(msg, Root / "doc"))
  }
  
  it should "replace unresolved nodes with invalid elements" in {
    val input = """[invalid1]
                  |
                  |Text
                  |
                  |[invalid2]""".stripMargin
    val doc = MarkupParser.of(Markdown).build.parseUnresolved(input).toOption.get.document
    doc.rewrite(TemplateRewriter.rewriteRules(DocumentCursor(doc))).content should be (root(
      p(InvalidSpan("Unresolved link id reference 'invalid1'", source("[invalid1]", input))),
      p("Text"),
      p(InvalidSpan("Unresolved link id reference 'invalid2'", source("[invalid2]", input))),
    ))
  }
  
}
