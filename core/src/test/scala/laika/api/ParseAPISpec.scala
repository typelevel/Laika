/*
 * Copyright 2013-2016 the original author or authors.
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

import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.Markdown
import org.scalatest.{FlatSpec, Matchers}



class ParseAPISpec extends FlatSpec 
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
    parser.parse(input).toOption.get.content should be (root(p(link(txt("link")).url("http://foo/"))))
  }
  
  it should "allow parsing Markdown into a raw document, without applying the default rewrite rules" in {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    MarkupParser.of(Markdown).withoutRewrite.build.parse(input).toOption.get.content should be (root 
        (p (LinkReference(List(Text("link")), "id", "[link][id]")), ExternalLinkDefinition("id","http://foo/",None)))
  }
  
}
