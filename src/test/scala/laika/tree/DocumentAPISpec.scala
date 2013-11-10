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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import laika.tree.helper.ModelBuilder
import laika.api.Parse
import laika.parse.markdown.Markdown
import laika.tree.Elements._

class DocumentAPISpec extends FlatSpec 
                      with ShouldMatchers
                      with ModelBuilder {

  
  "The Document API" should "allow to specify a title in a config section" in {
    val markup = """<% title: Foo and Bar %>
      |
      |# Ignored Title
      |
      |Some text
      |
      |## Section
      |
      |Some more text""".stripMargin
    
    (Parse as Markdown fromString markup).title should be (List(txt("Foo and Bar")))
  }
  
  it should "use the title from the first headline if it is not overridden in a config section" in {
    val markup = """# Title
      |
      |Some text
      |
      |## Section
      |
      |Some more text""".stripMargin
    
    (Parse as Markdown fromString markup).title should be (List(txt("Title")))
  }
  
  it should "return an empty title if there is neither a structure with a title nor a title in a config section" in {
    val markup = """# Section 1
      |
      |Some text
      |
      |# Section 2
      |
      |Some more text""".stripMargin
    
    (Parse as Markdown fromString markup).title should be (List(txt("")))
  }
  
  it should "produce the same result when rewriting a document once or twice" in {
    val markup = """# Section 1
      |
      |Some text
      |
      |# Section 2
      |
      |Some more text""".stripMargin
    
    val doc = Parse as Markdown fromString markup
    val rewritten = doc.rewrite
    rewritten.content should be (doc.content)
  }
  
  it should "allow to remove the default rewrite rules from a raw document" in {
    val markup = """# Section 1
      |
      |Some text
      |
      |# Section 2
      |
      |Some more text""".stripMargin
    
    val doc = (Parse as Markdown asRawDocument) fromString markup
    val rewritten = doc.removeRules.rewrite
    rewritten.content should be (doc.content)
  }
  
  it should "allow to rewrite the document using a custom rule" in {
    val markup = """# Section 1
      |
      |Some text
      |
      |# Section 2
      |
      |Some more text""".stripMargin
    
    val raw = (Parse as Markdown asRawDocument) fromString markup
    val rewritten = raw rewrite {
      case Text("Some text",_) => Some(Text("Swapped"))
    }
    rewritten.content should be (doc(
      Section(Header(1, List(Text("Section 1")), Id("section-1") + Styles("section")), List(p("Swapped"))),
      Section(Header(1, List(Text("Section 2")), Id("section-2") + Styles("section")), List(p("Some more text")))
    ))
  }
  
  
}