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

package laika.ast

import laika.api.Parse
import laika.ast.helper.{InputBuilder, ModelBuilder}
import laika.bundle.BundleProvider
import laika.format.{Markdown, ReStructuredText}
import laika.rewrite.TemplateRewriter
import org.scalatest.{FlatSpec, Matchers}

class ConfigSpec extends FlatSpec 
                    with Matchers
                    with ModelBuilder {


  trait Inputs extends InputBuilder  {
    val markup = """{% foo: bar %}
      |aaa
      |bbb""".stripMargin
      
    val markupWithRef = """aaa
      |{{config.foo}}
      |bbb""".stripMargin
      
    val templateWithRef = """<h1>{{config.foo}}</h1>
      |<div>{{document.content}}</div>
      |CCC""".stripMargin
      
    val templateWithConfig= """{% foo: bar %}
      |<div>{{document.content}}</div>
      |CCC""".stripMargin
      
    def builder (source: String) = parseTreeStructure(source)
    
    lazy val contents = Map(
      "templateWithRef" -> templateWithRef,
      "templateWithConfig" -> templateWithConfig,
      "markup" -> markup,
      "markupWithRef" -> markupWithRef
    )
    
    def resultOf (tree: DocumentTree) = {
      val result = TemplateRewriter.applyTemplates(tree, "html")
      result.content.collect{case doc: Document => doc}.head.content
    }
    
  }
  
  "The Config parser" should "parse configuration sections embedded in Markdown documents" in {
    new Inputs {
      val dir = """- default.template.html:templateWithRef
        |- input.md:markup""".stripMargin
      val expected = root(
        TemplateRoot(List(
          TemplateString("<h1>"),
          TemplateString("bar"),
          TemplateString("</h1>\n<div>"),
          eRoot(p("aaa\nbbb")),
          TemplateString("</div>\nCCC")
        ))
      )
      resultOf(Parse as Markdown fromInputTree builder(dir)) should be (expected)
    }
  }
  
  it should "parse configuration sections embedded in reStructuredText documents" in {
    new Inputs {
      val dir = """- default.template.html:templateWithRef
        |- input.rst:markup""".stripMargin
      val expected = root(
        TemplateRoot(List(
          TemplateString("<h1>"),
          TemplateString("bar"),
          TemplateString("</h1>\n<div>"),
          eRoot(p("aaa\nbbb")),
          TemplateString("</div>\nCCC")
        ))
      )
      resultOf(Parse as ReStructuredText fromInputTree builder(dir)) should be (expected)
    }
  }
  
  it should "parse configuration sections embedded in template documents for Markdown" in {
    new Inputs {
      val dir = """- default.template.html:templateWithConfig
        |- input.md:markupWithRef""".stripMargin
      val expected = root(
        TemplateRoot(List(
          TemplateString("<div>"),
          eRoot(p(txt("aaa\n"),txt("bar"),txt("\nbbb"))),
          TemplateString("</div>\nCCC")
        ))
      )
      resultOf(Parse as Markdown fromInputTree builder(dir)) should be (expected)
    }
  }
  
  it should "parse configuration sections embedded in template documents for reStructuredText" in {
    new Inputs {
      val dir = """- default.template.html:templateWithConfig
        |- input.rst:markupWithRef""".stripMargin
      val expected = root(
        TemplateRoot(List(
          TemplateString("<div>"),
          eRoot(p(txt("aaa\n"),txt("bar"),txt("\nbbb"))),
          TemplateString("</div>\nCCC")
        ))
      )
      resultOf(Parse as ReStructuredText fromInputTree builder(dir)) should be (expected)
    }
  }
  
  it should "merge configuration found in documents, templates, directories and programmatic setup" in {
    new Inputs {
      override lazy val contents = Map(
        "template" -> template,
        "markup" -> md,
        "config3" -> config3,
        "config4" -> config4
      )
      val md = """{% key1: val1 %}
        |aaa""".stripMargin
      val template = """{% key2: val2 %}
        |{{config.key1}}
        |{{config.key2}}
        |{{config.key3}}
        |{{config.key4}}
        |{{config.key5}}""".stripMargin
      val dirs = """- directory.conf:config4
          |+ dir
          |  - default.template.html:template
          |  - directory.conf:config3
          |  - input.md:markup""".stripMargin
      val config3 = "key3: val3"
      val config4 = "key4: val4"
      val config5 = "key5: val5"
        
      val expected = root(
        TemplateRoot(
          (1 to 5) map (n => List(TemplateString("val"+n))) reduce (_ ++ List(TemplateString("\n")) ++ _)
        )
      )
      
      val tree = Parse.as(Markdown).using(BundleProvider.forConfigString(config5)).fromInputTree(builder(dirs))
      val result = TemplateRewriter.applyTemplates(tree, "html")
      result.selectDocument(Path.Current / "dir" / "input.md").get.content should be (expected)
    }
  }
  
  
}
