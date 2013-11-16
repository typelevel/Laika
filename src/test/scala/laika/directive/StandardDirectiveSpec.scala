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

package laika.directive

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import laika.tree.helper.ModelBuilder
import laika.api.Parse
import laika.parse.markdown.Markdown
import laika.tree.Elements.BlockSequence
import laika.template.ParseTemplate
import laika.template.DefaultTemplate
import com.typesafe.config.ConfigFactory
import laika.tree.Templates.TemplateDocument
import laika.tree.Documents.Root
import laika.tree.Documents.DocumentContext
import laika.tree.Documents.Document
import scala.collection.JavaConversions._
import laika.tree.Templates.TemplateSpanSequence

class StandardDirectiveSpec extends FlatSpec
                            with ShouldMatchers
                            with ModelBuilder {

  
  def parse (input: String) = (Parse as Markdown fromString input)

  def parseWithFragments (input: String) = {
    val doc = parse(input)
    (doc.fragments, doc.content)
  }
  
  def parseTemplate (input: String) = (ParseTemplate as DefaultTemplate fromString input).content

  def parseTemplateWithConfig (input: String, config: String) = {
    val root = parseTemplate(input)
    val template = new TemplateDocument(Root, root)
    (template rewrite DocumentContext(new Document(Root, doc(), config = ConfigFactory.parseString(config)))).content
  }
  

  "The fragment directive" should "parse a fragment with a single paragraph" in {
    val input = """aa
      |
      |@:fragment foo:
      |  Fragment Text
      |
      |bb""".stripMargin
    parseWithFragments(input) should be ((Map("foo" -> BlockSequence(List(p("Fragment Text")))),doc(p("aa"),p("bb"))))
  }
  
  it should "parse a fragment with a two paragraphs" in {
    val input = """aa
      |
      |@:fragment foo:
      |  Line 1
      |
      |  Line 2
      |
      |bb""".stripMargin
    parseWithFragments(input) should be ((Map("foo" -> BlockSequence(List(p("Line 1"), p("Line 2")))),doc(p("aa"),p("bb"))))
  }
  
  
  "The for directive" should "process the default body once if the referenced object is a map" in {
    val input = """aaa @:for "config.person": { {{name}} {{age}} } bbb"""
    val config = "person: { name: Mary, age: 35 }" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(
        tt(" "),tt("Mary"),tt(" "),tt("35"),tt(" ")
      )),
      tt(" bbb")
    )))  
  } 
  
  it should "process the default body multiple times if the referenced object is a list" in {
    val input = """aaa @:for "config.persons": { {{name}} {{age}} } bbb"""
    val config = "persons: [{ name: Mary, age: 35 },{ name: Lucy, age: 32 },{ name: Anna, age: 42 }]" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(
        TemplateSpanSequence(List(
          tt(" "),tt("Mary"),tt(" "),tt("35"),tt(" ")
        )),
        TemplateSpanSequence(List(
          tt(" "),tt("Lucy"),tt(" "),tt("32"),tt(" ")
        )),
        TemplateSpanSequence(List(
          tt(" "),tt("Anna"),tt(" "),tt("42"),tt(" ")
        ))
      )),
      tt(" bbb")
    )))  
  } 
  
  it should "not process the default body if the referenced object is an empty collection" in {
    val input = """aaa @:for "config.persons": { {{name}} {{age}} } bbb"""
    val config = "persons: []" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(Nil),
      tt(" bbb")
    )))  
  } 
  
  it should "process the fallback body if the referenced object is an empty collection" in {
    val input = """aaa @:for "config.persons": { {{name}} {{age}} } ~empty: { none } bbb"""
    val config = "persons: []" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" none "))),
      tt(" bbb")
    )))  
  } 
  
  it should "process the default body once if the referenced object is a scalar value" in {
    val input = """aaa @:for "config.person": { text } bbb"""
    val config = "person: Mary" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" text "))),
      tt(" bbb")
    )))  
  } 
  
  "The if directive" should "process the default body once if the referenced object is the string 'true'" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "monday: true" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" text "))),
      tt(" bbb")
    )))  
  } 
  
  it should "process the default body once if the referenced object is the string 'on'" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "monday: on" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" text "))),
      tt(" bbb")
    )))  
  } 
  
  it should "not process the default body if the referenced object does not exist" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "tuesday: on" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(Nil),
      tt(" bbb")
    )))  
  } 
  
  it should "not process the default body if the referenced object is not a string recognized as true" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "monday: off" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(Nil),
      tt(" bbb")
    )))  
  } 
  
  it should "process the fallback body if the referenced object does not exist" in {
    val input = """aaa @:if "config.monday": { text } ~else: { none } bbb"""
    val config = "tuesday: on" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" none "))),
      tt(" bbb")
    )))  
  } 
  
  it should "process the fallback body if the referenced object is not a string recognized as true" in {
    val input = """aaa @:if "config.monday": { text } ~else: { none } bbb"""
    val config = "monday: off" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" none "))),
      tt(" bbb")
    )))  
  } 
  
  
}