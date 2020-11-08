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

package laika.directive.std

import laika.ast.helper.ModelBuilder
import laika.ast.{RootElement, TemplateRoot, TemplateSpan, TemplateSpanSequence, TemplateString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ControlFlowDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder
  with TemplateParserSetup {

  def root (spans: TemplateSpan*): RootElement = RootElement(TemplateRoot(spans))
  
  def result (span: TemplateSpan): RootElement = RootElement(TemplateRoot(
    TemplateString("aaa "),
    span,
    TemplateString(" bbb")
  ))
  
  def surroundedBySpaces (text1: String, text2: String): TemplateSpan =
    TemplateSpanSequence(
      TemplateString(" "),
      TemplateString(text1),
      TemplateString(" "),
      TemplateString(text2),
      TemplateString(" ")
    )
  
  "The for directive" should "process the default body once if the referenced object is a map" in {
    val input = """aaa @:for(person) ${_.name} ${_.age} @:@ bbb"""
    val config = "person: { name: Mary, age: 35 }"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      surroundedBySpaces("Mary", "35")
    ))
  }

  it should "process the default body multiple times if the referenced object is a list" in {
    val input = """aaa @:for(persons) ${_.name} ${_.age} @:@ bbb"""
    val config = "persons: [{ name: Mary, age: 35 },{ name: Lucy, age: 32 },{ name: Anna, age: 42 }]"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence(
        surroundedBySpaces("Mary", "35"),
        surroundedBySpaces("Lucy", "32"),
        surroundedBySpaces("Anna", "42")
      )
    ))
  }

  it should "not process the default body if the referenced object is an empty collection" in {
    val input = """aaa @:for(persons) ${_.name} ${_.age} @:@ bbb"""
    val config = "persons: []"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence.empty
    ))
  }

  it should "process the @:empty body part if the referenced object is an empty collection" in {
    val input = """aaa @:for(persons) ${_.name} ${_.age} @:empty none @:@ bbb"""
    val config = "persons: []"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence(" none ")
    ))
  }

  it should "process the default body once if the referenced object is a scalar value" in {
    val input = """aaa @:for(person) ${_} @:@ bbb"""
    val config = "person: Mary"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence(TemplateString(" "), TemplateString("Mary"), TemplateString(" "))
    ))
  }

  "The if directive" should "process the default body once if the referenced object is the string 'true'" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "monday: true"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence(" text ")
    ))
  }

  it should "process the default body once if the referenced object is the string 'on'" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "monday: on"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence(" text ")
    ))
  }

  it should "not process the default body if the referenced object does not exist" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "tuesday: on"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence.empty
    ))
  }

  it should "process the @:else body if the referenced object does not exist" in {
    val input = """aaa @:if(monday) text @:else none @:@ bbb"""
    val config = "tuesday: on"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence(" none ")
    ))
  }

  it should "process the first @:elseIf body if it is defined" in {
    val input = """111 @:if(aaa) aaa @:elseIf(bbb) bbb @:elseIf(ccc) ccc @:else none @:@ 222"""
    val config = "bbb: on"
    parseTemplateWithConfig(input, config) shouldBe Right(root(
      TemplateString("111 "),
      TemplateSpanSequence(" bbb "),
      TemplateString(" 222")
    ))
  }

  it should "process the second @:elseIf body if it is defined" in {
    val input = """111 @:if(aaa) aaa @:elseIf(bbb) bbb @:elseIf(ccc) ccc @:else none @:@ 222"""
    val config = "ccc: on"
    parseTemplateWithConfig(input, config) shouldBe Right(root(
      TemplateString("111 "),
      TemplateSpanSequence(" ccc "),
      TemplateString(" 222")
    ))
  }

  it should "not process the default body if the referenced object is not a string recognized as true" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "monday: off"
    parseTemplateWithConfig(input, config) shouldBe Right(result(
      TemplateSpanSequence.empty
    ))
  }
  
  
}
