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

package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.tree.Elements.Span
import laika.tree.helper.ModelBuilder
import laika.parse.rst.Elements._
import laika.tree.Elements._
import laika.parse.rst.TextRoles.TextRole
import laika.parse.rst.Directives._
import laika.parse.rst.Directives.Parts._
   
class DirectiveSpec extends FlatSpec 
                        with ShouldMatchers 
                        with BlockParsers 
                        with InlineParsers
                        with ParseResultHelpers 
                        with DefaultParserHelpers[Document] 
                        with ModelBuilder {

  
  val defaultParser: Parser[Document] = document
  
  
  def invalid (input: String) = Comment(input.drop(3).replaceAll("\n[ ]*", "\n"))
  
  def positiveInt (input: String) = 
    try {
      val num = input.toInt
      if (num > 0) Right(num) else Left("not a positive number: " + num)
    }
    catch { 
      case e: Exception => Left("unable to convert to int: " + e.toString) 
    }
    
  def intList (input: String) = {
    val list = input split "\n" filter (_.trim.nonEmpty) map (_.trim.toInt)
    if (list.nonEmpty) Right(list) else Left("no integers provided")
  }
  

  
  def stars (num: Int) = p("*" * num)
  
  val blockDirectives: Map[String, DirectivePart[Block]] = Map(
    "oneArg" -> (requiredStringArg map p),
    "twoArgs" -> (requiredStringArg ~ requiredStringArg) { (arg1,arg2) => p(arg1+arg2) },
    "oneIntArg" -> (requiredArg(positiveInt) map stars),
    "oneOptArg" -> (optionalStringArg map { arg => p(arg.getOrElse("missing")) }),
    "oneOptIntArg" -> (optionalArg(positiveInt) map { arg => p("*" * arg.getOrElse(1)) }),
    "reqAndOptArg" -> (requiredStringArg ~ optionalStringArg) { (arg1,arg2) => p(arg1+arg2.getOrElse("missing")) },
    "argWithWS" -> (requiredStringArg ~ requiredStringArgWithWS) { (arg1,arg2) => p(arg1+arg2) },
    "oneFd" -> (requiredStringField("name") map p),
    "oneIntFd" -> (requiredField("name",positiveInt) map stars),
    "twoFd" -> (requiredStringField("name1") ~ requiredStringField("name2")) { (arg1,arg2) => p(arg1+arg2) },
    "oneOptFd" -> (optionalStringField("name") map { arg => p(arg.getOrElse("missing")) }),
    "oneOptIntFd" -> (optionalField("name",positiveInt) map { arg => p("*" * arg.getOrElse(1)) }),
    "reqAndOptFd" -> (requiredStringField("name1") ~ optionalStringField("name2")) { (arg1,arg2) => p(arg1+arg2.getOrElse("missing")) },
    "argAndFd" -> (requiredArg(positiveInt) ~ requiredField("name",positiveInt)) { (arg1,arg2) => p(("*" * arg1) + ("#" * arg2)) },
    "stdBody" -> (standardContent map BlockSequence),
    "customBody" -> content(body => if (body.length > 10) Right(p(body)) else Left("body too short")),
    "argAndBody" -> (requiredStringArg ~ standardContent) { (arg,blocks) => BlockSequence(p(arg+"!") +: blocks) },
    "fdAndBody" -> (requiredStringField("name") ~ standardContent) { (field,blocks) => BlockSequence(p(field+"!") +: blocks) },
    "all" -> (requiredStringArg ~ requiredStringField("name") ~ standardContent) {
      (arg,field,blocks) => BlockSequence(p(arg+":"+field) +: blocks)
    },
    "allOpt" -> (optionalArg(positiveInt) ~ optionalField("name",positiveInt) ~ content(intList)) {
      (arg,field,list) => (p((arg.getOrElse(0) +: field.getOrElse(0) +: list).sum.toString))
    }
  )
  
  val spanDirectives: Map[String, DirectivePart[Span]] = Map(
    "spans" -> (requiredStringArg map Text) 
  )
  
  val textRoles: Map[String, TextRole] = Map(
    "role" -> TextRole("role", 7)(TextRoles.Parts.requiredField("name",positiveInt)) { (res,text) =>
       txt(text+"("+res+")")
    }  
  )
  
  
  
  "The directive parser" should "parse a directive with one required argument" in {
    val input = """.. oneArg:: arg"""
    Parsing (input) should produce (doc (p("arg")))
  }
  
  it should "ignore a directive with a required argument missing" in {
    val input = """.. oneArg::"""
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with a required argument being invalid" in {
    val input = """.. oneIntArg:: foo"""
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with a converted required argument" in {
    val input = """.. oneIntArg:: 7"""
    Parsing (input) should produce (doc (p("*" * 7)))
  }
  
  it should "parse a directive with two required arguments" in {
    val input = """.. twoArgs:: arg arg"""
    Parsing (input) should produce (doc (p("argarg")))
  }
  
  it should "ignore a directive with one out of two required arguments missing" in {
    val input = """.. twoArgs:: arg"""
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with one optional argument" in {
    val input = """.. oneOptArg:: arg"""
    Parsing (input) should produce (doc (p("arg")))
  }
  
  it should "parse a directive with one optional argument missing" in {
    val input = """.. oneOptArg::"""
    Parsing (input) should produce (doc (p("missing")))
  }
  
  it should "parse a directive with a converted optional argument" in {
    val input = """.. oneOptIntArg:: 7"""
    Parsing (input) should produce (doc (p("*" * 7)))
  }
  
  it should "ignore a directive with an optional argument being invalid" in {
    val input = """.. oneOptIntArg:: foo"""
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with one required and one optional argument" in {
    val input = """.. reqAndOptArg:: arg arg"""
    Parsing (input) should produce (doc (p("argarg")))
  }
  
  it should "parse a directive with one required and one missing optional argument" in {
    val input = """.. reqAndOptArg:: arg"""
    Parsing (input) should produce (doc (p("argmissing")))
  }
  
  it should "parse a directive with one regular argument and one argument with whitespace" in {
    val input = """.. argWithWS:: arg Line1 !
      | Line2""".stripMargin
    Parsing (input) should produce (doc (p("argLine1 !\nLine2")))
  }
  
  it should "ignore a directive with a required argument with whitespace missing" in {
    val input = """.. argWithWS:: arg"""
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with one required field" in {
    val input = """.. oneFd::
      | :name: arg""".stripMargin
    Parsing (input) should produce (doc (p("arg")))
  }
  
  it should "ignore a directive with a required field missing" in {
    val input = """.. oneFd::"""
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with a required field being invalid" in {
    val input = """.. oneIntFd::
      | :name: foo""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with a converted required field" in {
    val input = """.. oneIntFd::
      | :name: 7""".stripMargin
    Parsing (input) should produce (doc (p("*" * 7)))
  }
  
  it should "parse a directive with two required fields" in {
    val input = """.. twoFd::
      | :name1: value1
      | :name2: value2""".stripMargin
    Parsing (input) should produce (doc (p("value1value2")))
  }
  
  it should "ignore a directive with one out of two required fields missing" in {
    val input = """.. twoFd::
      | :name1: value1""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with one optional field" in {
    val input = """.. oneOptFd::
      | :name: arg""".stripMargin
    Parsing (input) should produce (doc (p("arg")))
  }
  
  it should "parse a directive with one optional field with a value spanning two lines" in {
    val input = """.. oneOptFd::
      | :name: arg
      |  arg""".stripMargin
    Parsing (input) should produce (doc (p("arg\narg")))
  }
  
  it should "parse a directive with one optional field missing" in {
    val input = """.. oneOptFd::"""
    Parsing (input) should produce (doc (p("missing")))
  }
  
  it should "parse a directive with a converted optional field" in {
    val input = """.. oneOptIntFd:: 
      | :name: 7""".stripMargin
    Parsing (input) should produce (doc (p("*" * 7)))
  }
  
  it should "ignore a directive with an optional field being invalid" in {
    val input = """.. oneOptIntFd::
      | :name: foo""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with one required and one optional field" in {
    val input = """.. reqAndOptFd::
      | :name1: value1
      | :name2: value2""".stripMargin
    Parsing (input) should produce (doc (p("value1value2")))
  }
  
  it should "parse a directive with one required and one missing optional field" in {
    val input = """.. reqAndOptFd::
      | :name1: value1""".stripMargin
    Parsing (input) should produce (doc (p("value1missing")))
  }
  
  it should "parse a directive with one converted argument and one converted field" in {
    val input = """.. argAndFd:: 3
      | :name: 5""".stripMargin
    Parsing (input) should produce (doc (p("***#####")))
  }
  
  it should "ignore a directive with one converted argument and one required field missing" in {
    val input = """.. argAndFd:: 3"""
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with one converted field and one required argument missing" in {
    val input = """.. argAndFd::
      | :name: 5""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with one converted argument and one field invalid" in {
    val input = """.. argAndFd:: 3
      | :name: foo""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with one converted field and one argument invalid" in {
    val input = """.. argAndFd:: foo
      | :name: 5""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with standard block content" in {
    val input = """.. stdBody:: Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (BlockSequence(List(p("Line 1"),p("Line 2")))))
  }
  
  it should "parse a directive with empty block content" in {
    val input = """.. stdBody::"""
    Parsing (input) should produce (doc (BlockSequence(Nil)))
  }
  
  it should "parse a directive with custom content" in {
    val input = """.. customBody:: Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (p("Line 1\n\nLine 2")))
  }
  
  it should "ignore a directive with invalid custom content" in {
    val input = """.. customBody:: 1
      | 
      | 2""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with an argument and standard block content" in {
    val input = """.. argAndBody:: arg
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (BlockSequence(List(p("arg!"),p("Line 1"),p("Line 2")))))
  }
  
  it should "parse a directive with an argument and empty block content" in {
    val input = """.. argAndBody:: arg"""
    Parsing (input) should produce (doc (BlockSequence(List(p("arg!")))))
  }
  
  it should "parse a directive with a field and standard block content" in {
    val input = """.. fdAndBody::
      | :name: value
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (BlockSequence(List(p("value!"),p("Line 1"),p("Line 2")))))
  }
  
  it should "parse a directive with a field and empty block content" in {
    val input = """.. fdAndBody::
      | :name: value""".stripMargin
    Parsing (input) should produce (doc (BlockSequence(List(p("value!")))))
  }
  
  it should "ignore a directive with standard block content and a missing required field" in {
    val input = """.. fdAndBody:: Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with an argument, a field and standard block content" in {
    val input = """.. all:: arg
      | :name: value
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (BlockSequence(List(p("arg:value"),p("Line 1"),p("Line 2")))))
  }
  
  it should "ignore a directive with a field and standard block content, but a missing required argument" in {
    val input = """.. all::
      | :name: value
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with an argument and standard block content, but a missing required field" in {
    val input = """.. all:: arg
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "parse a directive with an argument, a field and empty block content" in {
    val input = """.. all:: arg
      | :name: value""".stripMargin
    Parsing (input) should produce (doc (BlockSequence(List(p("arg:value")))))
  }
  
  it should "parse a directive with a converted argument, field and body" in {
    val input = """.. allOpt:: 2
      | :name: 3
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (doc (p("14")))
  }
  
  it should "parse a directive with a converted field and body and a missing optional argument" in {
    val input = """.. allOpt::
      | :name: 3
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (doc (p("12")))
  }
  
  it should "parse a directive with a converted argument and body and a missing optional field" in {
    val input = """.. allOpt:: 2
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (doc (p("11")))
  }
  
  it should "parse a directive with a converted body and missing optional argument and field" in {
    val input = """.. allOpt::
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (doc (p("9")))
  }
  
  it should "ignore a directive with a converted field and body and an invalid argument" in {
    val input = """.. allOpt:: foo
      | :name: 3
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with a converted argument and body and an invalid field" in {
    val input = """.. allOpt:: 2
      | :name: foo
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  it should "ignore a directive with a converted field and argument and an invalid body" in {
    val input = """.. allOpt:: 2
      | :name: 3""".stripMargin
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  
  "The substitution definition parser" should "parse a simple definition" in {
    val input = ".. |def| spans:: text"
    Parsing (input) should produce (doc (SubstitutionDefinition("def", txt("text"))))
  }
  
  it should "ignore a definition with an invalid directive" in {
    val input = ".. |def| spans::"
    Parsing (input) should produce (doc (invalid(input)))
  }
  
  
  val docWithTextRoles: Parser[Document] = document ^^ { doc =>
    doc.rewrite {
      case CustomizedTextRole(name, f) => Some(p(f(name)))
    }
  }
  
  "The role directive parser" should "parse a simple definition" in {
    val input = """.. role::custom(role)
    	| :name: 9""".stripMargin
    parseAll(docWithTextRoles,input) should produce (doc (p("custom(9)")))
  }

  
}