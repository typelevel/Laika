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

package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.tree.helper.ModelBuilder
import laika.parse.rst.Elements._
import laika.tree.Elements._
import laika.parse.rst.TextRoles.TextRole
import laika.parse.rst.Directives._
import laika.parse.rst.Directives.Parts._
import laika.parse.rst.TextRoles.RoleDirectivePart
   
class DirectiveSpec extends FlatSpec 
                        with Matchers 
                        with BlockParsers 
                        with InlineParsers
                        with ParseResultHelpers 
                        with DefaultParserHelpers[RootElement] 
                        with ModelBuilder {

  
  val defaultParser: Parser[RootElement] = rootElement
  
  
  def invalid (input: String, error: String): InvalidBlock = 
    InvalidBlock(SystemMessage(laika.tree.Elements.Error, error), 
        LiteralBlock(input.replaceAll("\n ","\n").replaceAll("::$",":: ").replaceAll("::\n",":: \n")))
  
  def positiveInt (input: String): Either[String, Int] = 
    try {
      val num = input.toInt
      if (num > 0) Right(num) else Left(s"not a positive number: $num")
    }
    catch { 
      case e: Exception => Left(s"unable to convert to int: ${e.toString}") 
    }
    
  def intList (input: String): Either[String, Array[Int]] = {
    val list = input split "\n" filter (_.trim.nonEmpty) map (_.trim.toInt)
    if (list.nonEmpty) Right(list) else Left("no integers provided")
  }
  

  
  def stars (num: Int): Paragraph = p("*" * num)
  
  def toLowerCase [T] (tuple: (String, T)): (String, T) = (tuple._1.toLowerCase, tuple._2)
  
  val blockDirectives: Map[String, DirectivePart[Block]] = Map(
    "oneArg" -> (argument() map p),
    "twoArgs" -> (argument() ~ argument()) { (arg1,arg2) => p(arg1+arg2) },
    "oneIntArg" -> (argument(positiveInt) map stars),
    "oneOptArg" -> (optArgument() map { arg => p(arg.getOrElse("missing")) }),
    "oneOptIntArg" -> (optArgument(positiveInt) map { arg => p("*" * arg.getOrElse(1)) }),
    "reqAndOptArg" -> (argument() ~ optArgument()) { (arg1,arg2) => p(arg1+arg2.getOrElse("missing")) },
    "argWithWS" -> (argument() ~ argument(withWS = true)) { (arg1,arg2) => p(arg1+arg2) },
    "oneFd" -> (field("name") map p),
    "oneIntFd" -> (field("name",positiveInt) map stars),
    "twoFd" -> (field("name1") ~ field("name2")) { (arg1,arg2) => p(arg1+arg2) },
    "oneOptFd" -> (optField("name") map { arg => p(arg.getOrElse("missing")) }),
    "oneOptIntFd" -> (optField("name",positiveInt) map { arg => p("*" * arg.getOrElse(1)) }),
    "reqAndOptFd" -> (field("name1") ~ optField("name2")) { (arg1,arg2) => p(arg1+arg2.getOrElse("missing")) },
    "argAndFd" -> (argument(positiveInt) ~ field("name",positiveInt)) { (arg1,arg2) => p(("*" * arg1) + ("#" * arg2)) },
    "optArgAndFd" -> (optArgument() ~ optField("name")) { (arg1,arg2) => p(arg1.getOrElse("missing")+arg2.getOrElse("missing")) },
    "optArgFdBody" -> (optArgument(withWS = true) ~ optField("name") ~ content(Right(_))) { 
      (arg1,arg2,content) => p(arg1.getOrElse("missing")+arg2.getOrElse("missing")+content) },
    "optFdBody" -> (optField("name") ~ content(Right(_))) { 
      (field,content) => p(field.getOrElse("missing")+content) },
    "stdBody" -> (blockContent map (BlockSequence(_))),
    "customBody" -> content(body => if (body.length > 10) Right(p(body)) else Left("body too short")),
    "argAndBlocks" -> (argument() ~ blockContent) { (arg,blocks) => BlockSequence(p(arg+"!") +: blocks) },
    "argAndSpans" -> (argument() ~ spanContent) { (arg,spans) => Paragraph(txt(arg) +: spans) },
    "fdAndBody" -> (field("name") ~ blockContent) { (field,blocks) => BlockSequence(p(field+"!") +: blocks) },
    "all" -> (argument() ~ field("name") ~ blockContent) {
      (arg,field,blocks) => BlockSequence(p(s"$arg:$field") +: blocks)
    },
    "allOpt" -> (optArgument(positiveInt) ~ optField("name",positiveInt) ~ content(intList)) {
      (arg,field,list) => (p((arg.getOrElse(0) +: field.getOrElse(0) +: list).sum.toString))
    }
  ).map(toLowerCase)
  
  val spanDirectives: Map[String, DirectivePart[Span]] = Map(
    "spans" -> (argument() map (Text(_))) 
  )
  
  val textRoles: Map[String, TextRole] = Map(
    "role" -> TextRole("role", 7)(TextRoles.Parts.field("name",positiveInt)) { (res,text) =>
       txt(s"$text($res)")
    }  
  )
  
  def blockDirective (name: String): Option[DirectivePart[Block]] = blockDirectives.get(name)
  def spanDirective (name: String): Option[DirectivePart[Span]] = spanDirectives.get(name)
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = textRoles.get(name).map(_.part(this))
  
  
  
  "The directive parser" should "parse a directive with one required argument" in {
    val input = """.. oneArg:: arg"""
    Parsing (input) should produce (root (p("arg")))
  }
  
  it should "detect a directive with a required argument missing as invalid" in {
    val error = "missing required argument"
    val input = """.. oneArg::"""
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with a required argument being invalid" in {
    val error = """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneIntArg:: foo"""
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with a converted required argument" in {
    val input = """.. oneIntArg:: 7"""
    Parsing (input) should produce (root (p("*" * 7)))
  }
  
  it should "parse a directive with two required arguments" in {
    val input = """.. twoArgs:: arg arg"""
    Parsing (input) should produce (root (p("argarg")))
  }
  
  it should "detect a directive with one out of two required arguments missing as invalid" in {
    val error = "missing required argument"
    val input = """.. twoArgs:: arg"""
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with one optional argument" in {
    val input = """.. oneOptArg:: arg"""
    Parsing (input) should produce (root (p("arg")))
  }
  
  it should "parse a directive with one optional argument missing" in {
    val input = """.. oneOptArg::"""
    Parsing (input) should produce (root (p("missing")))
  }
  
  it should "parse a directive with a converted optional argument" in {
    val input = """.. oneOptIntArg:: 7"""
    Parsing (input) should produce (root (p("*" * 7)))
  }
  
  it should "detect a directive with an optional argument being invalid" in {
    val error = """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneOptIntArg:: foo"""
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with one required and one optional argument" in {
    val input = """.. reqAndOptArg:: arg arg"""
    Parsing (input) should produce (root (p("argarg")))
  }
  
  it should "parse a directive with one required and one missing optional argument" in {
    val input = """.. reqAndOptArg:: arg"""
    Parsing (input) should produce (root (p("argmissing")))
  }
  
  it should "parse a directive with one regular argument and one argument with whitespace" in {
    val input = """.. argWithWS:: arg Line1 !
      | Line2""".stripMargin
    Parsing (input) should produce (root (p("argLine1 !\nLine2")))
  }
  
  it should "detect a directive with a required argument with whitespace missing as invalid" in {
    val error = "missing required argument"
    val input = """.. argWithWS:: arg"""
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with one required field" in {
    val input = """.. oneFd::
      | :name: arg""".stripMargin
    Parsing (input) should produce (root (p("arg")))
  }
  
  it should "detect a directive with a required field missing as invalid" in {
    val error = """missing required options: name"""
    val input = """.. oneFd::"""
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with a required field being invalid" in {
    val error = """invalid option values: name: unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneIntFd::
      | :name: foo""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with a converted required field" in {
    val input = """.. oneIntFd::
      | :name: 7""".stripMargin
    Parsing (input) should produce (root (p("*" * 7)))
  }
  
  it should "parse a directive with two required fields" in {
    val input = """.. twoFd::
      | :name1: value1
      | :name2: value2""".stripMargin
    Parsing (input) should produce (root (p("value1value2")))
  }
  
  it should "detect a directive with one out of two required fields missing as invalid" in {
    val error = """missing required options: name2"""
    val input = """.. twoFd::
      | :name1: value1""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with an unknown field name as invalid" in {
    val error = "unknown options: name3"
    val input = """.. twoFd::
      | :name1: value1
      | :name2: value2
      | :name3: value3""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with one optional field" in {
    val input = """.. oneOptFd::
      | :name: arg""".stripMargin
    Parsing (input) should produce (root (p("arg")))
  }
  
  it should "parse a directive with one optional field with a value spanning two lines" in {
    val input = """.. oneOptFd::
      | :name: arg
      |  arg""".stripMargin
    Parsing (input) should produce (root (p("arg\narg")))
  }
  
  it should "parse a directive with one optional field missing" in {
    val input = """.. oneOptFd::"""
    Parsing (input) should produce (root (p("missing")))
  }
  
  it should "parse a directive with a converted optional field" in {
    val input = """.. oneOptIntFd:: 
      | :name: 7""".stripMargin
    Parsing (input) should produce (root (p("*" * 7)))
  }
  
  it should "detect a directive with an optional field being invalid" in {
    val error = """invalid option values: name: unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneOptIntFd::
      | :name: foo""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with one required and one optional field" in {
    val input = """.. reqAndOptFd::
      | :name1: value1
      | :name2: value2""".stripMargin
    Parsing (input) should produce (root (p("value1value2")))
  }
  
  it should "parse a directive with one required and one missing optional field" in {
    val input = """.. reqAndOptFd::
      | :name1: value1""".stripMargin
    Parsing (input) should produce (root (p("value1missing")))
  }
  
  it should "parse a directive with one converted argument and one converted field" in {
    val input = """.. argAndFd:: 3
      | :name: 5""".stripMargin
    Parsing (input) should produce (root (p("***#####")))
  }
  
  it should "detect a directive with one converted argument and one required field missing as invalid" in {
    val error = """missing required options: name"""
    val input = """.. argAndFd:: 3"""
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with one converted field and one required argument missing as invalid" in {
    val error = "missing required argument"
    val input = """.. argAndFd::
      | :name: 5""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with one converted argument and one field invalid" in {
    val error = """invalid option values: name: unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. argAndFd:: 3
      | :name: foo""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with one converted field and one argument invalid" in {
    val error = """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. argAndFd:: foo
      | :name: 5""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with one optional argument and one missing optional field" in {
    val input = """.. optArgAndFd:: arg""".stripMargin
    Parsing (input) should produce (root (p("argmissing")))
  }
  
  it should "parse a directive with one optional argument, one missing optional field and standard body" in {
    val input = """.. optArgFdBody:: arg
      |
      | Some Text""".stripMargin
    Parsing (input) should produce (root (p("argmissingSome Text")))
  }
  
  it should "parse a directive with one optional field and standard body" in {
    val input = """.. optFdBody::
      | :name: foo
      |
      | Some Text""".stripMargin
    Parsing (input) should produce (root (p("fooSome Text")))
  }
  
  it should "parse a directive with one missing optional field and standard body after a blank line" in {
    val input = """.. optFdBody::
      |
      | Some Text""".stripMargin
    Parsing (input) should produce (root (p("missingSome Text")))
  }
  
  it should "parse a directive with one missing optional field and standard body on the same line" in {
    val input = """.. optFdBody:: Some Text
      | Some More""".stripMargin
    Parsing (input) should produce (root (p("missingSome Text\nSome More")))
  }
  
  it should "parse a directive with standard block content" in {
    val input = """.. stdBody:: Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (BlockSequence(List(p("Line 1"),p("Line 2")))))
  }
  
  it should "parse a directive with empty block content" in {
    val input = """.. stdBody::"""
    Parsing (input) should produce (root (BlockSequence(Nil)))
  }
  
  it should "parse a directive with custom content" in {
    val input = """.. customBody:: Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (p("Line 1\n\nLine 2")))
  }
  
  it should "detect a directive with invalid custom content" in {
    val input = """.. customBody:: 1
      | 
      | 2""".stripMargin
    Parsing (input) should produce (root (invalid(input, "body too short")))
  }
  
  it should "parse a directive with an argument and standard block content" in {
    val input = """.. argAndBlocks:: arg
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (BlockSequence(List(p("arg!"),p("Line 1"),p("Line 2")))))
  }
  
  it should "parse a directive with an argument and empty block content" in {
    val input = """.. argAndBlocks:: arg"""
    Parsing (input) should produce (root (BlockSequence(List(p("arg!")))))
  }
  
  it should "parse a directive with an argument and standard span content" in {
    val input = """.. argAndSpans:: arg
      |
      | Line 1
      | Line 2""".stripMargin
    Parsing (input) should produce (root (p(txt("arg"),txt("Line 1\nLine 2"))))
  }
  
  it should "parse a directive with an argument and empty span content" in {
    val input = """.. argAndSpans:: arg"""
    Parsing (input) should produce (root (p(txt("arg"))))
  }
  
  it should "parse a directive with a field and standard block content" in {
    val input = """.. fdAndBody::
      | :name: value
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (BlockSequence(List(p("value!"),p("Line 1"),p("Line 2")))))
  }
  
  it should "parse a directive with a field and empty block content" in {
    val input = """.. fdAndBody::
      | :name: value""".stripMargin
    Parsing (input) should produce (root (BlockSequence(List(p("value!")))))
  }
  
  it should "detect a directive with standard block content and a missing required field as invalid" in {
    val error = "missing required options: name"
    val input = """.. fdAndBody:: Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with an argument, a field and standard block content" in {
    val input = """.. all:: arg
      | :name: value
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (BlockSequence(List(p("arg:value"),p("Line 1"),p("Line 2")))))
  }
  
  it should "detect a directive with a field and standard block content, but a missing required argument as invalid" in {
    val error = "missing required argument"
    val input = """.. all::
      | :name: value
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with an argument and standard block content, but a missing required field as invalid" in {
    val error = "missing required options: name"
    val input = """.. all:: arg
      |
      | Line 1
      | 
      | Line 2""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "parse a directive with an argument, a field and empty block content" in {
    val input = """.. all:: arg
      | :name: value""".stripMargin
    Parsing (input) should produce (root (BlockSequence(List(p("arg:value")))))
  }
  
  it should "parse a directive with a converted argument, field and body" in {
    val input = """.. allOpt:: 2
      | :name: 3
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (root (p("14")))
  }
  
  it should "parse a directive with a converted field and body and a missing optional argument" in {
    val input = """.. allOpt::
      | :name: 3
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (root (p("12")))
  }
  
  it should "parse a directive with a converted argument and body and a missing optional field" in {
    val input = """.. allOpt:: 2
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (root (p("11")))
  }
  
  it should "parse a directive with a converted body and missing optional argument and field" in {
    val input = """.. allOpt::
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (root (p("9")))
  }
  
  it should "detect a directive with a converted field and body and an invalid argument" in {
    val error = """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. allOpt:: foo
      | :name: 3
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with a converted argument and body and an invalid field" in {
    val error = """invalid option values: name: unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. allOpt:: 2
      | :name: foo
      |
      | 4
      | 
      | 5""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with a converted field and argument and an invalid body" in {
    val error = "no integers provided"
    val input = """.. allOpt:: 2
      | :name: 3""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  it should "detect a directive with an unknown name as invalid" in {
    val error = "unknown directive: foo"
    val input = """.. foo::
      | :name: 3""".stripMargin
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  
  "The substitution definition parser" should "parse a simple definition" in {
    val input = ".. |def| spans:: text"
    Parsing (input) should produce (root (SubstitutionDefinition("def", txt("text"))))
  }
  
  it should "detect a definition with an invalid directive" in {
    val error = "missing required argument"
    val input = ".. |def| spans::"
    Parsing (input) should produce (root (invalid(input,error)))
  }
  
  
  val docWithTextRoles: Parser[RootElement] = defaultParser ^^ { root =>
    root.rewrite { 
      case CustomizedTextRole(name, f, _) => Some(p(f(name)))
    }
  }
  
  "The role directive parser" should "parse a simple definition" in {
    val input = """.. role::custom(role)
    	| :name: 9""".stripMargin
    parseAll(docWithTextRoles,input) should produce (root (p("custom(9)")))
  }
  
  it should "detect a defintion with a missing required field as invalid" in {
    val error = "missing required options: name"
    val input = """.. role::custom(role)"""
    Parsing(input) should produce (root (invalid(input+" ",error)))
  }

  
}
