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

package laika.internal.rst.ext

import laika.ast.*
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.internal.rst.ast.{ CustomizedTextRole, SubstitutionDefinition }
import laika.parse.builders.~
import laika.parse.combinator.Parsers
import laika.parse.{ Parser, SourceFragment }
import laika.internal.rst.ext.Directives.Parts.*
import laika.internal.rst.ext.Directives.*
import laika.internal.rst.ext.TextRoles.TextRole
import munit.FunSuite

class DirectiveSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {

  val stringContent: DirectivePartBuilder[String] = content(src => Right(src.input))

  val blockDirectives: Seq[Directive[Block]] = Seq(
    BlockDirective("oneArg")(argument() map p),
    BlockDirective("twoArgs")((argument() ~ argument()).map { case arg1 ~ arg2 => p(arg1 + arg2) }),
    BlockDirective("oneIntArg")(argument(positiveInt) map stars),
    BlockDirective("oneOptArg")(optArgument() map { arg => p(arg.getOrElse("missing")) }),
    BlockDirective("oneOptIntArg")(optArgument(positiveInt) map { arg =>
      p("*" * arg.getOrElse(1))
    }),
    BlockDirective("reqAndOptArg")((argument() ~ optArgument()).map { case arg1 ~ arg2 =>
      p(arg1 + arg2.getOrElse("missing"))
    }),
    BlockDirective("argWithWS")((argument() ~ argument(withWS = true)).map { case arg1 ~ arg2 =>
      p(arg1 + arg2)
    }),
    BlockDirective("oneFd")(field("name") map p),
    BlockDirective("oneIntFd")(field("name", positiveInt) map stars),
    BlockDirective("twoFd")((field("name1") ~ field("name2")).map { case arg1 ~ arg2 =>
      p(arg1 + arg2)
    }),
    BlockDirective("oneOptFd")(optField("name") map { arg => p(arg.getOrElse("missing")) }),
    BlockDirective("oneOptIntFd")(optField("name", positiveInt) map { arg =>
      p("*" * arg.getOrElse(1))
    }),
    BlockDirective("reqAndOptFd")((field("name1") ~ optField("name2")).map { case arg1 ~ arg2 =>
      p(arg1 + arg2.getOrElse("missing"))
    }),
    BlockDirective("argAndFd")((argument(positiveInt) ~ field("name", positiveInt)).map {
      case arg1 ~ arg2 => p(("*" * arg1) + ("#" * arg2))
    }),
    BlockDirective("optArgAndFd")((optArgument() ~ optField("name")).map { case arg1 ~ arg2 =>
      p(arg1.getOrElse("missing") + arg2.getOrElse("missing"))
    }),
    BlockDirective("optArgFdBody")(
      (optArgument(withWS = true) ~ optField("name") ~ stringContent).map {
        case arg1 ~ arg2 ~ content =>
          p(arg1.getOrElse("missing") + arg2.getOrElse("missing") + content)
      }
    ),
    BlockDirective("optFdBody")((optField("name") ~ stringContent).map { case field ~ content =>
      p(field.getOrElse("missing") + content)
    }),
    BlockDirective("stdBody")(blockContent map (BlockSequence(_))),
    BlockDirective("customBody")(
      content(body => if (body.length > 10) Right(p(body.input)) else Left("body too short"))
    ),
    BlockDirective("argAndBlocks")((argument() ~ blockContent).map { case arg ~ blocks =>
      BlockSequence(p(arg + "!") +: blocks)
    }),
    BlockDirective("argAndSpans")((argument() ~ spanContent).map { case arg ~ spans =>
      Paragraph(Text(arg) +: spans)
    }),
    BlockDirective("fdAndBody")((field("name") ~ blockContent).map { case field ~ blocks =>
      BlockSequence(p(field + "!") +: blocks)
    }),
    BlockDirective("all")((argument() ~ field("name") ~ blockContent).map {
      case arg ~ field ~ blocks => BlockSequence(p(s"$arg:$field") +: blocks)
    }),
    BlockDirective("allOpt")(
      (optArgument(positiveInt) ~ optField("name", positiveInt) ~ content(intList)).map {
        case arg ~ field ~ list => p((arg.getOrElse(0) +: field.getOrElse(0) +: list).sum.toString)
      }
    )
  )

  val spanDirectives: Seq[Directive[Span]] = Seq(
    SpanDirective("spans")(argument() map (Text(_)))
  )

  val textRoles: Seq[TextRole] = Seq(
    TextRole("role", 7)(TextRoles.Parts.field("name", positiveInt)) { (res, text) =>
      Text(s"$text($res)")
    }
  )

  val defaultParser: Parser[RootElement] =
    RootParserProvider.forBundle(
      ExtensionProvider.forExtensions(blockDirectives, spanDirectives, textRoles)
    ).rootElement

  def invalid(input: String, error: String): InvalidBlock =
    InvalidBlock(error, generatedSource(input))

  def positiveInt(src: SourceFragment): Either[String, Int] =
    try {
      val num = src.input.toInt
      if (num > 0) Right(num) else Left(s"not a positive number: $num")
    }
    catch {
      case e: Exception => Left(s"unable to convert to int: ${e.toString}")
    }

  def intList(src: SourceFragment): Either[String, Array[Int]] = {
    val list = src.input.split("\n").filter(_.trim.nonEmpty).map(_.trim.toInt)
    if (list.nonEmpty) Right(list) else Left("no integers provided")
  }

  def stars(num: Int): Paragraph = p("*" * num)

  def toLowerCase[T](tuple: (String, T)): (String, T) = (tuple._1.toLowerCase, tuple._2)

  def run(input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

  test("directive with one required argument") {
    val input = """.. oneArg:: arg"""
    run(input, p("arg"))
  }

  test("failure - directive with a required argument missing as invalid") {
    val error = "missing required argument"
    val input = """.. oneArg::"""
    run(input, invalid(input, error))
  }

  test("failure - directive with a required argument being invalid") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneIntArg:: foo"""
    run(input, invalid(input, error))
  }

  test("directive with a converted required argument") {
    val input = """.. oneIntArg:: 7"""
    run(input, p("*" * 7))
  }

  test("directive with two required arguments") {
    val input = """.. twoArgs:: arg arg"""
    run(input, p("argarg"))
  }

  test("failure - directive with one out of two required arguments missing as invalid") {
    val error = "missing required argument"
    val input = """.. twoArgs:: arg"""
    run(input, invalid(input, error))
  }

  test("directive with one optional argument") {
    val input = """.. oneOptArg:: arg"""
    run(input, p("arg"))
  }

  test("directive with one optional argument missing") {
    val input = """.. oneOptArg::"""
    run(input, p("missing"))
  }

  test("directive with a converted optional argument") {
    val input = """.. oneOptIntArg:: 7"""
    run(input, p("*" * 7))
  }

  test("failure - directive with an optional argument being invalid") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneOptIntArg:: foo"""
    run(input, invalid(input, error))
  }

  test("directive with one required and one optional argument") {
    val input = """.. reqAndOptArg:: arg arg"""
    run(input, p("argarg"))
  }

  test("directive with one required and one missing optional argument") {
    val input = """.. reqAndOptArg:: arg"""
    run(input, p("argmissing"))
  }

  test("directive with one regular argument and one argument with whitespace") {
    val input = """.. argWithWS:: arg Line1 !
                  | Line2""".stripMargin
    run(input, p("argLine1 !\nLine2"))
  }

  test("failure - directive with a required argument with whitespace missing as invalid") {
    val error = "missing required argument"
    val input = """.. argWithWS:: arg"""
    run(input, invalid(input, error))
  }

  test("directive with one required field") {
    val input = """.. oneFd::
                  | :name: arg""".stripMargin
    run(input, p("arg"))
  }

  test("failure - directive with a required field missing as invalid") {
    val error = """missing required options: name"""
    val input = """.. oneFd::"""
    run(input, invalid(input, error))
  }

  test("failure - directive with a required field being invalid") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneIntFd::
                  | :name: foo""".stripMargin
    run(input, invalid(input, error))
  }

  test("directive with a converted required field") {
    val input = """.. oneIntFd::
                  | :name: 7""".stripMargin
    run(input, p("*" * 7))
  }

  test("directive with two required fields") {
    val input = """.. twoFd::
                  | :name1: value1
                  | :name2: value2""".stripMargin
    run(input, p("value1value2"))
  }

  test("failure - directive with one out of two required fields missing as invalid") {
    val error = """missing required options: name2"""
    val input = """.. twoFd::
                  | :name1: value1""".stripMargin
    run(input, invalid(input, error))
  }

  test("failure - directive with an unknown field name as invalid") {
    val error = "unknown options: name3"
    val input = """.. twoFd::
                  | :name1: value1
                  | :name2: value2
                  | :name3: value3""".stripMargin
    run(input, invalid(input, error))
  }

  test("directive with one optional field") {
    val input = """.. oneOptFd::
                  | :name: arg""".stripMargin
    run(input, p("arg"))
  }

  test("directive with one optional field with a value spanning two lines") {
    val input = """.. oneOptFd::
                  | :name: arg
                  |  arg""".stripMargin
    run(input, p("arg\narg"))
  }

  test("directive with one optional field missing") {
    val input = """.. oneOptFd::"""
    run(input, p("missing"))
  }

  test("directive with a converted optional field") {
    val input = """.. oneOptIntFd:: 
                  | :name: 7""".stripMargin
    run(input, p("*" * 7))
  }

  test("failure - directive with an optional field being invalid") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. oneOptIntFd::
                  | :name: foo""".stripMargin
    run(input, invalid(input, error))
  }

  test("directive with one required and one optional field") {
    val input = """.. reqAndOptFd::
                  | :name1: value1
                  | :name2: value2""".stripMargin
    run(input, p("value1value2"))
  }

  test("directive with one required and one missing optional field") {
    val input = """.. reqAndOptFd::
                  | :name1: value1""".stripMargin
    run(input, p("value1missing"))
  }

  test("directive with one converted argument and one converted field") {
    val input = """.. argAndFd:: 3
                  | :name: 5""".stripMargin
    run(input, p("***#####"))
  }

  test(
    "failure - directive with one converted argument and one required field missing as invalid"
  ) {
    val error = """missing required options: name"""
    val input = """.. argAndFd:: 3"""
    run(input, invalid(input, error))
  }

  test(
    "failure - directive with one converted field and one required argument missing as invalid"
  ) {
    val error = "missing required argument"
    val input = """.. argAndFd::
                  | :name: 5""".stripMargin
    run(input, invalid(input, error))
  }

  test("failure - directive with one converted argument and one field invalid") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. argAndFd:: 3
                  | :name: foo""".stripMargin
    run(input, invalid(input, error))
  }

  test("failure - directive with one converted field and one argument invalid") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. argAndFd:: foo
                  | :name: 5""".stripMargin
    run(input, invalid(input, error))
  }

  test("directive with one optional argument and one missing optional field") {
    val input = """.. optArgAndFd:: arg""".stripMargin
    run(input, p("argmissing"))
  }

  test("directive with one optional argument, one missing optional field and standard body") {
    val input = """.. optArgFdBody:: arg
                  |
                  | Some Text""".stripMargin
    run(input, p("argmissingSome Text"))
  }

  test("directive with one optional field and standard body") {
    val input = """.. optFdBody::
                  | :name: foo
                  |
                  | Some Text""".stripMargin
    run(input, p("fooSome Text"))
  }

  test("directive with one missing optional field and standard body after a blank line") {
    val input = """.. optFdBody::
                  |
                  | Some Text""".stripMargin
    run(input, p("missingSome Text"))
  }

  test("directive with one missing optional field and standard body on the same line") {
    val input = """.. optFdBody:: Some Text
                  | Some More""".stripMargin
    run(input, p("missingSome Text\nSome More"))
  }

  test("directive with standard block content") {
    val input = """.. stdBody:: Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, BlockSequence(p("Line 1"), p("Line 2")))
  }

  test("directive with empty block content") {
    val input = """.. stdBody::"""
    run(input, BlockSequence.empty)
  }

  test("directive with custom content") {
    val input = """.. customBody:: Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, p("Line 1\n\nLine 2"))
  }

  test("failure - directive with invalid custom content") {
    val input = """.. customBody:: 1
                  | 
                  | 2""".stripMargin
    run(input, invalid(input, "body too short"))
  }

  test("directive with an argument and standard block content") {
    val input = """.. argAndBlocks:: arg
                  |
                  | Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, BlockSequence(p("arg!"), p("Line 1"), p("Line 2")))
  }

  test("directive with an argument and empty block content") {
    val input = """.. argAndBlocks:: arg"""
    run(input, BlockSequence(p("arg!")))
  }

  test("directive with an argument and standard span content") {
    val input = """.. argAndSpans:: arg
                  |
                  | Line 1
                  | Line 2""".stripMargin
    run(input, p(Text("arg"), Text("Line 1\nLine 2")))
  }

  test("directive with an argument and empty span content") {
    val input = """.. argAndSpans:: arg"""
    run(input, p(Text("arg")))
  }

  test("directive with a field and standard block content") {
    val input = """.. fdAndBody::
                  | :name: value
                  |
                  | Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, BlockSequence(p("value!"), p("Line 1"), p("Line 2")))
  }

  test("directive with a field and empty block content") {
    val input = """.. fdAndBody::
                  | :name: value""".stripMargin
    run(input, BlockSequence(p("value!")))
  }

  test("failure - directive with standard block content and a missing required field as invalid") {
    val error = "missing required options: name"
    val input = """.. fdAndBody:: Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, invalid(input, error))
  }

  test("directive with an argument, a field and standard block content") {
    val input = """.. all:: arg
                  | :name: value
                  |
                  | Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, BlockSequence(p("arg:value"), p("Line 1"), p("Line 2")))
  }

  test(
    "failure - directive with a field and standard block content, but a missing required argument as invalid"
  ) {
    val error = "missing required argument"
    val input = """.. all::
                  | :name: value
                  |
                  | Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, invalid(input, error))
  }

  test(
    "failure - directive with an argument and standard block content, but a missing required field as invalid"
  ) {
    val error = "missing required options: name"
    val input = """.. all:: arg
                  |
                  | Line 1
                  | 
                  | Line 2""".stripMargin
    run(input, invalid(input, error))
  }

  test("directive with an argument, a field and empty block content") {
    val input = """.. all:: arg
                  | :name: value""".stripMargin
    run(input, BlockSequence(p("arg:value")))
  }

  test("directive with a converted argument, field and body") {
    val input = """.. allOpt:: 2
                  | :name: 3
                  |
                  | 4
                  | 
                  | 5""".stripMargin
    run(input, p("14"))
  }

  test("directive with a converted field and body and a missing optional argument") {
    val input = """.. allOpt::
                  | :name: 3
                  |
                  | 4
                  | 
                  | 5""".stripMargin
    run(input, p("12"))
  }

  test("directive with a converted argument and body and a missing optional field") {
    val input = """.. allOpt:: 2
                  |
                  | 4
                  | 
                  | 5""".stripMargin
    run(input, p("11"))
  }

  test("directive with a converted body and missing optional argument and field") {
    val input = """.. allOpt::
                  |
                  | 4
                  | 
                  | 5""".stripMargin
    run(input, p("9"))
  }

  test("failure - directive with a converted field and body and an invalid argument") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. allOpt:: foo
                  | :name: 3
                  |
                  | 4
                  | 
                  | 5""".stripMargin
    run(input, invalid(input, error))
  }

  test("failure - directive with a converted argument and body and an invalid field") {
    val error =
      """unable to convert to int: java.lang.NumberFormatException: For input string: "foo""""
    val input = """.. allOpt:: 2
                  | :name: foo
                  |
                  | 4
                  | 
                  | 5""".stripMargin
    run(input, invalid(input, error))
  }

  test("failure - directive with a converted field and argument and an invalid body") {
    val error = "no integers provided"
    val input = """.. allOpt:: 2
                  | :name: 3""".stripMargin
    run(input, invalid(input, error))
  }

  test("failure - directive with an unknown name as invalid") {
    val error = "unknown directive: foo"
    val input = """.. foo::
                  | :name: 3""".stripMargin
    run(input, invalid(input, error))
  }

  test("substitution definition") {
    val input = ".. |def| spans:: text"
    run(input, SubstitutionDefinition("def", Text("text")))
  }

  test("substitution definition with an invalid directive") {
    val error = "missing required argument"
    val input = ".. |def| spans::"
    run(input, invalid(input, error))
  }

  val docWithTextRoles: Parser[RootElement] = defaultParser.map { root =>
    root.rewriteBlocks { case CustomizedTextRole(name, f, _) =>
      RewriteAction.Replace(p(f(name)))
    }
  }

  test("role directive") {
    val input = """.. role::custom(role)
                  | :name: 9""".stripMargin
    assertEquals(
      Parsers.consumeAll(docWithTextRoles).parse(input).toEither,
      Right(RootElement(p("custom(9)")))
    )
  }

  test("role directive with a missing required field as invalid") {
    val error = "missing required options: name"
    val input = """.. role::custom(role)"""
    run(input, invalid(input, error))
  }

}
