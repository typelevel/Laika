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

package laika.parse.code.common

import cats.data.NonEmptyList
import laika.ast.CodeSpan
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.builders._
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.implicits._
import laika.parse.text.CharGroup
import munit.FunSuite

/** @author Jens Halm
  */
class CommonSyntaxParserSpec extends FunSuite {

  val rule: CodeSpanParser =
    CodeSpanParser.onLineStart(CodeCategory.Markup.Fence)(literal("===").source)

  private def createParser(allowLetterAfterNumber: Boolean = false): Parser[Seq[CodeSpan]] =
    new SyntaxHighlighter {
      val language: NonEmptyList[String] = NonEmptyList.of("test-lang")

      val spanParsers: Seq[CodeSpanParser] = Seq(
        rule,
        Comment.multiLine("/*", "*/"),
        Comment.singleLine("//"),
        Keywords("foo", "bar", "baz"),
        CharLiteral.standard.embed(
          StringLiteral.Escape.unicode,
          StringLiteral.Escape.hex,
          StringLiteral.Escape.octal,
          StringLiteral.Escape.char
        ),
        RegexLiteral.standard,
        StringLiteral.multiLine("'''"),
        StringLiteral.singleLine('\'').embed(
          StringLiteral.Escape.unicode,
          StringLiteral.Escape.hex,
          StringLiteral.Escape.octal,
          StringLiteral.Escape.char,
          StringLiteral.Escape.literal("$$"),
          StringLiteral.Substitution.between("${", "}"),
          StringLiteral.Substitution(("$" ~ someOf(CharGroup.alphaNum.add('_'))).source)
        ),
        Identifier.alphaNum.withIdStartChars('_', '$').withCategoryChooser(
          Identifier.upperCaseTypeName
        ).copy(allowDigitBeforeStart = allowLetterAfterNumber),
        NumberLiteral.binary.withUnderscores.withSuffix(NumericSuffix.long),
        NumberLiteral.octal.withUnderscores.withSuffix(NumericSuffix.long),
        NumberLiteral.hexFloat.withUnderscores.withSuffix(NumericSuffix.float),
        NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long),
        NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float).copy(
          allowFollowingLetter = allowLetterAfterNumber
        ),
        NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long).copy(
          allowFollowingLetter = allowLetterAfterNumber
        )
      )

    }.rootParser

  val defaultParser: Parser[Seq[CodeSpan]] = createParser()

  def run(input: String, spans: CodeSpan*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  object Identifiers {

    def test(id: String, category: CodeCategory): Unit =
      run(s"+- $id *^", CodeSpan("+- "), CodeSpan(id, category), CodeSpan(" *^"))

  }

  test("identifier starting with a lower-case letter") {
    Identifiers.test("id", CodeCategory.Identifier)
  }

  test("identifier starting with an underscore") {
    Identifiers.test("_id", CodeCategory.Identifier)
  }

  test("identifier containing a digit") {
    Identifiers.test("id9", CodeCategory.Identifier)
  }

  test("type name starting with an upper-case letter") {
    Identifiers.test("Type", CodeCategory.TypeName)
  }

  test("type name containing a digit") {
    Identifiers.test("Type9", CodeCategory.TypeName)
  }

  test("type name containing an underscore") {
    Identifiers.test("Type_Foo", CodeCategory.TypeName)
  }

  object Numeric {

    def test(numberLiteral: String): Unit = run(
      s"one1 $numberLiteral three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(numberLiteral, CodeCategory.NumberLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

  }

  test("binary literal") {
    Numeric.test("0b10011011")
  }

  test("binary literal with underscores") {
    Numeric.test("0b_1001_1011")
  }

  test("binary literal with L suffix") {
    Numeric.test("0b_1001_1011L")
  }

  test("octal literal") {
    Numeric.test("0o171")
  }

  test("octal literal with underscores") {
    Numeric.test("0o171_151")
  }

  test("octal literal with L suffix") {
    Numeric.test("0o171L")
  }

  test("hex literal") {
    Numeric.test("0xff99ee")
  }

  test("hex literal with underscores") {
    Numeric.test("0xff_99_ee")
  }

  test("hex literal with L suffix") {
    Numeric.test("0xff99eeL")
  }

  test("single-digit decimal literal") {
    Numeric.test("3")
  }

  test("multi-digit decimal literal") {
    Numeric.test("902")
  }

  test("decimal literal with underscores") {
    Numeric.test("12_000")
  }

  test("decimal literal with L suffix") {
    Numeric.test("42L")
  }

  test("decimal float literal") {
    Numeric.test("23.45")
  }

  test("decimal float literal with leading dot") {
    Numeric.test(".456")
  }

  test("decimal float literal with underscores") {
    Numeric.test("23_427.45")
  }

  test("decimal float literal with D suffix") {
    Numeric.test("23.45D")
  }

  test("decimal float literal with exponent") {
    Numeric.test("23.45e24")
  }

  test("decimal float literal with a signed exponent") {
    Numeric.test("23.45e-24")
  }

  test("decimal float literal with exponent and D suffix") {
    Numeric.test("23.45e24D")
  }

  test("hex float literal") {
    Numeric.test("0x23.f5")
  }

  test("hex float literal with an exponent") {
    Numeric.test("0x23.f5p-23")
  }

  test("do not recognize a number immediately followed by a letter") {
    run(
      s"one1 123bb three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" 123bb "),
      CodeSpan("three3", CodeCategory.Identifier)
    )
  }

  test(
    "recognize a number immediately followed by a letter if explicitly allowed (e.g. for numbers with unit like in CSS)"
  ) {
    assertEquals(
      createParser(allowLetterAfterNumber = true).parse(s"one1 123bb three3").toEither,
      Right(
        Seq(
          CodeSpan("one1", CodeCategory.Identifier),
          CodeSpan(" "),
          CodeSpan("123", CodeCategory.NumberLiteral),
          CodeSpan("bb", CodeCategory.Identifier),
          CodeSpan(" "),
          CodeSpan("three3", CodeCategory.Identifier)
        )
      )
    )
  }

  object StringLiterals {

    def test(literal: String): Unit = run(
      s"one1 $literal three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(literal, CodeCategory.StringLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

    def testEmbedded(category: CodeCategory, text: String): Unit = run(
      s"one1 'aa $text bb' three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("'aa ", CodeCategory.StringLiteral),
      CodeSpan(text, category),
      CodeSpan(" bb'", CodeCategory.StringLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

    def testEscape(escape: String): Unit      = testEmbedded(CodeCategory.EscapeSequence, escape)
    def testSubstitution(subst: String): Unit = testEmbedded(CodeCategory.Substitution, subst)
  }

  test("single-line string literal") {
    StringLiterals.test("'foo'")
  }

  test("multi-line string literal") {
    StringLiterals.test("'''foo bar'''")
  }

  test("single character escape") {
    StringLiterals.testEscape("\\t")
  }

  test("unicode escape") {
    StringLiterals.testEscape("\\ua24f")
  }

  test("octal escape") {
    StringLiterals.testEscape("\\322")
  }

  test("hex escape") {
    StringLiterals.testEscape("\\x7f")
  }

  test("literal escape") {
    StringLiterals.testEscape("$$")
  }

  test("substitution expression") {
    StringLiterals.testSubstitution("${ref}")
  }

  test("substitution identifier") {
    StringLiterals.testSubstitution("$ref22")
  }

  object CharLiterals {

    def test(literal: String): Unit = run(
      s"one1 $literal three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(literal, CodeCategory.CharLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

    def testEscape(text: String): Unit = run(
      s"one1 '$text' three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("'", CodeCategory.CharLiteral),
      CodeSpan(text, CodeCategory.EscapeSequence),
      CodeSpan("'", CodeCategory.CharLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

  }

  test("char literal") {
    CharLiterals.test("'c'")
  }

  test("char literal with single character escape") {
    CharLiterals.testEscape("\\t")
  }

  test("char literal with unicode escape") {
    CharLiterals.testEscape("\\ua24f")
  }

  test("char literal with octal escape") {
    CharLiterals.testEscape("\\322")
  }

  test("char literal with hex escape") {
    CharLiterals.testEscape("\\x7f")
  }

  test("regex literal") {
    run(
      s"one1 /[a-z]*/ three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("/[a-z]*/", CodeCategory.RegexLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )
  }

  test("regex literal with an escape sequence") {
    run(
      s"one1 /[\\\\]*/ three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("/[", CodeCategory.RegexLiteral),
      CodeSpan("\\\\", CodeCategory.EscapeSequence),
      CodeSpan("]*/", CodeCategory.RegexLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )
  }

  test("regex literal with flags") {
    run(
      s"one1 /[a-z]*/gi three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("/[a-z]*/gi", CodeCategory.RegexLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )
  }

  test("single line comment") {

    val input =
      """line1
        |line2 // comment
        |line3""".stripMargin

    run(
      input,
      CodeSpan("line1", CodeCategory.Identifier),
      CodeSpan("\n"),
      CodeSpan("line2", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("// comment\n", CodeCategory.Comment),
      CodeSpan("line3", CodeCategory.Identifier)
    )
  }

  test("multi-line comment") {

    val input =
      """line1 /* moo
        |mar
        |maz */ line3""".stripMargin

    run(
      input,
      CodeSpan("line1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("/* moo\nmar\nmaz */", CodeCategory.Comment),
      CodeSpan(" "),
      CodeSpan("line3", CodeCategory.Identifier)
    )
  }

  test("keywords") {
    val input = "one foo three"

    run(
      input,
      CodeSpan("one", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("foo", CodeCategory.Keyword),
      CodeSpan(" "),
      CodeSpan("three", CodeCategory.Identifier)
    )
  }

  test("ignore keywords when they are followed by more letters or digits") {
    val input = "one foo1 bar2 four"

    run(
      input,
      CodeSpan("one", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("foo1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("bar2", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("four", CodeCategory.Identifier)
    )
  }

  test("ignore keywords when they are preceded by letters or digits") {
    val input = "one 1foo bbar four"

    run(
      input,
      CodeSpan("one", CodeCategory.Identifier),
      CodeSpan(" 1foo "),
      CodeSpan("bbar", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("four", CodeCategory.Identifier)
    )
  }

  test("newline detection - recognize input at the start of a line") {
    val input =
      """line1
        |===
        |line3""".stripMargin

    run(
      input,
      CodeSpan("line1", CodeCategory.Identifier),
      CodeSpan("\n"),
      CodeSpan("===", CodeCategory.Markup.Fence),
      CodeSpan("\n"),
      CodeSpan("line3", CodeCategory.Identifier)
    )
  }

  test("newline detection - recognize input at the start of the input") {
    val input =
      """===
        |line2
        |line3""".stripMargin

    run(
      input,
      CodeSpan("===", CodeCategory.Markup.Fence),
      CodeSpan("\n"),
      CodeSpan("line2", CodeCategory.Identifier),
      CodeSpan("\n"),
      CodeSpan("line3", CodeCategory.Identifier)
    )
  }

  test("newline detection - do not recognize input in the middle of a line") {
    val input =
      """line1
        |line2 ===
        |line3""".stripMargin

    run(
      input,
      CodeSpan("line1", CodeCategory.Identifier),
      CodeSpan("\n"),
      CodeSpan("line2", CodeCategory.Identifier),
      CodeSpan(" ===\n"),
      CodeSpan("line3", CodeCategory.Identifier)
    )
  }

}
