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
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.helper.{MigrationSpec, ParseResultHelpers}
import laika.parse.implicits._
import laika.parse.text.CharGroup
import org.scalatest.Assertion

/**
  * @author Jens Halm
  */
class CommonSyntaxParserSpec extends MigrationSpec
                             with ParseResultHelpers {

  
  val rule: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Fence)(literal("===").source)
  
  private def createParser (allowLetterAfterNumber: Boolean = false): Parser[Seq[CodeSpan]] = new SyntaxHighlighter {
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
      Identifier.alphaNum.withIdStartChars('_','$').withCategoryChooser(Identifier.upperCaseTypeName).copy(allowDigitBeforeStart = allowLetterAfterNumber),
      NumberLiteral.binary.withUnderscores.withSuffix(NumericSuffix.long),
      NumberLiteral.octal.withUnderscores.withSuffix(NumericSuffix.long),
      NumberLiteral.hexFloat.withUnderscores.withSuffix(NumericSuffix.float),
      NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long),
      NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float).copy(allowFollowingLetter = allowLetterAfterNumber),
      NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long).copy(allowFollowingLetter = allowLetterAfterNumber)
    )
  }.rootParser
  
  val defaultParser: Parser[Seq[CodeSpan]] = createParser()

  def run (input: String, spans: CodeSpan*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))
    
  
  "The identifier parser" should {

    def test(id: String, category: CodeCategory): Assertion = run(s"+- $id *^", 
      CodeSpan("+- "),
      CodeSpan(id, category),
      CodeSpan(" *^")
    )
    
    "parse an identifier starting with a lower-case letter" in {
      test("id", CodeCategory.Identifier)
    }

    "parse an identifier starting with an underscore" in {
      test("_id", CodeCategory.Identifier)
    }

    "parse an identifier containing a digit" in {
      test("id9", CodeCategory.Identifier)
    }

    "parse a type name starting with an upper-case letter" in {
      test("Type", CodeCategory.TypeName)
    }

    "parse a type name containing a digit" in {
      test("Type9", CodeCategory.TypeName)
    }

    "parse a type name containing an underscore" in {
      test("Type_Foo", CodeCategory.TypeName)
    }
    
  }
  
  
  "The numeric literal parser" should {
    
    def test(numberLiteral: String): Assertion = run(s"one1 $numberLiteral three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(numberLiteral, CodeCategory.NumberLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

    "parse a binary literal" in {
      test("0b10011011")
    }
    
    "parse a binary literal with underscores" in {
      test("0b_1001_1011")
    }

    "parse a binary literal with L suffix" in {
      test("0b_1001_1011L")
    }

    "parse an octal literal" in {
      test("0o171")
    }

    "parse an octal literal with underscores" in {
      test("0o171_151")
    }

    "parse an octal literal with L suffix" in {
      test("0o171L")
    }

    "parse a hex literal" in {
      test("0xff99ee")
    }

    "parse a hex literal with underscores" in {
      test("0xff_99_ee")
    }

    "parse a hex literal with L suffix" in {
      test("0xff99eeL")
    }

    "parse a single-digit decimal literal" in {
      test("3")
    }

    "parse a multi-digit decimal literal" in {
      test("902")
    }

    "parse a decimal literal with underscores" in {
      test("12_000")
    }

    "parse a decimal literal with L suffix" in {
      test("42L")
    }

    "parse a decimal float literal" in {
      test("23.45")
    }

    "parse a decimal float literal with leading dot" in {
      test(".456")
    }

    "parse a decimal float literal with underscores" in {
      test("23_427.45")
    }

    "parse a decimal float literal with D suffix" in {
      test("23.45D")
    }

    "parse a decimal float literal with exponent" in {
      test("23.45e24")
    }

    "parse a decimal float literal with a signed exponent" in {
      test("23.45e-24")
    }

    "parse a decimal float literal with exponent and D suffix" in {
      test("23.45e24D")
    }

    "parse a hex float literal" in {
      test("0x23.f5")
    }

    "parse a hex float literal with an exponent" in {
      test("0x23.f5p-23")
    }
    
    "not recognize a number immediately followed by a letter" in {
      run(s"one1 123bb three3",
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" 123bb "),
        CodeSpan("three3", CodeCategory.Identifier)
      )
    }

    "recognize a number immediately followed by a letter if explicitly allowed (e.g. for numbers with unit like in CSS)" in {
      assertEquals(createParser(allowLetterAfterNumber = true).parse(s"one1 123bb three3").toEither, Right(Seq( 
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("123", CodeCategory.NumberLiteral),
        CodeSpan("bb", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("three3", CodeCategory.Identifier)
      )))
    }
    
  }
  
  "The string literal parser" should {

    def test(literal: String): Assertion = run(s"one1 $literal three3", 
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(literal, CodeCategory.StringLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

    "parse a single-line literal" in {
      test("'foo'")
    }

    "parse a multi-line literal" in {
      test("'''foo bar'''")
    }
    
  }

  "The embedded parsers for the string literal parser" should {

    def test(category: CodeCategory, text: String): Assertion = run(s"one1 'aa $text bb' three3", 
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("'aa ", CodeCategory.StringLiteral),
      CodeSpan(text, category),
      CodeSpan(" bb'", CodeCategory.StringLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )
    
    def testEscape(escape: String): Assertion = test(CodeCategory.EscapeSequence, escape)
    def testSubstitution(subst: String): Assertion = test(CodeCategory.Substitution, subst)

    "parse a single character escape" in {
      testEscape("\\t")
    }

    "parse a unicode escape" in {
      testEscape("\\ua24f")
    }

    "parse an octal escape" in {
      testEscape("\\322")
    }

    "parse a hex escape" in {
      testEscape("\\x7f")
    }

    "parse a literal escape" in {
      testEscape("$$")
    }

    "parse a substitution expression" in {
      testSubstitution("${ref}")
    }

    "parse a substitution identifier" in {
      testSubstitution("$ref22")
    }

  }

  "The char literal parser" should {

    def test(literal: String): Assertion = run(s"one1 $literal three3", 
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(literal, CodeCategory.CharLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

    "parse a literal" in {
      test("'c'")
    }

  }

  "The embedded parsers for the char literal parser" should {

    def test (text: String): Assertion = run(s"one1 '$text' three3",
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("'", CodeCategory.CharLiteral),
      CodeSpan(text, CodeCategory.EscapeSequence),
      CodeSpan("'", CodeCategory.CharLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    )

    "parse a single character escape" in {
      test("\\t")
    }

    "parse a unicode escape" in {
      test("\\ua24f")
    }

    "parse an octal escape" in {
      test("\\322")
    }

    "parse a hex escape" in {
      test("\\x7f")
    }

  }

  "The regex literal parser" should {

    "parse a regex literal" in {
      run(s"one1 /[a-z]*/ three3", 
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/[a-z]*/", CodeCategory.RegexLiteral),
        CodeSpan(" "),
        CodeSpan("three3", CodeCategory.Identifier)
      )
    }

    "parse a regex literal with an escape sequence" in {
      run(s"one1 /[\\\\]*/ three3", 
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/[", CodeCategory.RegexLiteral),
        CodeSpan("\\\\", CodeCategory.EscapeSequence),
        CodeSpan("]*/", CodeCategory.RegexLiteral),
        CodeSpan(" "),
        CodeSpan("three3", CodeCategory.Identifier)
      )
    }

    "parse a regex literal with flags" in {
      run(s"one1 /[a-z]*/gi three3", 
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/[a-z]*/gi", CodeCategory.RegexLiteral),
        CodeSpan(" "),
        CodeSpan("three3", CodeCategory.Identifier)
      )
    }

  }

  "The comment parser" should {
    
    "parse a single line comment" in {
      
      val input =
        """line1
          |line2 // comment
          |line3""".stripMargin
      
      run(input, 
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("line2", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("// comment\n", CodeCategory.Comment),
        CodeSpan("line3", CodeCategory.Identifier),
      )
    }

    "parse a multi-line comment" in {

      val input =
        """line1 /* moo
          |mar
          |maz */ line3""".stripMargin

      run(input, 
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/* moo\nmar\nmaz */", CodeCategory.Comment),
        CodeSpan(" "),
        CodeSpan("line3", CodeCategory.Identifier),
      )
    }
    
  }
  
  "The keyword parser" should {
    
    "parse keywords" in {
      val input = "one foo three"

      run(input, 
        CodeSpan("one", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("foo", CodeCategory.Keyword),
        CodeSpan(" "),
        CodeSpan("three", CodeCategory.Identifier),
      )
    }
    
    "ignore keywords when they are followed by more letters or digits" in {
      val input = "one foo1 bar2 four"

      run(input, 
        CodeSpan("one", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("foo1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("bar2", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("four", CodeCategory.Identifier),
      )
    }

    "ignore keywords when they are preceded by letters or digits" in {
      val input = "one 1foo bbar four"

      run(input, 
        CodeSpan("one", CodeCategory.Identifier),
        CodeSpan(" 1foo "),
        CodeSpan("bbar", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("four", CodeCategory.Identifier),
      )
    }
    
  }
  
  "The parser for newline detections" should {
    
    "recognize input at the start of a line" in {
      val input =
        """line1
          |===
          |line3""".stripMargin

      run(input, 
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("===", CodeCategory.Markup.Fence),
        CodeSpan("\n"),
        CodeSpan("line3", CodeCategory.Identifier),
      )
    }

    "recognize input at the start of the input" in {
      val input =
        """===
          |line2
          |line3""".stripMargin

      run(input, 
        CodeSpan("===", CodeCategory.Markup.Fence),
        CodeSpan("\n"),
        CodeSpan("line2", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("line3", CodeCategory.Identifier),
      )
    }

    "not recognize input in the middle of a line" in {
      val input =
        """line1
          |line2 ===
          |line3""".stripMargin

      run(input, 
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("line2", CodeCategory.Identifier),
        CodeSpan(" ===\n"),
        CodeSpan("line3", CodeCategory.Identifier),
      )
    }
    
  }
  
  
}
