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

import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.text.TextParsers._
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import org.scalatest.{Assertion, Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class CommonSyntaxParserSpec extends WordSpec 
                             with Matchers
                             with ParseResultHelpers
                             with DefaultParserHelpers[Seq[CodeSpan]] {

  
  val rule = CodeSpanParsers(CodeCategory.Markup.Fence, '\n')(literal("==="))
  
  val defaultParser: Parser[Seq[CodeSpan]] = SyntaxHighlighter.build("test-lang")(
    rule,
    Comment.multiLine("/*", "*/"),
    Comment.singleLine("//"),
    Keywords("foo", "bar", "baz"),
    CharLiteral.standard.embed(
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.octal,
      StringLiteral.Escape.char
    ).build,
    RegexLiteral.standard,
    StringLiteral.multiLine("'''").build,
    StringLiteral.singleLine('\'').embed(
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.octal,
      StringLiteral.Escape.char,
      StringLiteral.Escape.literal("$$"),
      StringLiteral.Substitution.between("${", "}"),
      StringLiteral.Substitution('$')(anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_').min(1))
    ).build,
    Identifier.standard.withIdStartChars('_','$').withCategoryChooser(Identifier.upperCaseTypeName).build,
    NumberLiteral.binary.withUnderscores.withSuffix(NumericSuffix.long).build,
    NumberLiteral.octal.withUnderscores.withSuffix(NumericSuffix.long).build,
    NumberLiteral.hexFloat.withUnderscores.withSuffix(NumericSuffix.float).build,
    NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long).build,
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float).build,
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long).build,
  ).rootParser
  
  
  "The identifier parser" should {

    def test(id: String, category: CodeCategory): Assertion = Parsing(s"+- $id *^") should produce (Seq(
      CodeSpan("+- "),
      CodeSpan(id, category),
      CodeSpan(" *^")
    ))
    
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
    
    def test(numberLiteral: String): Assertion = Parsing(s"one1 $numberLiteral three3") should produce (Seq(
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(numberLiteral, CodeCategory.NumberLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    ))

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
    
  }
  
  "The string literal parser" should {

    def test(literal: String): Assertion = Parsing(s"one1 $literal three3") should produce (Seq(
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(literal, CodeCategory.StringLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    ))

    "parse a single-line literal" in {
      test("'foo'")
    }

    "parse a multi-line literal" in {
      test("'''foo bar'''")
    }
    
  }

  "The embedded parsers for the string literal parser" should {

    def test(category: CodeCategory, text: String): Assertion = Parsing(s"one1 'aa $text bb' three3") should produce (Seq(
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("'aa ", CodeCategory.StringLiteral),
      CodeSpan(text, category),
      CodeSpan(" bb'", CodeCategory.StringLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    ))
    
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

    "parse a literl escape" in {
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

    def test(literal: String): Assertion = Parsing(s"one1 $literal three3") should produce (Seq(
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan(literal, CodeCategory.CharLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    ))

    "parse a literal" in {
      test("'c'")
    }

  }

  "The embedded parsers for the char literal parser" should {

    def test (text: String): Assertion = Parsing(s"one1 '$text' three3") should produce(Seq(
      CodeSpan("one1", CodeCategory.Identifier),
      CodeSpan(" "),
      CodeSpan("'", CodeCategory.CharLiteral),
      CodeSpan(text, CodeCategory.EscapeSequence),
      CodeSpan("'", CodeCategory.CharLiteral),
      CodeSpan(" "),
      CodeSpan("three3", CodeCategory.Identifier)
    ))

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
      Parsing(s"one1 /[a-z]*/ three3") should produce (Seq(
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/[a-z]*/", CodeCategory.RegexLiteral),
        CodeSpan(" "),
        CodeSpan("three3", CodeCategory.Identifier)
      ))
    }

    "parse a regex literal with an escape sequence" in {
      Parsing(s"one1 /[\\\\]*/ three3") should produce (Seq(
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/[", CodeCategory.RegexLiteral),
        CodeSpan("\\\\", CodeCategory.EscapeSequence),
        CodeSpan("]*/", CodeCategory.RegexLiteral),
        CodeSpan(" "),
        CodeSpan("three3", CodeCategory.Identifier)
      ))
    }

    "parse a regex literal with flags" in {
      Parsing(s"one1 /[a-z]*/gi three3") should produce (Seq(
        CodeSpan("one1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/[a-z]*/gi", CodeCategory.RegexLiteral),
        CodeSpan(" "),
        CodeSpan("three3", CodeCategory.Identifier)
      ))
    }

  }

  "The comment parser" should {
    
    "parse a single line comment" in {
      
      val input =
        """line1
          |line2 // comment
          |line3""".stripMargin
      
      Parsing(input) should produce (Seq(
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("line2", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("// comment\n", CodeCategory.Comment),
        CodeSpan("line3", CodeCategory.Identifier),
      ))
    }

    "parse a multi-line comment" in {

      val input =
        """line1 /* moo
          |mar
          |maz */ line3""".stripMargin

      Parsing(input) should produce (Seq(
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/* moo\nmar\nmaz */", CodeCategory.Comment),
        CodeSpan(" "),
        CodeSpan("line3", CodeCategory.Identifier),
      ))
    }
    
  }
  
  "The keyword parser" should {
    
    "parse keywords" in {
      val input = "one foo three"

      Parsing(input) should produce (Seq(
        CodeSpan("one", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("foo", CodeCategory.Keyword),
        CodeSpan(" "),
        CodeSpan("three", CodeCategory.Identifier),
      ))
    }
    
    "ignore keywords when they are followed by more characters" in {
      val input = "one foo1 bar2 four"

      Parsing(input) should produce (Seq(
        CodeSpan("one", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("foo1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("bar2", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("four", CodeCategory.Identifier),
      ))
    }
    
  }
  
  "The parser for newline detections" should {
    
    "recognize input at the start of a line" in {
      val input =
        """line1
          |===
          |line3""".stripMargin

      Parsing(input) should produce (Seq(
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan("\n===", CodeCategory.Markup.Fence),
        CodeSpan("\n"),
        CodeSpan("line3", CodeCategory.Identifier),
      ))
    }

    "recognize input at the start of the input" in {
      val input =
        """===
          |line2
          |line3""".stripMargin

      Parsing(input) should produce (Seq(
        CodeSpan("===", CodeCategory.Markup.Fence),
        CodeSpan("\n"),
        CodeSpan("line2", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("line3", CodeCategory.Identifier),
      ))
    }

    "not recognize input in the middle of a line" in {
      val input =
        """line1
          |line2 ===
          |line3""".stripMargin

      Parsing(input) should produce (Seq(
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("line2", CodeCategory.Identifier),
        CodeSpan(" ===\n"),
        CodeSpan("line3", CodeCategory.Identifier),
      ))
    }
    
  }
  
  
}
