package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.api.bundle.SyntaxHighlighter
import laika.parse.implicits._
import laika.parse.code.CodeCategory.{ BooleanLiteral, LiteralValue, TypeName }
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.code.common.{
  CharLiteral,
  Comment,
  Identifier,
  Keywords,
  NumberLiteral,
  StringLiteral
}

/** @author Jens Halm
  */
object DartSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of("dart")

  private val annotation: CodeSpanParser = CodeSpanParser {
    "@" ~> Identifier.alphaNum.withCategory(CodeCategory.Annotation).map { name =>
      Seq(name.copy(content = "@" + name.content))
    }
  }

  private val charEscapes: CodeSpanParser =
    StringLiteral.Escape.unicode ++
      StringLiteral.Escape.octal ++
      StringLiteral.Escape.char

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    CharLiteral.standard.embed(charEscapes),
    StringLiteral.singleLine("r\"", "\""),
    StringLiteral.singleLine("r'", "'"),
    StringLiteral.singleLine('"').embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}")
    ),
    annotation,
    StringLiteral.multiLine("\"\"\"").embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}")
    ),
    StringLiteral.multiLine("'''").embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}")
    ),
    StringLiteral.singleLine('\'').embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}")
    ),
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords(TypeName)("bool", "double", "int", "dynamic", "String", "void"),
    Keywords(
      "assert",
      "break",
      "case",
      "catch",
      "class",
      "const",
      "default",
      "do",
      "else",
      "enum",
      "extends",
      "final",
      "finally",
      "for",
      "if",
      "is",
      "in",
      "new",
      "rethrow",
      "return",
      "super",
      "switch",
      "this",
      "throw",
      "try",
      "var",
      "while",
      "with"
    ),
    Identifier.alphaNum.withIdStartChars('_', '$').withCategoryChooser(
      Identifier.upperCaseTypeName
    ),
    NumberLiteral.hex,
    NumberLiteral.decimalFloat,
    NumberLiteral.decimalInt
  )

}
