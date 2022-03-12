package laika.parse.code.languages


import cats.data.NonEmptyList
import laika.bundle.SyntaxHighlighter
import laika.parse.implicits._
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue, TypeName}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.code.common.{CharLiteral, Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, StringLiteral}

/**
  * @author Jens Halm
  */
object DartSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of("dart")
  
  val annotation: CodeSpanParser = CodeSpanParser {
    "@" ~> Identifier.alphaNum.withCategory(CodeCategory.Annotation).map { name => 
      Seq(name.copy(content = "@" + name.content))
    }
  }

  def number(parser: NumberLiteral.NumericParser): CodeSpanParser = parser.withUnderscores.withSuffix(NumericSuffix.bigInt)


  val charEscapes: CodeSpanParser = 
    StringLiteral.Escape.unicode ++
    StringLiteral.Escape.octal ++
    StringLiteral.Escape.char
  
  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    CharLiteral.standard.embed(charEscapes),
    StringLiteral.singleLine('"').embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}")
    ),
    annotation,
    StringLiteral.multiLine("\"\"\"").embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}"),
    ),
    StringLiteral.multiLine("'''").embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}"),
    ),
    StringLiteral.singleLine('\'').embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}")
    ),
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords(TypeName)("bool", "double", "int", "dynamic","String","void"),
    Keywords(
    "abstract","as","assert","async","await","break","case","catch","class","const","default","else",
    "enum","export","extends","extension","final","finally","for","get","if","implements","import",
    "is","late","mixin","new","on","operator","required","rethrow","return", "static", "super", "switch", "this", "throw",
    "try", "var", "while", "with", "yield"),
    Identifier.alphaNum.withIdStartChars('_','$').withCategoryChooser(Identifier.upperCaseTypeName),
    number(NumberLiteral.hex),
    number(NumberLiteral.decimalFloat),
    number(NumberLiteral.decimalInt),
  )
  
}
