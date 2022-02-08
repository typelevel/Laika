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
    StringLiteral.singleLine('"').embed(charEscapes),
    StringLiteral.singleLine('\'').embed(charEscapes),
    annotation,
    StringLiteral.multiLine("`").embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}"),
    ),
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords(TypeName)("bool", "byte", "double", "float", "int", "long","dynamic","void"),
    Keywords(
    "abstract","as","assert","async","await","break","case","catch","class","const","continue","default","else",
    "enum","export","extends","extension","final","finally","for","if","implements","import",
    "is","is!","mixin","new","on","required","rethrow","return","show", "static", "super", "switch", "this", "throw",
    "try", "var", "while", "with", "yield"),
    Identifier.alphaNum.withIdStartChars('_','$').withCategoryChooser(Identifier.upperCaseTypeName),
    number(NumberLiteral.binary),
    number(NumberLiteral.octal),
    number(NumberLiteral.hex),
    number(NumberLiteral.decimalFloat),
    number(NumberLiteral.decimalInt),
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float),
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long | NumericSuffix.float),
  )
  
}
