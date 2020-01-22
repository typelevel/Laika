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

package laika.parse.code.languages

import laika.ast.~
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.common.{EmbeddedCodeSpans, Identifier, Keywords, NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.text.TextParsers._
import laika.rst.BaseParsers
import laika.rst.InlineParsers.{delimitedByMarkupEnd, markupStart}

/**
  * @author Jens Halm
  */
object TextMarkup {
  
  import NumberLiteral._
  
  private def noSpaceAhead(delimChar: Char): Parser[String] = lookAhead(anyBut(' ','\n',delimChar).take(1)).map(_ => "")
  private def noSpaceBefore(numDelimChars: Int): Parser[Unit] = lookBehind(numDelimChars + 1, anyBut(' ','\n').take(1)).map(_ => ())
  
  def span(category: CodeCategory, delim: String): CodeSpanParsers =
    StringLiteral
      .multiLine(delim)
      .withCategory(category)
      .withPrefix(noSpaceAhead(delim.head))
      .withPostCondition(noSpaceBefore(delim.length))
      .build
  
  def singleLine(category: CodeCategory, start: String, end: Char): CodeSpanParsers =
    StringLiteral
      .singleLine(Set(start.head), end)
      .withCategory(category)
      .withPrefix(literal(start.tail))
      .build
  
  private def linkRest(prefix: String): Parser[Seq[CodeSpan]] = {
    
    val url = '(' ~> delimitedBy(')').nonEmpty.failOn('\n').map(url => CodeSpan(s"($url)", CodeCategory.Markup.LinkTarget))
    val ref = '[' ~> delimitedBy(']').failOn('\n').map(ref => CodeSpan(s"[$ref]", CodeCategory.Identifier))
    val link = (literal(prefix.tail) ~ delimitedBy(']').failOn('\n')).concat.map(prefix.head.toString+_+"]")
    
    (link ~ opt(url | ref)).map {
      case link ~ Some(target) if target.content == "[]" => Seq(CodeSpan(link+"[]", CodeCategory.Identifier))  
      case link ~ Some(target) => Seq(CodeSpan(link, CodeCategory.Markup.LinkText), target)  
      case link ~ None => Seq(CodeSpan(link, CodeCategory.Identifier))  
    } 
  }
  
  val link: CodeSpanParsers = CodeSpanParsers('[')(linkRest("["))
  val image: CodeSpanParsers = CodeSpanParsers('!')(linkRest("!["))
  
  val linkTarget: CodeSpanParsers = CodeSpanParsers('\n') {
    ('[' ~> delimitedBy("]:").failOn('\n') ~ restOfLine).map {
      case ref ~ target => Seq(
        CodeSpan("\n"),
        CodeSpan(s"[$ref]:", CodeCategory.Identifier),
        CodeSpan(target, CodeCategory.Markup.LinkTarget),
        CodeSpan("\n"),
      )
    }
  }

  val atxHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    (anyOf('#').min(1).max(6) ~ restOfLine).concat.map(_+"\n")
  }

  val setexHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    val deco = (anyOf('=').min(1) | anyOf('-').min(1)) <~ wsEol
    (restOfLine.map(_+"\n") ~ deco).concat.map(_+"\n")
  }
  
  val codeFence: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
    (anyOf('`').take(3) ~ restOfLine).concat
  }

  val rules: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
    Seq('*', '-', '_').map { decoChar =>
      (char(decoChar) ~ (anyOf(' ') ~ literal(decoChar.toString)).concat.rep.min(2) ~ ws ~ '\n').map {
        case start ~ pattern ~ s ~ nl => start.toString + pattern.mkString + s + nl
      }
    }.reduceLeft(_ | _)
  }
  
  val quoteChars: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Quote, '\n')(anyOf('>').min(1))

  val mdSpans: CodeSpanParsers = span(CodeCategory.Markup.Emphasized, "**") ++
    span(CodeCategory.Markup.Emphasized, "*") ++
    span(CodeCategory.Markup.Emphasized, "__") ++
    span(CodeCategory.Markup.Emphasized, "_") ++
    span(CodeCategory.StringLiteral, "``") ++
    span(CodeCategory.StringLiteral, "`")
  
  lazy val markdown: SyntaxHighlighter = SyntaxHighlighter.build("markdown", "md")(
    mdSpans,
    singleLine(CodeCategory.Markup.LinkTarget, "<", '>'),
    image,
    link,
    StringLiteral.Escape.char,
    linkTarget,
    codeFence,
    atxHeader,
    setexHeader,
    rules,
    quoteChars,
  )

  private def span (start: String, end: String): Parser[String] = span(start, end, success(()))

  private def span (start: String, end: String, postCondition: Parser[Any]): Parser[String] = 
    markupStart(start, end) ~> delimitedByMarkupEnd(end, postCondition) ^^ { text => s"$start$text$end" }

  def rawSpan(start: String, end: String, category: CodeCategory): Parser[CodeSpan] = // does not perform checks for rst inline markup recognition
    (start ~ delimitedBy(end).failOn('\n')).concat.map(res => CodeSpan(s"$res$end", category))

  val strong: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Emphasized, '*')(span("*","**"))
  val em: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Emphasized, '*')(span("","*",not("*")))
  val lit: CodeSpanParsers = CodeSpanParsers(CodeCategory.StringLiteral, '`')(span("`","``"))
  val ref: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.LinkTarget, '`')(span("","`__")) ++
    CodeSpanParsers(CodeCategory.Markup.LinkTarget, '`')(span("","`_"))
  val subst: CodeSpanParsers = CodeSpanParsers(CodeCategory.Substitution, '|')(span("","|__")) ++ 
    CodeSpanParsers(CodeCategory.Substitution, '|')(span("","|_")) ++
    CodeSpanParsers(CodeCategory.Substitution, '|')(span("","|"))
  val interpretedText: CodeSpanParsers = CodeSpanParsers(CodeCategory.Substitution, '`')(span("","`"))
  val roleName: CodeSpanParsers = CodeSpanParsers(CodeCategory.Identifier, ':')(span("",":"))
  val internalTarget: CodeSpanParsers = CodeSpanParsers(CodeCategory.Identifier, '_')(span("`","`"))
  val footnote: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.LinkTarget, '[')(span("","]_"))
  
  val header: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    anyOf(BaseParsers.punctuationChars.toSeq:_*).take(1) >> { startChar =>
      (anyOf(startChar.head).min(1) ~ ws ~ ('\n' ~> not(blankLine) ~> restOfLine) ~ anyOf(startChar.head).min(1) ~ ws <~ lookAhead('\n')).map {
        case deco1 ~ spaces ~ text ~ deco2 ~ spaces2 => s"$startChar$deco1$spaces\n$text\n$deco2$spaces2"
      }
    }
  }

  val underlinedHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    (not(blankLine) ~> restOfLine.map(_+"\n") ~ anyOf(BaseParsers.punctuationChars.toSeq:_*).take(1)) >> {
      case text ~ decoStart =>
        (anyOf(decoStart.head).min(1) ~ ws <~ lookAhead('\n')).map {
          case deco ~ spaces => s"$text$decoStart$deco$spaces"
        }
    }
  }

  val transition: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
    anyOf(BaseParsers.punctuationChars.toSeq:_*).take(1) >> { startChar =>
      anyOf(startChar.head).min(1).map(startChar + _) <~ lookAhead(ws ~ '\n' ~ blankLine)
    }
  }
  
  val explicitItems: CodeSpanParsers = CodeSpanParsers('\n') {
    
    val subst = (rawSpan("|", "|", CodeCategory.Substitution) ~ ws ~ delimitedBy("::")).map {
      case sub ~ space ~ name => Seq(sub, CodeSpan(space), CodeSpan(s"$name::", CodeCategory.Identifier))
    }
    val linkTarget = rawSpan("_", ":", CodeCategory.Markup.LinkTarget).map(Seq(_))
    val footnote = rawSpan("[", "]", CodeCategory.Markup.LinkTarget).map(Seq(_))
    val directive = rawSpan("", "::", CodeCategory.Identifier).map(Seq(_))
    
    (opt(atStart).map(_.isDefined) ~ (literal(".. ") ~> (subst | linkTarget | footnote | directive))).map {
      case startOfInput ~ res =>
        val nl = if (startOfInput) "" else "\n"
        CodeSpan(s"$nl.. ") +: res
    }
  }
  
  val fieldDef: CodeSpanParsers = CodeSpanParsers('\n') {
    (ws ~ rawSpan(":", ":", CodeCategory.Identifier)).map {
      case space ~ name => Seq(CodeSpan(s"\n$space"), name)
    }
  }
  
  
  val rst: SyntaxHighlighter = SyntaxHighlighter.build("reStructuredText", "rst")(
    explicitItems,
    fieldDef,
    strong,
    em,
    subst,
    footnote,
    lit,
    ref,
    internalTarget,
    interpretedText,
    roleName,
    StringLiteral.Escape.char,
    header,
    transition,
    underlinedHeader
  )
  
  val laikaSubstitution: CodeSpanParsers = StringLiteral.Substitution.between("${", "}")
  
  val laikaHoconBlock: CodeSpanParsers = CodeSpanParsers('{') {
    embeddedHocon("%", "%}", Set(CodeCategory.Keyword)).map(res => CodeSpan("{", CodeCategory.Keyword) +: res)
  }

  def embeddedHocon(start: String, end: String, delimCategory: Set[CodeCategory] = Set()): Parser[Seq[CodeSpan]] = {
    val embedded: EmbeddedCodeSpans = new EmbeddedCodeSpans {
      val embedded: Seq[CodeSpanParsers] = HOCON.highlighter.spanParsers
      val defaultCategories: Set[CodeCategory] = Set()
    }
    (literal(start) ~> embedded.contentParser(delimitedBy(end))).map { hocon => // TODO - support nested objects
      CodeSpan(start, delimCategory) +: hocon :+ CodeSpan(end, delimCategory)
    }
  }
  
  val laikaDirective: CodeSpanParsers = CodeSpanParsers('@') {
    (':' ~> Identifier.standard.standaloneParser ~ opt(ws.min(1) ~ embeddedHocon("{","}"))).map {
      case name ~ Some(spaces ~ hocon) => CodeSpan("@:", CodeCategory.Keyword) +: name +: CodeSpan(spaces) +: hocon
      case name ~ None                 => Seq(CodeSpan("@:", CodeCategory.Keyword), name)
    }
  }
  
  val laikaFence: CodeSpanParsers = Keywords("@:@")

  lazy val laikaMarkdown: SyntaxHighlighter = SyntaxHighlighter.build("laikaMarkdown", "laika-md")(
    laikaSubstitution,
    laikaDirective,
    laikaFence,
    laikaHoconBlock,
    mdSpans,
    singleLine(CodeCategory.Markup.LinkTarget, "<", '>'),
    image,
    link,
    StringLiteral.Escape.char,
    linkTarget,
    codeFence,
    atxHeader,
    setexHeader,
    rules,
    quoteChars,
  )

  lazy val laikaRst: SyntaxHighlighter = SyntaxHighlighter.build("laikaReStructuredText", "laika-rst")(
    laikaSubstitution,
    laikaDirective,
    laikaFence,
    laikaHoconBlock,
    explicitItems,
    fieldDef,
    strong,
    em,
    subst,
    footnote,
    lit,
    ref,
    internalTarget,
    interpretedText,
    roleName,
    StringLiteral.Escape.char,
    header,
    transition,
    underlinedHeader
  )

  val enhancedStartTag: CodeSpanParsers = HTML.TagParser(CodeCategory.XML.TagName, "<", ">", HTML.nameParser).embed(
    StringLiteral.singleLine('\'').embed(ref, laikaSubstitution).build,
    StringLiteral.singleLine('"').embed(ref, laikaSubstitution).build,
    HTML.name(CodeCategory.AttributeName)
  ).build

  lazy val laikaHTML: SyntaxHighlighter = SyntaxHighlighter.build("laikaHTML", "laika-html")(
    laikaSubstitution,
    laikaDirective,
    laikaFence,
    laikaHoconBlock,
    HTML.docType,
    HTML.comment,
    HTML.ref,
    HTML.emptyTag,
    HTML.scriptTag,
    HTML.styleTag,
    enhancedStartTag,
    HTML.endTag
  )
  
}
