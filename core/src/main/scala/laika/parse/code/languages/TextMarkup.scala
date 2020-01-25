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

import laika.ast.{CodeSpan, ~}
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.common.{EmbeddedCodeSpans, Identifier, Keywords, NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.text.TextParsers._
import laika.rst.BaseParsers
import laika.rst.InlineParsers.{delimitedByMarkupEnd, markupStart}

/**
  * @author Jens Halm
  */
object TextMarkup {
  
  import NumberLiteral._
  
  object md {

    private def noSpaceAhead (delimChar: Char): Parser[String] = lookAhead(anyBut(' ', '\n', delimChar).take(1)).map(_ => "")

    private def noSpaceBefore (numDelimChars: Int): Parser[Unit] = lookBehind(numDelimChars + 1, anyBut(' ', '\n').take(1)).map(_ => ())

    def span (category: CodeCategory, delim: String): CodeSpanParsers =
      StringLiteral
        .multiLine(delim)
        .withCategory(category)
        .withPrefix(noSpaceAhead(delim.head))
        .withPostCondition(noSpaceBefore(delim.length))
        .build

    def singleLine (category: CodeCategory, start: String, end: Char): CodeSpanParsers =
      StringLiteral
        .singleLine(Set(start.head), end)
        .withCategory(category)
        .withPrefix(literal(start.tail))
        .build

    private def linkRest (prefix: String): Parser[Seq[CodeSpan]] = {

      val url = '(' ~> delimitedBy(')').nonEmpty.failOn('\n').map(url => CodeSpan(s"($url)", CodeCategory.Markup.LinkTarget))
      val ref = '[' ~> delimitedBy(']').failOn('\n').map(ref => CodeSpan(s"[$ref]", CodeCategory.Markup.LinkTarget))
      val link = (literal(prefix.tail) ~ delimitedBy(']').failOn('\n')).concat.map(prefix.head.toString + _ + "]")

      (link ~ opt(url | ref)).map {
        case link ~ Some(target) if target.content == "[]" => Seq(CodeSpan(link + "[]", CodeCategory.Markup.LinkTarget))
        case link ~ Some(target) => Seq(CodeSpan(link, CodeCategory.Markup.LinkText), target)
        case link ~ None => Seq(CodeSpan(link, CodeCategory.Markup.LinkTarget))
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
      (anyOf('#').min(1).max(6) ~ restOfLine).concat.map(_ + "\n")
    }

    val setexHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
      val deco = (anyOf('=').min(1) | anyOf('-').min(1)) <~ wsEol
      (restOfLine.map(_ + "\n") ~ deco).concat.map(_ + "\n")
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

    val parsers: CodeSpanParsers = mdSpans ++
      singleLine(CodeCategory.Markup.LinkTarget, "<", '>') ++
      image ++
      link ++
      StringLiteral.Escape.char ++
      linkTarget ++
      codeFence ++
      atxHeader ++
      setexHeader ++
      rules ++
      quoteChars

    val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("markdown", "md")(parsers)
  }

  object rst {

    // raw = does not perform checks for rst inline markup recognition
    def rawSpan (start: String, end: String, category: CodeCategory, includeDelim: Boolean = true): Parser[CodeSpan] =
      (start ~ delimitedBy(end).failOn('\n')).concat.map {
        res => CodeSpan(if (includeDelim) s"$res$end" else res, category)
      }
    
    private def span (start: String, end: String, category: CodeCategory, postCondition: Parser[Any] = success(())): CodeSpanParsers =
      CodeSpanParsers(category, start.head) {
        markupStart(start.tail, end) ~> delimitedByMarkupEnd(end, postCondition) ^^ { text => s"${start.tail}$text$end" }
      }

    val strong: CodeSpanParsers = span("**", "**", CodeCategory.Markup.Emphasized)
    val em: CodeSpanParsers     = span("*", "*", CodeCategory.Markup.Emphasized, not("*"))
    val lit: CodeSpanParsers    = span("``", "``", CodeCategory.StringLiteral)
    val ref: CodeSpanParsers    = span("`", "`__", CodeCategory.Markup.LinkTarget) ++ span("`", "`_", CodeCategory.Markup.LinkTarget)
    val subst: CodeSpanParsers  = span("|", "|__", CodeCategory.Substitution) ++ span("|", "|_", CodeCategory.Substitution) ++ span("|", "|", CodeCategory.Substitution)
    
    val interpretedText: CodeSpanParsers = span("`", "`", CodeCategory.Substitution)
    val roleName: CodeSpanParsers        = span(":", ":", CodeCategory.Identifier)
    val internalTarget: CodeSpanParsers  = span("_`", "`", CodeCategory.Identifier)
    val footnote: CodeSpanParsers        = span("[", "]_", CodeCategory.Markup.LinkTarget)

    val header: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
      anyOf(BaseParsers.punctuationChars.toSeq: _*).take(1) >> { startChar =>
        (anyOf(startChar.head).min(1) ~ ws ~ ('\n' ~> not(blankLine) ~> restOfLine) ~ anyOf(startChar.head).min(1) ~ ws <~ lookAhead('\n')).map {
          case deco1 ~ spaces ~ text ~ deco2 ~ spaces2 => s"$startChar$deco1$spaces\n$text\n$deco2$spaces2"
        }
      }
    }

    val underlinedHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
      (not(blankLine) ~> restOfLine.map(_ + "\n") ~ anyOf(BaseParsers.punctuationChars.toSeq: _*).take(1)) >> {
        case text ~ decoStart =>
          (anyOf(decoStart.head).min(1) ~ ws <~ lookAhead('\n')).map {
            case deco ~ spaces => s"$text$decoStart$deco$spaces"
          }
      }
    }

    val transition: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
      anyOf(BaseParsers.punctuationChars.toSeq: _*).take(1) >> { startChar =>
        anyOf(startChar.head).min(1).map(startChar + _) <~ lookAhead(ws ~ '\n' ~ blankLine)
      }
    }

    val explicitItems: CodeSpanParsers = CodeSpanParsers('\n') {

      val subst = (rawSpan("|", "|", CodeCategory.Substitution) ~ ws ~ delimitedBy("::")).map {
        case sub ~ space ~ name => Seq(sub, CodeSpan(space), CodeSpan(s"$name::", CodeCategory.Identifier))
      }
      val linkTarget = rawSpan("_", ":", CodeCategory.Markup.LinkTarget).map(Seq(_))
      val footnote = rawSpan("[", "]", CodeCategory.Markup.LinkTarget).map(Seq(_))
      val directive = rawSpan("", "::", CodeCategory.Identifier, includeDelim = false)
        .map(Seq(_, CodeSpan("::", CodeCategory.Keyword)))

      (opt(atStart).map(_.isDefined) ~ (literal(".. ") ~> (subst | linkTarget | footnote | directive))).map {
        case startOfInput ~ res =>
          val nl = if (startOfInput) "" else "\n"
          CodeSpan(s"$nl.. ") +: res
      }
    }

    val fieldDef: CodeSpanParsers = CodeSpanParsers('\n') {
      (ws ~ rawSpan(":", ":", CodeCategory.AttributeName)).map {
        case space ~ name => Seq(CodeSpan(s"\n$space"), name)
      }
    }

    val parsers: CodeSpanParsers = explicitItems ++
      fieldDef ++
      strong ++
      em ++
      subst ++
      footnote ++
      lit ++
      ref ++
      internalTarget ++
      interpretedText ++
      roleName ++
      StringLiteral.Escape.char ++
      header ++
      transition ++
      underlinedHeader

    val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("reStructuredText", "rst")(parsers)
  }
  
  object laikaExtensions {

    val substitution: CodeSpanParsers = StringLiteral.Substitution.between("${", "}")

    val hoconBlock: CodeSpanParsers = CodeSpanParsers('{') {
      embeddedHocon("%", "%}", Set(CodeCategory.Keyword)).map(res => CodeSpan("{", CodeCategory.Keyword) +: res)
    }

    def embeddedHocon (start: String, end: String, delimCategory: Set[CodeCategory] = Set()): Parser[Seq[CodeSpan]] = {
      (literal(start) ~> EmbeddedCodeSpans.parser(delimitedBy(end), HOCON.highlighter.spanParsers)).map { hocon => // TODO - support nested objects
        CodeSpan(start, delimCategory) +: hocon :+ CodeSpan(end, delimCategory)
      }
    }

    val directive: CodeSpanParsers = CodeSpanParsers('@') {
      (':' ~> Identifier.standard.standaloneParser ~ opt(ws.min(1) ~ embeddedHocon("{", "}"))).map {
        case name ~ Some(spaces ~ hocon) => CodeSpan("@:", CodeCategory.Keyword) +: name +: CodeSpan(spaces) +: hocon
        case name ~ None => Seq(CodeSpan("@:", CodeCategory.Keyword), name)
      }
    }

    val fence: CodeSpanParsers = Keywords("@:@")

    val allExtensions: CodeSpanParsers = substitution ++ directive ++ fence ++ hoconBlock

    lazy val extendedMarkdown: SyntaxHighlighter = SyntaxHighlighter.build("laikaMarkdown", "laika-md")(
      allExtensions,
      md.parsers
    )

    lazy val extendedRst: SyntaxHighlighter = SyntaxHighlighter.build("laikaReStructuredText", "laika-rst")(
      allExtensions,
      rst.parsers
    )

    val enhancedStartTag: CodeSpanParsers = HTML.TagParser(CodeCategory.Tag.Name, "<", ">", HTML.nameParser).embed(
      StringLiteral.singleLine('\'').embed(HTML.ref, substitution).build,
      StringLiteral.singleLine('"').embed(HTML.ref, substitution).build,
      HTML.name(CodeCategory.AttributeName)
    ).build

    lazy val extendedHTML: SyntaxHighlighter = SyntaxHighlighter.build("laikaHTML", "laika-html")(
      allExtensions,
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
  
}
