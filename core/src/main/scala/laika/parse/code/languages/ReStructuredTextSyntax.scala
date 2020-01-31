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

import cats.data.NonEmptyList
import laika.ast.{CodeSpan, ~}
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.code.common.{NumberLiteral, StringLiteral}
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers._
import laika.rst.BaseParsers
import laika.rst.InlineParsers.{delimitedByMarkupEnd, markupStart}

/**
  * @author Jens Halm
  */
object ReStructuredTextSyntax extends SyntaxHighlighter {

  import NumberLiteral._

  // raw = does not perform checks for rst inline markup recognition
  def rawSpan (start: String, end: String, category: CodeCategory, includeDelim: Boolean = true): Parser[CodeSpan] =
    (literalOrEmpty(start) ~ delimitedBy(end).failOn('\n')).concat.map {
      res => CodeSpan(if (includeDelim) s"$res$end" else res, category)
    }

  private def span (start: String, end: String, category: CodeCategory, postCondition: Parser[Any] = success(())): CodeSpanParser =
    CodeSpanParser(category) {
      markupStart(start, end) ~> delimitedByMarkupEnd(end, postCondition) ^^ { text => s"$start$text$end" }
    }

  val newLine: Parser[String] = atStart ^^^ "" | "\n"

  val strong: CodeSpanParser = span("**", "**", CodeCategory.Markup.Emphasized)
  val em: CodeSpanParser     = span("*", "*", CodeCategory.Markup.Emphasized, not("*"))
  val lit: CodeSpanParser    = span("``", "``", CodeCategory.StringLiteral)
  val ref: CodeSpanParser    = span("`", "`__", CodeCategory.Markup.LinkTarget) ++ span("`", "`_", CodeCategory.Markup.LinkTarget)
  val subst: CodeSpanParser  = span("|", "|__", CodeCategory.Substitution) ++ span("|", "|_", CodeCategory.Substitution) ++ span("|", "|", CodeCategory.Substitution)

  val interpretedText: CodeSpanParser = span("`", "`", CodeCategory.Substitution)
  val roleName: CodeSpanParser        = span(":", ":", CodeCategory.Identifier)
  val internalTarget: CodeSpanParser  = span("_`", "`", CodeCategory.Identifier)
  val footnote: CodeSpanParser        = span("[", "]_", CodeCategory.Markup.LinkTarget)

  val header: CodeSpanParser = CodeSpanParser(CodeCategory.Markup.Headline) {
    PrefixedParser('\n') {
      newLine ~ anyOf(BaseParsers.punctuationChars.toSeq: _*).take(1) >> { case nl ~ startChar =>
        (anyOf(startChar.head).min(1) ~ ws ~ ('\n' ~> not(blankLine) ~> restOfLine) ~ anyOf(startChar.head).min(1) ~ ws <~ lookAhead('\n')).map {
          case deco1 ~ spaces ~ text ~ deco2 ~ spaces2 => s"$nl$startChar$deco1$spaces\n$text\n$deco2$spaces2"
        }
      }
    }
  }

  val underlinedHeader: CodeSpanParser = CodeSpanParser(CodeCategory.Markup.Headline) {
    PrefixedParser('\n') {
      (newLine ~ (not(blankLine) ~> restOfLine.map(_ + "\n")) ~ anyOf(BaseParsers.punctuationChars.toSeq: _*).take(1)) >> {
        case nl ~ text ~ decoStart =>
          (anyOf(decoStart.head).min(1) ~ ws <~ lookAhead('\n')).map {
            case deco ~ spaces => s"$nl$text$decoStart$deco$spaces"
          }
      }
    }
  }

  val transition: CodeSpanParser = CodeSpanParser(CodeCategory.Markup.Fence) {
    PrefixedParser('\n') {
      newLine ~ anyOf(BaseParsers.punctuationChars.toSeq: _*).take(1) >> { case nl ~ startChar =>
        anyOf(startChar.head).min(1).map(nl + startChar + _) <~ lookAhead(ws ~ '\n' ~ blankLine)
      }
    }
  }

  val explicitItems: CodeSpanParser = CodeSpanParser('\n') {

    PrefixedParser('\n') {
      val subst = (rawSpan("|", "|", CodeCategory.Substitution) ~ ws ~ delimitedBy("::")).map {
        case sub ~ space ~ name => Seq(sub, CodeSpan(space), CodeSpan(s"$name::", CodeCategory.Identifier))
      }
      val linkTarget = rawSpan("_", ":", CodeCategory.Markup.LinkTarget).map(Seq(_))
      val footnote = rawSpan("[", "]", CodeCategory.Markup.LinkTarget).map(Seq(_))
      val directive = rawSpan("", "::", CodeCategory.Identifier, includeDelim = false)
        .map(Seq(_, CodeSpan("::", CodeCategory.Keyword)))

      (newLine ~ (literal(".. ") ~> (subst | linkTarget | footnote | directive))).map {
        case nl ~ res =>
          CodeSpan(s"$nl.. ") +: res
      }
    }
  }

  val fieldDef: CodeSpanParser = CodeSpanParser('\n') {
    PrefixedParser('\n') {
      (newLine ~ ws ~ rawSpan(":", ":", CodeCategory.AttributeName)).map {
        case nl ~ space ~ name => Seq(CodeSpan(s"\n$space"), name)
      }
    }
  }

  val language: NonEmptyList[String] = NonEmptyList.of("reStructuredText", "rst")

  val spanParsers: Seq[CodeSpanParser] = Seq(explicitItems,
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
    underlinedHeader)

}
