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
import laika.ast.{ CodeSpan, ~ }
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.code.implicits._
import laika.parse.code.common.StringLiteral
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.rst.BaseParsers
import laika.rst.InlineParsers.{ markupEnd, markupStart }

/** @author Jens Halm
  */
object ReStructuredTextSyntax extends SyntaxHighlighter {

  // raw = does not perform checks for rst inline markup recognition
  def rawSpan(start: String, end: String, category: CodeCategory): Parser[CodeSpan] =
    (literal(start) ~ delimitedBy(end).failOn('\n')).source.asCode(category)

  def rawSpan(end: String, category: CodeCategory): Parser[CodeSpan] =
    delimitedBy(end).failOn('\n').asCode(category)

  private def span(start: String, end: String, category: CodeCategory): CodeSpanParser =
    CodeSpanParser(category) {
      val endDelim = if (end == "*") delimiter("*").nextNot('*') else delimiter(end)
      (markupStart(start, end) ~ delimitedBy(markupEnd(endDelim))).source
    }

  val strong: CodeSpanParser = span("**", "**", CodeCategory.Markup.Emphasized)
  val em: CodeSpanParser     = span("*", "*", CodeCategory.Markup.Emphasized)
  val lit: CodeSpanParser    = span("``", "``", CodeCategory.StringLiteral)

  val ref: CodeSpanParser = span("`", "`__", CodeCategory.Markup.LinkTarget) ++
    span("`", "`_", CodeCategory.Markup.LinkTarget)

  val subst: CodeSpanParser = span("|", "|__", CodeCategory.Substitution) ++
    span("|", "|_", CodeCategory.Substitution) ++
    span("|", "|", CodeCategory.Substitution)

  val interpretedText: CodeSpanParser = span("`", "`", CodeCategory.Substitution)
  val roleName: CodeSpanParser        = span(":", ":", CodeCategory.Identifier)
  val internalTarget: CodeSpanParser  = span("_`", "`", CodeCategory.Identifier)
  val footnote: CodeSpanParser        = span("[", "]_", CodeCategory.Markup.LinkTarget)

  val header: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Headline) {
    oneOf(BaseParsers.punctuationChars) >> { startChar =>
      (someOf(startChar.head) ~ ws ~ ("\n" ~> not(blankLine) ~> restOfLine) ~ someOf(
        startChar.head
      ) ~ ws <~ nextIn('\n'))
        .source.map { header => s"$startChar$header" }
    }
  }

  val underlinedHeader: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Headline) {
    ((not(blankLine) ~> restOfLine.map(_ + "\n")) ~ oneOf(BaseParsers.punctuationChars)) >> {
      case text ~ decoStart =>
        (someOf(decoStart.head) ~ ws <~ nextIn('\n')).source.map { header =>
          s"$text$decoStart$header"
        }
    }
  }

  val transition: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Fence) {
    oneOf(BaseParsers.punctuationChars) >> { startChar =>
      someOf(startChar.head).map(startChar + _) <~ lookAhead(ws ~ "\n" ~ blankLine)
    }
  }

  val explicitItems: CodeSpanParser = CodeSpanParser.onLineStart {

    val subst      = (rawSpan("|", "|", CodeCategory.Substitution) ~ ws ~ delimitedBy("::")).map {
      case sub ~ space ~ name =>
        Seq(sub, CodeSpan(space), CodeSpan(s"$name::", CodeCategory.Identifier))
    }
    val linkTarget = rawSpan("_", ":", CodeCategory.Markup.LinkTarget).map(Seq(_))
    val footnote   = rawSpan("[", "]", CodeCategory.Markup.LinkTarget).map(Seq(_))
    val directive  =
      rawSpan("::", CodeCategory.Identifier).map(Seq(_, CodeSpan("::", CodeCategory.Keyword)))

    (literal(".. ") ~> (subst | linkTarget | footnote | directive)).map { res =>
      CodeSpan(s".. ") +: res
    }
  }

  val fieldDef: CodeSpanParser = CodeSpanParser.onLineStart {
    (ws ~ rawSpan(":", ":", CodeCategory.AttributeName)).map { case space ~ name =>
      Seq(CodeSpan(space), name)
    }
  }

  val language: NonEmptyList[String] = NonEmptyList.of("reStructuredText", "rst")

  val spanParsers: Seq[CodeSpanParser] = Seq(
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

}
