/*
 * Copyright 2012-2022 the original author or authors.
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

package laika.internal.rewrite

import laika.ast._
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.api.config.Config.ConfigResult

import scala.collection.mutable.ListBuffer

private[laika] object TemplateFormatter extends RewriteRulesBuilder {

  private def joinTextSpans(spans: Seq[Span]): Seq[Span] =
    if (spans.isEmpty) spans
    else
      spans.sliding(2).foldLeft(spans.take(1)) {
        case (acc, Seq(Text(_, NoOpt), Text(txt2, NoOpt))) =>
          acc.dropRight(1) :+ Text(acc.last.asInstanceOf[Text].content + txt2)
        case (acc, Seq(_, other))                          => acc :+ other
        case (acc, _)                                      => acc
      }

  private def format(spans: Seq[TemplateSpan]): Seq[TemplateSpan] = {
    def indentFor(text: String): Int = text.lastIndexOf('\n') match {
      case -1    => 0
      case index => if (text.drop(index).trim.isEmpty) text.length - index - 1 else 0
    }
    if (spans.isEmpty) spans
    else
      spans.sliding(2).foldLeft(new ListBuffer[TemplateSpan]() += spans.head) {
        case (buffer, Seq(TemplateString(text, NoOpt), TemplateElement(elem, 0, opt))) =>
          buffer += TemplateElement(elem, indentFor(text), opt)
        case (buffer, Seq(TemplateString(text, NoOpt), EmbeddedRoot(elem, 0, opt)))    =>
          buffer += EmbeddedRoot(elem, indentFor(text), opt)
        case (buffer, Seq(_, elem)) => buffer += elem
        case (buffer, _)            => buffer
      }.toList
  }

  def apply(cursor: DocumentCursor): ConfigResult[RewriteRules] = {

    val rules = RewriteRules.forBlocks {
      case TemplateRoot(spans, opt)     => Replace(TemplateRoot(format(spans), opt))
      case sc: SpanContainer with Block =>
        Replace(sc.withContent(joinTextSpans(sc.content)).asInstanceOf[Block])
    } ++ RewriteRules.forSpans { case sc: SpanContainer with Span =>
      Replace(sc.withContent(joinTextSpans(sc.content)).asInstanceOf[Span])
    } ++ RewriteRules.forTemplates { case TemplateSpanSequence(spans, opt) =>
      Replace(TemplateSpanSequence(format(spans), opt))
    }

    Right(rules)
  }

}
