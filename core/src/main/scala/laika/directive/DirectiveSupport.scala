/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.directive

import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders}
import laika.directive.Directives.{Blocks, Spans}

/**
  * @author Jens Halm
  */
class DirectiveSupport (blockDirectives: Seq[Blocks.Directive], spanDirectives: Seq[Spans.Directive]) extends ExtensionBundle {

  override lazy val parserDefinitions: ParserDefinitionBuilders = ParserDefinitionBuilders(
    blockParsers = Seq(BlockDirectiveParsers.blockDirective(Blocks.toMap(blockDirectives))),
    spanParsers = Seq(SpanDirectiveParsers.spanDirective(Spans.toMap(spanDirectives)), SpanDirectiveParsers.contextRef)
  )

  /*
  TODO - implement:
  - docTypeMatcher (recognizing templates)
  - default templates (also in Theme, but this one is specific to directives)

  - template parser
  - template directive
  */

  def withDirectives (newBlockDirectives: Seq[Blocks.Directive], newSpanDirectives: Seq[Spans.Directive]) : DirectiveSupport =
    new DirectiveSupport(blockDirectives ++ newBlockDirectives, spanDirectives ++ newSpanDirectives)

}

object DirectiveSupport extends DirectiveSupport(Nil, Nil)


trait DirectiveRegistry extends ExtensionBundle {

  /**  Registers the specified span directives.
    *
    *  Example:
    *
    *  {{{
    *  object MyDirectives extends DirectiveRegistry {
    *    val spanDirectives = Seq(
    *      Spans.create("ticket") {
    *        (attribute(Default) ~ attribute("param").optional) { (ticketNo, param) =>
    *          val base = "http://tickets.service.com/"+ticketNo
    *          val url = base + (param map (p => "?param="+p) getOrElse "")
    *          ExternalLink(Seq(Text("Ticket "+ticketNo)), url, options = Styles("ticket"))
    *        }
    *      }
    *    )
    *    val blockDirectives = Seq()
    *  }
    *
    *  Transform from Markdown to HTML using MyDirectives fromFile "hello.md" toFile "hello.html"
    *  }}}
    *
    *  The code above registers a span directive that detects markup like
    *  `@:ticket 2356.` and turns it into an external link node for the
    *  URL `http://tickets.service.com/2356`.
    *
    *  For more details on implementing Laika directives see [[laika.directive.Directives]].
    */
  def spanDirectives: Seq[Spans.Directive]

  /**  Registers the specified block directives.
    *
    *  Example:
    *
    *  {{{
    *  case class Note (title: String, content: Seq[Block], options: Options = NoOpt)
    *                                                       extends Block with BlockContainer[Note]
    *  object MyDirectives extends DirectiveRegistry {
    *    val blockDirectives = Seq(
    *      Blocks.create("note") {
    *        (attribute(Default) ~ body(Default))(Note(_,_))
    *      }
    *    )
    *    val spanDirectives = Seq()
    *  }
    *
    *  Transform from Markdown to HTML using MyDirectives fromFile "hello.md" toFile "hello.html"
    *  }}}
    *
    *  For more details on implementing Laika directives see [[laika.directive.Directives]].
    */
  def blockDirectives: Seq[Blocks.Directive]


  override def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] = {
    case ds: DirectiveSupport => ds.withDirectives(blockDirectives, spanDirectives)
  }

}
