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

package laika.parse.directive

import cats.data.{ NonEmptyChain, NonEmptySet }
import laika.api.bundle.{ AttributeKey, BlockParserBuilder, SpanParserBuilder }
import laika.api.config.Key
import laika.ast.*
import laika.parse.builders.*
import laika.parse.hocon.*
import laika.parse.implicits.*
import laika.parse.markup.{ RecursiveParsers, RecursiveSpanParsers }
import laika.parse.text.{ CharGroup, PrefixedParser }
import laika.parse.{
  BlockSource,
  Failure,
  LineSource,
  Message,
  Parser,
  SourceCursor,
  SourceFragment
}

/** Parsers for all types of custom directives that can be used
  *  in templates or as inline or block elements in markup documents.
  *
  *  @author Jens Halm
  */
private[laika] object DirectiveParsers {

  case class DirectiveSpec(name: String, fence: String)

  type BodyParserBuilder = DirectiveSpec => Parser[Option[SourceFragment]]

  /** Parses a HOCON-style reference enclosed between `\${` and `}` that may be marked as optional (`\${?some.param}`).
    */
  def hoconReference[T](
      f: (Key, Boolean, SourceFragment) => T,
      e: InvalidSpan => T
  ): PrefixedParser[T] =
    ("${" ~> opt("?") ~ HoconParsers.concatenatedKey(NonEmptySet.one('}')) <~ "}").withCursor.map {
      case (optional ~ key, source) =>
        key.fold(
          invalid =>
            e(
              InvalidSpan(
                s"Invalid HOCON reference: '${source.input}': ${invalid.failure.toString}",
                source
              )
            ),
          key => f(key, optional.isEmpty, source)
        )
    }

  /** Represents one part of a directive (an attribute or a body element).
    */
  case class Part(key: AttributeKey, content: String)

  /** Represents the parsed but unprocessed content of a directive.
    */
  case class ParsedDirective(
      name: String,
      attributes: ObjectBuilderValue,
      body: Option[SourceFragment]
  )

  /** Parses horizontal whitespace or newline characters.
    */
  lazy val wsOrNl: Parser[String] = anyOf(' ', '\t', '\n')

  /** Parses a name declaration that start with a letter and
    *  continues with letters, numbers or the symbols '-' or '_'.
    */
  lazy val nameDecl: Parser[String] =
    (oneOf(CharGroup.alpha) ~ anyOf(CharGroup.alphaNum.add('-').add('_'))).source

  /** Parses a full directive declaration, containing all its attributes,
    * but not the body elements.
    */
  def declarationParser(
      supportsCustomFence: Boolean = false
  ): Parser[(String, ObjectBuilderValue, String)] = {
    import HoconParsers._

    val defaultFence = success("@:@")
    val fence        =
      if (supportsCustomFence) (ws ~> anyNot(' ', '\n', '\t').take(3)) | defaultFence
      else defaultFence

    val quotedAttribute   = (ws ~ "\"") ~> text(delimitedBy('"')).embed("\\" ~> oneChar) <~ ws
    val unquotedAttribute = text(delimitedBy(',', ')').keepDelimiter).embed("\\" ~> oneChar) <~ ws
    val positionalAttributes = opt(
      ws ~> "(" ~> (quotedAttribute | unquotedAttribute).rep(",")
        .map(values =>
          BuilderField(
            AttributeKey.Positional.key,
            ArrayBuilderValue(values.map(sv => ValidStringValue(sv.trim)))
          )
        ) <~ ")"
    )

    val closingAttributes = literal("}").as(Option.empty[SourceCursor]) |
      success(()).withCursor.map { case (_, ctx) => Some(ctx) }

    val hoconAttributes = opt(ws ~> lazily("{" ~> objectMembers ~ closingAttributes))

    val attributeSection = (positionalAttributes ~ hoconAttributes).map {
      case posAttrs ~ Some(obj ~ optCtx) =>
        val positional = posAttrs.toSeq
        val attrs      = obj.copy(values = positional ++ obj.values)
        optCtx match {
          case Some(ctx) if ConfigResolver.extractErrors(obj).isEmpty =>
            val fail = Failure(Message.fixed("Missing closing brace for attribute section"), ctx)
            ObjectBuilderValue(
              Seq(BuilderField("failure", InvalidBuilderValue(SelfReference, fail)))
            )
          case _                                                      =>
            attrs
        }
      case posAttrs ~ None               => ObjectBuilderValue(posAttrs.toSeq)
    }

    ("@:" ~> nameDecl ~ attributeSection ~ fence).map { case name ~ attrs ~ fencePattern =>
      (name, attrs, fencePattern)
    }
  }

  /** Parses one directive instance containing its name declaration,
    *  all attributes and all body elements.
    *
    *  @param bodyContent the parser for the body content which is different for a block directive than for a span or template directive
    *  @param supportsCustomFence indicates whether the directive declaration allows the addition of a custom closing fence
    */
  def directiveParser(
      bodyContent: BodyParserBuilder,
      supportsCustomFence: Boolean = false
  ): Parser[ParsedDirective] =
    declarationParser(supportsCustomFence) >> { case (name, attrs, fence) =>
      bodyContent(DirectiveSpec(name, fence)).map(content => ParsedDirective(name, attrs, content))
    }

  val nestedBraces: PrefixedParser[Text] = ("{" ~> delimitedBy('}')).source.map(Text(_))

}

/** Provides the parser definitions for span directives in markup documents.
  */
private[laika] object SpanDirectiveParsers {

  import DirectiveParsers._
  import laika.api.bundle.SpanDirectives

  val contextRef: SpanParserBuilder =
    SpanParserBuilder.standalone(hoconReference(MarkupContextReference(_, _, _), identity))

  def spanDirective(directives: Map[String, SpanDirectives.Directive]): SpanParserBuilder =
    SpanParserBuilder.recursive(rec => spanDirectiveParser(directives)(rec))

  def spanDirectiveParser(
      directives: Map[String, SpanDirectives.Directive]
  )(recParsers: RecursiveSpanParsers): PrefixedParser[Span] = {

    import recParsers._

    val separators              = directives.values.flatMap(_.separators).toSet
    val body: BodyParserBuilder = spec =>
      if (directives.get(spec.name).exists(_.hasBody))
        recursiveSpans(delimitedBy(spec.fence)).source.line.map { src =>
          Some(LineSource(src.input.dropRight(spec.fence.length), src.parent))
        } | success(None)
      else success(None)

    PrefixedParser('@') {
      directiveParser(body).withCursor.map { case (res, source) =>
        if (separators.contains(res.name)) SpanDirectives.SeparatorInstance(res, source)
        else SpanDirectives.DirectiveInstance(directives.get(res.name), res, recParsers, source)
      }
    }
  }

}

/** Provides the parser definitions for block directives in markup documents.
  */
private[laika] object BlockDirectiveParsers {

  import DirectiveParsers._
  import laika.api.bundle.BlockDirectives

  def blockDirective(directives: Map[String, BlockDirectives.Directive]): BlockParserBuilder =
    BlockParserBuilder.recursive(blockDirectiveParser(directives))

  def blockDirectiveParser(
      directives: Map[String, BlockDirectives.Directive]
  )(recParsers: RecursiveParsers): PrefixedParser[Block] = {

    val separators              = directives.values.flatMap(_.separators).toSet
    val noBody                  = wsEol.as(None)
    val body: BodyParserBuilder = spec =>
      if (directives.get(spec.name).exists(_.hasBody)) {
        val closingFenceP = spec.fence <~ wsEol
        wsEol ~> (not(closingFenceP | eof) ~> restOfLine.line).rep <~ closingFenceP ^^ { lines =>
          val trimmedLines =
            lines.dropWhile(_.input.trim.isEmpty).reverse.dropWhile(_.input.trim.isEmpty).reverse
          val lineChain    = NonEmptyChain.fromSeq(trimmedLines).getOrElse(
            NonEmptyChain.one(LineSource("", SourceCursor("")))
          )
          Some(BlockSource(lineChain))
        }
      } | noBody
      else noBody

    PrefixedParser('@') {
      directiveParser(body, supportsCustomFence = true).withCursor.map { case (res, source) =>
        val trimmedSource =
          if (source.input.lastOption.contains('\n'))
            LineSource(source.input.dropRight(1), source)
          else source
        if (separators.contains(res.name)) BlockDirectives.SeparatorInstance(res, trimmedSource)
        else
          BlockDirectives.DirectiveInstance(
            directives.get(res.name),
            res,
            recParsers,
            trimmedSource
          )
      }
    }
  }

}
