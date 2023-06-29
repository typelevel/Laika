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

package laika.directive

import cats.implicits._
import laika.api.{ MarkupParser, RenderPhaseRewrite }
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.TestSourceBuilders
import laika.bundle.ParserBundle
import laika.config.ConfigBuilder
import laika.format.{ HTML, Markdown }
import laika.parse.markup.DocumentParser.TransformationError
import laika.parse.markup.RootParserProvider
import laika.parse.{ Parser, SourceFragment }
import munit.FunSuite

import scala.util.Try

class SpanDirectiveAPISpec extends FunSuite with TestSourceBuilders with RenderPhaseRewrite {

  object DirectiveSetup {
    import Spans.dsl._

    trait Empty {
      val directive = Spans.create("dir")(Spans.dsl.empty(Text("foo")))
    }

    trait RequiredPositionalAttribute {
      val directive = Spans.create("dir") { attribute(0).as[String] map (Text(_)) }
    }

    trait OptionalPositionalAttribute {

      val directive = Spans.create("dir") {
        attribute(0).as[Int].optional map (num => Text(num.map(_.toString).getOrElse("<>")))
      }

    }

    trait RequiredNamedAttribute {
      val directive = Spans.create("dir") { attribute("name").as[String] map (Text(_)) }
    }

    trait OptionalNamedAttribute {

      val directive = Spans.create("dir") {
        attribute("name").as[Int].optional map (num => Text(num.map(_.toString).getOrElse("<>")))
      }

    }

    trait AllAttributes {

      val directive = Spans.create("dir") {
        allAttributes.map { attrs =>
          val foo = attrs.get[String]("foo").toOption.get
          val bar = attrs.get[Int]("bar").toOption.get
          Text(s"$foo $bar")
        }
      }

    }

    trait RequiredBody {
      val directive = Spans.create("dir") { parsedBody map (SpanSequence(_)) }
    }

    trait SeparatedBody {

      sealed trait Child                               extends Product with Serializable
      case class Foo(content: Seq[Span])               extends Child
      case class Bar(content: Seq[Span], attr: String) extends Child

      val sep1 = Spans.separator("foo", min = 1) {
        parsedBody.map(Foo.apply)
      }

      val sep2 = Spans.separator("bar", max = 1) {
        (parsedBody, attribute(0).as[String]).mapN(Bar.apply)
      }

      val directive = Spans.create("dir") {
        separatedBody[Child](Seq(sep1, sep2)) map { multipart =>
          val seps = multipart.children.flatMap {
            case Foo(content)       => Text("foo") +: content
            case Bar(content, attr) => Text(attr) +: content
          }
          SpanSequence(multipart.mainBody ++ seps)
        }
      }

    }

    trait FullDirectiveSpec {

      val directive = Spans.create("dir") {
        (
          attribute(0).as[String],
          attribute(1).as[Int],
          attribute("strAttr").as[String].optional,
          attribute("intAttr").as[Int].optional,
          parsedBody
        ).mapN { (posStr, posInt, strAttr, intAttr, defBody) =>
          val sum = intAttr.getOrElse(0) + posInt
          val str = posStr + ":" + strAttr.getOrElse("..") + ":" + sum
          SpanSequence(Text(str) +: defBody)
        }
      }

    }

    trait DirectiveWithCustomBodyParser {

      import laika.parse.builders._
      import laika.parse.implicits._

      val directive = Spans.create("dir") {
        parsedBody(recParsers => anyChars.take(3) ~> recParsers.recursiveSpans(anyChars.line))
          .map(SpanSequence(_))
      }

    }

    trait DirectiveWithContextAccess {

      val directive = Spans.create("dir") {
        (rawBody, cursor).mapN { (body, cursor) =>
          Text(body + cursor.target.path)
        }
      }

    }

    trait DirectiveProducingResolver {

      case class DummyResolver(options: Options = NoOpt) extends SpanResolver {
        type Self = DummyResolver
        def resolve(cursor: DocumentCursor): Span = Text("foo")
        val source: SourceFragment                = generatedSource("@:dir")
        val unresolvedMessage                     = "broken"
        def withOptions(options: Options): Self   = copy(options = options)
        def runsIn(phase: RewritePhase): Boolean  = phase.isInstanceOf[RewritePhase.Render]
      }

      val directive = Spans.create("dir") {
        Spans.dsl.empty(DummyResolver())
      }

    }

    trait LinkDirectiveSetup {

      val directive = Links.eval("rfc") { (linkId, _) =>
        Try(Integer.parseInt(linkId))
          .toEither
          .fold(
            _ => Left(s"Not a valid RFC id: $linkId"),
            id => Right(SpanLink.external(s"http://tools.ietf.org/html/rfc$linkId")(s"RFC $id"))
          )
      }

      object bundle extends DirectiveRegistry {
        override def spanDirectives: Seq[Spans.Directive]         = Nil
        override def blockDirectives: Seq[Blocks.Directive]       = Nil
        override def templateDirectives: Seq[Templates.Directive] = Nil
        override def linkDirectives: Seq[Links.Directive]         = Seq(directive)
      }

      def parseAsMarkdown(input: String): Either[TransformationError, Block] = MarkupParser
        .of(Markdown)
        .using(bundle)
        .failOnMessages(MessageFilter.None)
        .build
        .parse(input)
        .flatMap(rewrite(HTML))
        .map(_.content.content.head)

    }

  }

  trait BaseParser {

    def directiveSupport: ParserBundle

    def input: String

    lazy val defaultParser: Parser[Span] = RootParserProvider.forParsers(
      markupExtensions = directiveSupport.markupExtensions
    ).standaloneSpanParser.evalMap { spans =>
      val seq = SpanSequence(spans)
      val doc = Document(
        Root,
        RootElement(seq),
        config = ConfigBuilder.empty.withValue("ref", "value").build
      )
      OperationConfig.default.rewriteRulesFor(doc, RewritePhase.Render(HTML))
        .map(_.rewriteSpan(seq))
        .leftMap(_.message)
    }

    def invalid(fragment: String, error: String): InvalidSpan =
      InvalidSpan(error, source(fragment, input))

    def invalid(fragment: String, error: String, path: Path): InvalidSpan =
      InvalidSpan(error, source(fragment, input, path))

    def ss(spans: Span*): Span = SpanSequence(spans)

    def run(resultSpans: Span*): Unit =
      assertEquals(defaultParser.parse(input).toEither, Right(SpanSequence(resultSpans)))

  }

  trait SpanParser extends BaseParser {
    def directive: Spans.Directive

    lazy val directiveSupport: ParserBundle =
      DirectiveSupport.withDirectives(Seq(), Seq(directive), Nil, Nil).parsers

  }

  trait LinkParser extends BaseParser {
    def directive: Links.Directive

    lazy val directiveSupport: ParserBundle =
      DirectiveSupport.withDirectives(Seq(), Nil, Nil, Seq(directive)).parsers

  }

  import DirectiveSetup._

  test("empty directive") {
    new SpanParser with Empty {
      val input = "aa @:dir bb"
      run(Text("aa foo bb"))
    }
  }

  test("directive producing a span resolver") {
    new SpanParser with DirectiveProducingResolver {
      val input = "aa @:dir bb"
      run(Text("aa foo bb"))
    }
  }

  test("directive with one required default string attribute") {
    new SpanParser with RequiredPositionalAttribute {
      val input = "aa @:dir(foo) bb"
      run(Text("aa foo bb"))
    }
  }

  test("invalid - directive with a missing required positional attribute") {
    new SpanParser with RequiredPositionalAttribute {
      val input = "aa @:dir bb"
      val msg   =
        "One or more errors processing directive 'dir': required positional attribute at index 0 is missing"
      run(Text("aa "), invalid("@:dir", msg), Text(" bb"))
    }
  }

  test("directive with an optional default int attribute") {
    new SpanParser with OptionalPositionalAttribute {
      val input = "aa @:dir(5) bb"
      run(Text("aa 5 bb"))
    }
  }

  test("invalid - directive with an optional invalid default int attribute") {
    new SpanParser with OptionalPositionalAttribute {
      val input = "aa @:dir(foo) bb"
      val msg   =
        "One or more errors processing directive 'dir': error converting positional attribute at index 0: not an integer: foo"
      run(Text("aa "), invalid("@:dir(foo)", msg), Text(" bb"))
    }
  }

  test("directive with a missing optional default int attribute") {
    new SpanParser with OptionalPositionalAttribute {
      val input = "aa @:dir bb"
      run(Text("aa <> bb"))
    }
  }

  test("directive with one required named string attribute") {
    new SpanParser with RequiredNamedAttribute {
      val input = "aa @:dir { name=foo } bb"
      run(Text("aa foo bb"))
    }
  }

  test("directive with a named string attribute value in quotes") {
    new SpanParser with RequiredNamedAttribute {
      val input = """aa @:dir { name="foo bar" } bb"""
      run(Text("aa foo bar bb"))
    }
  }

  test("invalid - directive with a missing required named attribute") {
    new SpanParser with RequiredNamedAttribute {
      val input = "aa @:dir bb"
      val msg   =
        "One or more errors processing directive 'dir': required attribute 'name' is missing"
      run(Text("aa "), invalid("@:dir", msg), Text(" bb"))
    }
  }

  test("invalid - directive with an invalid HOCON string attribute (missing closing quote)") {
    new SpanParser with RequiredNamedAttribute {
      val input = """aa @:dir { name="foo bar } bb"""
      val msg   =
        s"""One or more errors processing directive 'dir': Multiple errors parsing HOCON: [1.30] failure: Expected closing '"'
           |
           |$input
           |                             ^""".stripMargin
      run(Text("aa "), invalid("@:dir { name=\"foo bar } bb", msg))
    }
  }

  test(
    "invalid - directive with an invalid HOCON string attribute (invalid character in in unquoted string)"
  ) {
    new SpanParser with RequiredNamedAttribute {
      val input = """aa @:dir { name = foo ? bar } bb"""
      val msg   =
        s"""One or more errors processing directive 'dir': Multiple errors parsing HOCON: [1.23] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\\n', '}'
           |
           |$input
           |                      ^""".stripMargin
      run(Text("aa "), invalid("@:dir { name = foo ? bar }", msg), Text(" bb"))
    }
  }

  test("directive with an optional named int attribute") {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:dir { name=5 } bb"
      run(Text("aa 5 bb"))
    }
  }

  test("invalid - directive with an optional invalid named int attribute") {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:dir { name=foo } bb"
      val msg   =
        "One or more errors processing directive 'dir': error converting attribute 'name': not an integer: foo"
      run(Text("aa "), invalid("@:dir { name=foo }", msg), Text(" bb"))
    }
  }

  test("directive with a missing optional named int attribute") {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:dir bb"
      run(Text("aa <> bb"))
    }
  }

  test("directive with the allAttributes combinator") {
    new SpanParser with AllAttributes {
      val input = "aa @:dir { foo=Planet, bar=42 } bb"
      run(Text("aa Planet 42 bb"))
    }
  }

  test("directive with a body") {
    new SpanParser with RequiredBody {
      val input = "aa @:dir some ${ref} text @:@ bb"
      val body  = ss(Text(" some value text "))
      run(Text("aa "), body, Text(" bb"))
    }
  }

  test("support a directive with a nested pair of braces") {
    new SpanParser with RequiredBody {
      val input = "aa @:dir some {ref} text @:@ bb"
      val body  = ss(Text(" some {ref} text "))
      run(Text("aa "), body, Text(" bb"))
    }
  }

  test("invalid - directive with a missing body") {
    new SpanParser with RequiredBody {
      val input = "aa @:dir bb"
      val msg   = "One or more errors processing directive 'dir': required body is missing"
      run(Text("aa "), invalid("@:dir", msg), Text(" bb"))
    }
  }

  test("directive with a separated body") {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar(baz) ccc @:@ bb"""
      val body  = SpanSequence(Text(" aaa foo bbb baz ccc "))
      run(Text("aa "), body, Text(" bb"))
    }
  }

  test("invalid - directive with an invalid separator") {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar ccc @:@ bb"""
      val msg   =
        "One or more errors processing directive 'dir': One or more errors processing separator directive 'bar': required positional attribute at index 0 is missing"
      val src   = input.slice(3, 36)
      run(Text("aa "), invalid(src, msg), Text(" bb"))
    }
  }

  test("invalid - directive with a separator not meeting the min count requirements") {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:bar(baz) ccc @:@ bb"""
      val msg   =
        "One or more errors processing directive 'dir': too few occurrences of separator directive 'foo': expected min: 1, actual: 0"
      val src   = input.slice(3, 31)
      run(Text("aa "), invalid(src, msg), Text(" bb"))
    }
  }

  test("invalid - directive with a separator exceeding the max count constraint") {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar(baz) ccc @:bar(baz) ddd @:@ bb"""
      val msg   =
        "One or more errors processing directive 'dir': too many occurrences of separator directive 'bar': expected max: 1, actual: 2"
      val src   = input.drop(3).dropRight(3)
      run(Text("aa "), invalid(src, msg), Text(" bb"))
    }
  }

  test("detect an orphaned separator directive") {
    new SpanParser with SeparatedBody {
      val input = "aa @:foo bb"
      val msg   = "Orphaned separator directive with name 'foo'"
      run(Text("aa "), invalid("@:foo", msg), Text(" bb"))
    }
  }

  test("full directive spec with all elements present") {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir(foo, 4) { strAttr=str, intAttr=7 } 1 ${ref} 2 @:@ bb"
      val body  = ss(
        Text("foo:str:11 1 value 2 ")
      )
      run(Text("aa "), body, Text(" bb"))
    }
  }

  test("full directive spec with all elements present with attributes spanning two lines") {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir(foo,4) {\n strAttr=str\nintAttr=7 \n} 1 ${ref} 2 @:@ bb"
      val body  = ss(
        Text("foo:str:11 1 value 2 ")
      )
      run(Text("aa "), body, Text(" bb"))
    }
  }

  test("full directive spec with all optional elements missing") {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir(foo,4) 1 ${ref} 2 @:@ bb"
      val body  = ss(
        Text("foo:..:4 1 value 2 ")
      )
      run(Text("aa "), body, Text(" bb"))
    }
  }

  test("invalid - full directive spec with positional attributes and the body missing") {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir { strAttr=str } bb"
      val msg   =
        "One or more errors processing directive 'dir': required positional attribute at index 0 is missing, required positional attribute at index 1 is missing, required body is missing"
      run(Text("aa "), invalid("@:dir { strAttr=str }", msg), Text(" bb"))
    }
  }

  test("directive with a custom body parser") {
    new DirectiveWithCustomBodyParser with SpanParser {
      val input = "aa @:dir some ${ref} text @:@ bb"
      val body  = ss(Text("me value text "))
      run(Text("aa "), body, Text(" bb"))
    }
  }

  test("directive with a body and cursor access") {
    new DirectiveWithContextAccess with SpanParser {
      val input = "aa @:dir text @:@ bb"
      run(Text("aa  text / bb"))
    }
  }

  test("invalid - directive with an unknown name") {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:foo { name=foo } bb"
      val msg   =
        "One or more errors processing directive 'foo': No span directive registered with name: foo"
      run(Text("aa "), invalid("@:foo { name=foo }", msg), Text(" bb"))
    }
  }

  test("link directive") {
    new LinkParser with LinkDirectiveSetup {
      val input = "aa @:rfc(222) bb"
      run(
        Text("aa "),
        SpanLink.external("http://tools.ietf.org/html/rfc222")("RFC 222").withStyles("rfc"),
        Text(" bb")
      )
    }
  }

  test("link directive inside a native link expression") {
    new LinkParser with LinkDirectiveSetup {
      val input = "aa [RFC-222][@:rfc(222)] bb"
      assertEquals(
        parseAsMarkdown(input),
        Right(
          Paragraph(
            Text("aa "),
            SpanLink.external("http://tools.ietf.org/html/rfc222")("RFC-222").withStyles("rfc"),
            Text(" bb")
          )
        )
      )
    }
  }

  test("unknown link directive") {
    new LinkParser with LinkDirectiveSetup {
      val input = "aa [RFC-222][@:rfx(222)] bb"
      assertEquals(
        parseAsMarkdown(input),
        Right(
          Paragraph(
            Text("aa "),
            invalid("[RFC-222][@:rfx(222)]", "Unknown link directive: rfx", defaultPath),
            Text(" bb")
          )
        )
      )
    }
  }

  test("invalid link directive") {
    new LinkParser with LinkDirectiveSetup {
      val input = "aa [RFC-222][@:rfc(foo)] bb"
      assertEquals(
        parseAsMarkdown(input),
        Right(
          Paragraph(
            Text("aa "),
            invalid(
              "[RFC-222][@:rfc(foo)]",
              "Invalid link directive: Not a valid RFC id: foo",
              defaultPath
            ),
            Text(" bb")
          )
        )
      )
    }
  }

  test("invalid link directive syntax") {
    new LinkParser with LinkDirectiveSetup {
      val input = "aa [RFC-222][@:rfc foo] bb"
      assertEquals(
        parseAsMarkdown(input),
        Right(
          Paragraph(
            Text("aa "),
            invalid(
              "[RFC-222][@:rfc foo]",
              "Invalid link directive: `(' expected but `f` found",
              defaultPath
            ),
            Text(" bb")
          )
        )
      )
    }
  }

}
