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

package laika.config

import laika.api.MarkupParser
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.*
import laika.ast.sample.TestSourceBuilders
import laika.bundle.*
import laika.api.config.ConfigError.ConfigParserError
import laika.api.format.MarkupFormat
import MarkupFormat.MarkupParsers
import laika.api.config.{ Config, ConfigBuilder, ConfigError, ConfigParser, Origin }
import laika.ast.styles.{ StyleDeclaration, StylePredicate }
import laika.parse.*
import laika.parse.builders.*
import laika.parse.combinator.Parsers
import laika.parse.css.CSSParsers
import laika.parse.directive.ConfigHeaderParser
import laika.parse.implicits.*
import laika.parse.text.TextParsers
import laika.rewrite.ReferenceResolver.CursorKeys
import munit.FunSuite

trait ParserSetup {

  val defaultDocConfig: Config = {
    val base = ConfigBuilder.withOrigin(Origin(Origin.DocumentScope, Root / "doc")).build
    ConfigBuilder.withFallback(base).build
  }

  val defaultTextBlockParser: Parser[LineSource] = TextParsers.textLine.rep.min(1).mkLines.line

  def createParser(
      bundles: Seq[ExtensionBundle] = Nil,
      blocks: Seq[BlockParserBuilder] = Nil,
      spans: Seq[SpanParserBuilder] = Nil
  ): MarkupFormat = {

    new MarkupFormat {
      val fileSuffixes    = Set("foo")
      val blockParsers    = new MarkupParsers[BlockParserBuilder] {
        val all: Seq[BlockParserBuilder] = blocks
      }
      val spanParsers     = new MarkupParsers[SpanParserBuilder] {
        val all: Seq[SpanParserBuilder] = spans
      }
      lazy val extensions = bundles
    }

  }

}

trait BundleSetup extends TestSourceBuilders with ParserSetup {

  def createConfig(format: MarkupFormat, appBundles: ExtensionBundle*): OperationConfig = {
    val base = OperationConfig.default.withBundlesFor(format)
    appBundles.foldLeft(base) { (acc, bundle) => acc.withBundles(Seq(bundle)) }
  }

  val defaultConfig: OperationConfig = createConfig(createParser())
}

class BlockParserConfigSpec extends FunSuite with ParserSetup {

  private val input =
    """
      |>aaa
      |aaa
      |
      |+bbb
      |bbb
    """.stripMargin

  case class DecoratedBlock(deco: Char, content: Seq[Span], options: Options = NoOpt) extends Block
      with SpanContainer {
    type Self = DecoratedBlock
    def withContent(newContent: Seq[Span]): DecoratedBlock = copy(content = newContent)
    def withOptions(options: Options): DecoratedBlock      = copy(options = options)
  }

  def blockFor(deco: Char): BlockParserBuilder = blockFor(deco, deco)

  def blockFor(deco: Char, overrideDeco: Char): BlockParserBuilder =
    BlockParserBuilder.withSpans { spanParsers =>
      builders.oneOf(deco) ~> spanParsers.recursiveSpans(defaultTextBlockParser).map(
        DecoratedBlock(overrideDeco, _)
      )
    }

  def doc(blocks: (Char, String)*): Document =
    Document(
      Root / "doc",
      RootElement(blocks.map { case (deco, text) => DecoratedBlock(deco, Seq(Text(text))) })
    ).withConfig(defaultDocConfig)

  test("merge parsers from a host language with parsers from an app extension") {
    val format = createParser(blocks = Seq(blockFor('>')))

    val bundle = BundleProvider.forMarkupParser(blockParsers = Seq(blockFor('+')))

    assertEquals(
      MarkupParser.of(format).using(bundle).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa\naaa", '+' -> "bbb\nbbb").content)
    )
  }

  test("merge parsers from two app extensions") {
    val format = createParser()

    val bundle1 = BundleProvider.forMarkupParser(blockParsers = Seq(blockFor('+')))
    val bundle2 = BundleProvider.forMarkupParser(blockParsers = Seq(blockFor('>')))

    assertEquals(
      MarkupParser.of(format).using(bundle1, bundle2).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa\naaa", '+' -> "bbb\nbbb").content)
    )
  }

  test("let a parser from an app extension override a parser from the host language") {
    val format = createParser(blocks = Seq(blockFor('>'), blockFor('+')))

    val bundle = BundleProvider.forMarkupParser(blockParsers = Seq(blockFor('+', '!')))

    assertEquals(
      MarkupParser.of(format).using(bundle).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa\naaa", '!' -> "bbb\nbbb").content)
    )
  }

  test(
    "let a parser from the host language override a low-precedence parser from an app extension"
  ) {
    val format = createParser(blocks = Seq(blockFor('>'), blockFor('+')))

    val bundle = BundleProvider.forMarkupParser(blockParsers =
      Seq(
        BlockParserBuilder.standalone(literal("+").map(_ => Rule())).withLowPrecedence
      )
    )

    assertEquals(
      MarkupParser.of(format).using(bundle).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa\naaa", '+' -> "bbb\nbbb").content)
    )
  }

}

class SpanParserConfigSpec extends FunSuite with ParserSetup {

  import TextParsers.*

  val input = ">aaa +bbb"

  val blockParsers: Seq[BlockParserBuilder] = Seq(BlockParserBuilder.withSpans { spanParsers =>
    spanParsers.recursiveSpans(defaultTextBlockParser).map(Paragraph(_))
  })

  case class DecoratedSpan(deco: Char, text: String) extends Span {
    val options: Options = NoOpt
    type Self = DecoratedSpan
    def withOptions(options: Options): DecoratedSpan = this
  }

  def spanFor(deco: Char): SpanParserBuilder = spanFor(deco, deco)

  def spanFor(deco: Char, overrideDeco: Char): SpanParserBuilder =
    SpanParserBuilder.standalone {
      (deco.toString ~> anyNot(' ') <~ opt(" ")).map(DecoratedSpan(overrideDeco, _))
    }

  def doc(spans: (Char, String)*): Document =
    Document(
      Root / "doc",
      RootElement(
        Paragraph(
          spans.map { case (deco, text) => DecoratedSpan(deco, text) }
        )
      )
    ).withConfig(defaultDocConfig)

  def parser(spans: SpanParserBuilder*): MarkupFormat = createParser(
    spans = spans,
    blocks = blockParsers
  )

  test("merge parsers from a host language with parsers from an app extension") {
    val format = parser(spanFor('>'))

    val bundle = BundleProvider.forMarkupParser(spanParsers = Seq(spanFor('+')))

    assertEquals(
      MarkupParser.of(format).using(bundle).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa", '+' -> "bbb").content)
    )
  }

  test("merge parsers from two app extensions") {
    val format = parser()

    val bundle1 = BundleProvider.forMarkupParser(spanParsers = Seq(spanFor('+')))
    val bundle2 = BundleProvider.forMarkupParser(spanParsers = Seq(spanFor('>')))

    assertEquals(
      MarkupParser.of(format).using(bundle1, bundle2).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa", '+' -> "bbb").content)
    )
  }

  test("let a parser from an app extension override a parser from the host language") {
    val format = parser(spanFor('>'), spanFor('+'))

    val bundle = BundleProvider.forMarkupParser(spanParsers = Seq(spanFor('+', '!')))

    assertEquals(
      MarkupParser.of(format).using(bundle).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa", '!' -> "bbb").content)
    )
  }

  test(
    "let a parser from the host language override a low-precedence parser from an app extension"
  ) {
    val format = parser(spanFor('>'), spanFor('+'))

    val bundle = BundleProvider.forMarkupParser(spanParsers =
      Seq(
        SpanParserBuilder.standalone(literal("+").map(Text(_))).withLowPrecedence
      )
    )

    assertEquals(
      MarkupParser.of(format).using(bundle).build.parse(input).map(_.content),
      Right(doc('>' -> "aaa", '+' -> "bbb").content)
    )
  }

}

class ParserHookSpec extends FunSuite with ParserSetup {

  def parserBuilder(bundles: ExtensionBundle*): laika.api.builder.ParserBuilder = MarkupParser.of(
    createParser(
      blocks = Seq(BlockParserBuilder.standalone {
        TextParsers.textLine.map(Paragraph(_))
      }),
      bundles = bundles
    )
  )

  def preProcess(append: String): String => String = { raw =>
    raw + append
  }

  def appendString(root: RootElement, append: String): RootElement =
    root.copy(content = root.content.collect { case Paragraph(Seq(Text(text, _)), _) =>
      Paragraph(text + append)
    })

  def processDoc(append: String): UnresolvedDocument => UnresolvedDocument = { unresolved =>
    unresolved.copy(document =
      unresolved.document.withContent(appendString(unresolved.document.content, append))
    )
  }

  def processBlocks(append: String): Seq[Block] => Seq[Block] = { blocks =>
    blocks.collect { case Paragraph(Seq(Text(text, _)), _) =>
      Paragraph(text + append)
    }
  }

  def doc(text: String): Document =
    Document(Root / "doc", RootElement(text)).withConfig(defaultDocConfig)

  test(
    "preProcessInput - apply the hook from a parser extension before the hook in an app extension"
  ) {
    val parserBundle = BundleProvider.forParserHooks(preProcessInput = preProcess("!"))
    val appBundle    = BundleProvider.forParserHooks(preProcessInput = preProcess("?"))

    assertEquals(
      parserBuilder(parserBundle).using(appBundle).build.parse("foo").map(_.content),
      Right(doc("foo!?").content)
    )
  }

  test(
    "preProcessInput - apply the hook from an app extension before the hook in a subsequently installed app extension"
  ) {
    val appBundle1 = BundleProvider.forParserHooks(preProcessInput = preProcess("!"))
    val appBundle2 = BundleProvider.forParserHooks(preProcessInput = preProcess("?"))

    assertEquals(
      parserBuilder().using(appBundle1, appBundle2).build.parse("foo").map(_.content),
      Right(doc("foo!?").content)
    )
  }

  test("preProcessInput - provide the identity function when no hook is defined") {
    assertEquals(
      parserBuilder().build.parse("foo").map(_.content),
      Right(doc("foo").content)
    )
  }

  test(
    "postProcessBlocks hook- apply the hook from a parser extension before the hook in an app extension"
  ) {
    val parserBundle = BundleProvider.forParserHooks(postProcessDocument = processDoc("!"))
    val appBundle    = BundleProvider.forParserHooks(postProcessDocument = processDoc("?"))

    assertEquals(
      parserBuilder(parserBundle).using(appBundle).build.parse("foo").map(_.content),
      Right(doc("foo!?").content)
    )
  }

  test(
    "postProcessBlocks - apply the hook from an app extension before the hook in a subsequently installed app extension"
  ) {
    val appBundle1 = BundleProvider.forParserHooks(postProcessDocument = processDoc("!"))
    val appBundle2 = BundleProvider.forParserHooks(postProcessDocument = processDoc("?"))

    assertEquals(
      parserBuilder().using(appBundle1, appBundle2).build.parse("foo").map(_.content),
      Right(doc("foo!?").content)
    )
  }

  test("postProcessBlocks - provide the identity function when no hook is defined") {
    assertEquals(
      parserBuilder().build.parse("foo").map(_.content),
      Right(doc("foo").content)
    )
  }

  test(
    "postProcessDocument - apply the hook from a parser extension before the hook in an app extension"
  ) {
    val parserBundle = BundleProvider.forParserHooks(postProcessBlocks = processBlocks("!"))
    val appBundle    = BundleProvider.forParserHooks(postProcessBlocks = processBlocks("?"))

    assertEquals(
      parserBuilder(parserBundle).using(appBundle).build.parse("foo").map(_.content),
      Right(doc("foo!?").content)
    )
  }

  test(
    "postProcessDocument - apply the hook from an app extension before the hook in a subsequently installed app extension"
  ) {
    val appBundle1 = BundleProvider.forParserHooks(postProcessBlocks = processBlocks("!"))
    val appBundle2 = BundleProvider.forParserHooks(postProcessBlocks = processBlocks("?"))

    assertEquals(
      parserBuilder().using(appBundle1, appBundle2).build.parse("foo").map(_.content),
      Right(doc("foo!?").content)
    )
  }

  test("postProcessDocument - provide the identity function when no hook is defined") {
    assertEquals(
      parserBuilder().build.parse("foo").map(_.content),
      Right(doc("foo").content)
    )
  }

}

class ConfigProviderSpec extends FunSuite with BundleSetup {

  object BetweenBraces extends ConfigProvider {
    def markupConfigHeader: Parser[ConfigParser]    = ConfigHeaderParser.betweenLines("{{", "}}")
    def templateConfigHeader: Parser[ConfigParser]  = ConfigHeaderParser.betweenLines("{{", "}}")
    def configDocument(input: String): ConfigParser = ConfigParser.empty
  }

  object BetweenAngles extends ConfigProvider {
    def markupConfigHeader: Parser[ConfigParser]    = ConfigHeaderParser.betweenLines("<<", ">>")
    def templateConfigHeader: Parser[ConfigParser]  = ConfigHeaderParser.betweenLines("<<", ">>")
    def configDocument(input: String): ConfigParser = ConfigParser.empty
  }

  def parseWith(opConfig: OperationConfig, input: String): Either[ConfigError, Config] = opConfig
    .configProvider
    .markupConfigHeader
    .parse(input) match {
      case Success(builderRoot, _) =>
        builderRoot.resolve(Origin.root, Config.empty, Map.empty)
      case f: Failure              => Left(ConfigParserError(f))
    }

  private val expectedConfig = ConfigBuilder.empty.withValue("foo", 7).build

  test("should let configuration providers from app bundles override providers from parsers") {
    val format = createParser(bundles = Seq(BundleProvider.forConfigProvider(BetweenBraces)))
    val config = createConfig(format, BundleProvider.forConfigProvider(BetweenAngles))

    assertEquals(parseWith(config, "{{\nfoo: 7\n}}").toOption, None)
    assertEquals(parseWith(config, "<<\nfoo: 7\n>>"), Right(expectedConfig))
  }

  test("let an app config override a config provider in a previously installed app config") {
    val format = createParser()
    val config = createConfig(
      format,
      BundleProvider.forConfigProvider(BetweenBraces),
      BundleProvider.forConfigProvider(BetweenAngles)
    )

    assertEquals(parseWith(config, "{{\nfoo: 7\n}}").toOption, None)
    assertEquals(parseWith(config, "<<\nfoo: 7\n>>"), Right(expectedConfig))
  }

  test("use the default fallback parser in case no provider is installed") {
    assertEquals(
      parseWith(createConfig(createParser()), "{%\nfoo: 7\n%}"),
      Right(expectedConfig)
    )
  }

  test("fail when it does not recognize the header separator") {
    val input = "[[\nfoo: 7\n]]"

    parseWith(createConfig(createParser()), input) match {
      case Left(ConfigParserError(f)) =>
        assertEquals(
          f.toString,
          """[1.1] failure: `{%' expected but `[[` found
            |
            |[[
            |^""".stripMargin
        )
      case other                      => fail(s"Unexpected result: $other")
    }
  }

}

class TemplateParserConfigSpec extends FunSuite with BundleSetup {

  val missingParserMsg = "no template parser specified"

  test("let an app config override a parser in the extension config") {
    val format         = createParser(bundles =
      Seq(BundleProvider.forTemplateParser(Parsers.success(TemplateRoot("foo"))))
    )
    val config         =
      createConfig(format, BundleProvider.forTemplateParser(Parsers.success(TemplateRoot("bar"))))
    val templateParser = config.templateParser.toRight(missingParserMsg)
    assertEquals(
      templateParser.flatMap(_.parse("anything").toEither),
      Right(TemplateRoot("bar"))
    )
  }

  test("let an app config override a parser in a previously installed app config") {
    val format         = createParser()
    val config         = createConfig(
      format,
      BundleProvider.forTemplateParser(Parsers.success(TemplateRoot("foo"))),
      BundleProvider.forTemplateParser(Parsers.success(TemplateRoot("bar")))
    )
    val templateParser = config.templateParser.toRight(missingParserMsg)
    assertEquals(
      templateParser.flatMap(_.parse("anything").toEither),
      Right(TemplateRoot("bar"))
    )
  }

  test("use the default parser when there is no parser installed") {
    val input          = "${cursor.currentDocument.content}"
    val contextRef     =
      TemplateContextReference(CursorKeys.documentContent, required = true, generatedSource(input))
    val templateParser = defaultConfig.templateParser.toRight(missingParserMsg)
    assertEquals(
      templateParser.flatMap(_.parse(input).toEither),
      Right(TemplateRoot(contextRef))
    )
  }

  test("retain the template parser in strict mode") {
    val config = createConfig(createParser())
    assert(config.forStrictMode.templateParser.nonEmpty)
  }

}

class StyleSheetParserConfigSpec extends FunSuite with BundleSetup {

  def style(value: String): Set[StyleDeclaration] = Set(
    StyleDeclaration(StylePredicate.Id("id"), "foo" -> value)
  )

  test("let an app config override a parser in the extension config") {
    val format =
      createParser(bundles = Seq(BundleProvider.forStyleSheetParser(Parsers.success(style("red")))))
    val config =
      createConfig(format, BundleProvider.forStyleSheetParser(Parsers.success(style("blue"))))
    assertEquals(config.styleSheetParser.parse("anything").toEither, Right(style("blue")))
  }

  test("let an app config override a parser in a previously installed app config") {
    val format = createParser()
    val config = createConfig(
      format,
      BundleProvider.forStyleSheetParser(Parsers.success(style("red"))),
      BundleProvider.forStyleSheetParser(Parsers.success(style("blue")))
    )
    assertEquals(config.styleSheetParser.parse("anything").toEither, Right(style("blue")))
  }

  test("use the default fallback parser in case all other parsers fail") {
    assertEquals(defaultConfig.styleSheetParser, CSSParsers.styleDeclarationSet)
    assertEquals(
      defaultConfig.styleSheetParser.parse("#id { foo: blue; }").toEither,
      Right(style("blue"))
    )
  }

}
