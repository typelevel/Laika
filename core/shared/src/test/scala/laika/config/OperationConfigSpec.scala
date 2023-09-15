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

import laika.api.builder.OperationConfig
import laika.api.format.MarkupFormat
import laika.ast.*
import laika.ast.DocumentType.{ Markup, Static, Template }
import laika.ast.Path.Root
import laika.bundle.BundleProvider.TestExtensionBundle
import laika.api.bundle.ExtensionBundle.LaikaDefaults
import laika.bundle.BundleProvider
import MarkupFormat.MarkupParsers
import laika.api.bundle.{ BlockParserBuilder, BundleOrigin, ExtensionBundle, SpanParserBuilder }
import laika.markdown.bundle.VerbatimHTML
import laika.markdown.github.GitHubFlavor
import munit.FunSuite

/** @author Jens Halm
  */
class OperationConfigSpec extends FunSuite {

  def createConfig(
      parserBundles: Seq[ExtensionBundle],
      appBundles: Seq[ExtensionBundle]
  ): OperationConfig = {

    object Parser extends MarkupFormat {
      val fileSuffixes    = Set("foo")
      val blockParsers    = new MarkupParsers[BlockParserBuilder] {
        val all: Seq[BlockParserBuilder] = Nil
      }
      val spanParsers     = new MarkupParsers[SpanParserBuilder] {
        val all: Seq[SpanParserBuilder] = Nil
      }
      lazy val extensions = parserBundles
    }

    val base = OperationConfig.default.withBundlesFor(Parser)
    appBundles.foldLeft(base) { (acc, bundle) => acc.withBundles(Seq(bundle)) }

  }

  test("extension bundles - merge bundles based on their origin and configuration order") {
    object UserBundle1 extends TestExtensionBundle
    object UserBundle2 extends TestExtensionBundle
    object ThemeBundle extends TestExtensionBundle {
      override val origin: BundleOrigin = BundleOrigin.Theme
    }

    val bundles1 = Seq(UserBundle1, LaikaDefaults, ThemeBundle, GitHubFlavor, UserBundle2)
    val bundles2 = Seq(VerbatimHTML, UserBundle1, ExtensionBundle.Empty)
    val config1  = new OperationConfig(bundles1)
    val config2  = new OperationConfig(bundles2)
    assertEquals(
      config1.merge(config2).bundles,
      Seq(
        LaikaDefaults,
        ExtensionBundle.Empty,
        GitHubFlavor,
        VerbatimHTML,
        ThemeBundle,
        UserBundle1,
        UserBundle2
      )
    )
  }

  test("baseConfig - merge config from a markup extension with config from an app extension") {
    val config = createConfig(
      parserBundles = Seq(BundleProvider.forConfigString("foo: 1")),
      appBundles = Seq(BundleProvider.forConfigString("bar: 2"))
    )

    val baseConfig = config.baseConfig
    assertEquals(baseConfig.get[Int]("foo"), Right(1))
    assertEquals(baseConfig.get[Int]("bar"), Right(2))
  }

  test("baseConfig - let an app config override an identical key in the extension config") {
    val config = createConfig(
      parserBundles = Seq(BundleProvider.forConfigString("foo: 1")),
      appBundles = Seq(BundleProvider.forConfigString("foo: 2"))
    )

    val baseConfig = config.baseConfig
    assertEquals(baseConfig.get[Int]("foo"), Right(2))
  }

  test(
    "baseConfig - let an app config override an identical key in a previously installed app config"
  ) {
    val config = createConfig(
      parserBundles = Nil,
      appBundles =
        Seq(BundleProvider.forConfigString("foo: 1"), BundleProvider.forConfigString("foo: 2"))
    )

    val baseConfig = config.baseConfig
    assertEquals(baseConfig.get[Int]("foo"), Right(2))
  }

  test(
    "docTypeMatcher - merge matcher from a markup extension with the matcher from an app extension"
  ) {
    val config = createConfig(
      parserBundles = Seq(BundleProvider.forDocTypeMatcher { case Root / "foo" => Markup }),
      appBundles = Seq(BundleProvider.forDocTypeMatcher { case Root / "bar" => Markup })
    )

    val docTypeMatcher = config.docTypeMatcher
    assertEquals(docTypeMatcher(Root / "foo"), Markup)
    assertEquals(docTypeMatcher(Root / "bar"), Markup)
    assertEquals(docTypeMatcher(Root / "baz"), Static())
  }

  test(
    "docTypeMatcher - let an app config override an identical path key in the extension config"
  ) {
    val config = createConfig(
      parserBundles = Seq(BundleProvider.forDocTypeMatcher { case Root / "foo" => Markup }),
      appBundles = Seq(BundleProvider.forDocTypeMatcher { case Root / "foo" => Template })
    )

    val docTypeMatcher = config.docTypeMatcher
    assertEquals(docTypeMatcher(Root / "foo"), Template)
    assertEquals(docTypeMatcher(Root / "bar"), Static())
  }

  test(
    "docTypeMatcher - let an app config override an identical path key in a previously installed app config"
  ) {
    val config = createConfig(
      parserBundles = Nil,
      appBundles = Seq(
        BundleProvider.forDocTypeMatcher { case Root / "foo" => Markup },
        BundleProvider.forDocTypeMatcher { case Root / "foo" => Template }
      )
    )

    val docTypeMatcher = config.docTypeMatcher
    assertEquals(docTypeMatcher(Root / "foo"), Template)
    assertEquals(docTypeMatcher(Root / "bar"), Static())
  }

  test("rewrite rules - merge a rule from a markup extension with the rule from an app extension") {
    val config = createConfig(
      parserBundles = Seq(BundleProvider.forSpanRewriteRule { case s: Strong =>
        Replace(Literal(s.extractText))
      }),
      appBundles = Seq(BundleProvider.forSpanRewriteRule { case s: Emphasized =>
        Replace(Literal(s.extractText))
      })
    )

    val doc = Document(Root, RootElement(Strong("foo"), Emphasized("bar")))

    val expected = Document(Root, RootElement(Literal("foo"), Literal("bar")))

    assertEquals(
      config.rewriteRulesFor(doc, RewritePhase.Resolve).flatMap(doc.rewrite).map(_.content),
      Right(expected.content)
    )
  }

  test(
    "rewrite rules - apply a rule from an app config and a rule from a markup extension successively"
  ) {
    val config = createConfig(
      parserBundles = Seq(BundleProvider.forSpanRewriteRule { case Literal(text, _) =>
        Replace(Literal(text + "!"))
      }),
      appBundles = Seq(BundleProvider.forSpanRewriteRule { case Literal(text, _) =>
        Replace(Literal(text + "?"))
      })
    )

    val doc      = Document(Root, RootElement(Literal("foo")))
    val expected = Document(Root, RootElement(Literal("foo!?")))

    assertEquals(
      config.rewriteRulesFor(doc, RewritePhase.Resolve).flatMap(doc.rewrite).map(_.content),
      Right(expected.content)
    )
  }

  test(
    "rewrite rules - apply a rule from an app config and a rule from a previously installed app config successively"
  ) {
    val config = createConfig(
      parserBundles = Nil,
      appBundles = Seq(
        BundleProvider.forSpanRewriteRule { case Literal(text, _) => Replace(Literal(text + "!")) },
        BundleProvider.forSpanRewriteRule { case Literal(text, _) => Replace(Literal(text + "?")) }
      )
    )

    val doc      = Document(Root, RootElement(Literal("foo")))
    val expected = Document(Root, RootElement(Literal("foo!?")))

    assertEquals(
      config.rewriteRulesFor(doc, RewritePhase.Resolve).flatMap(doc.rewrite).map(_.content),
      Right(expected.content)
    )
  }

  case object Defaults extends TestExtensionBundle

  case object NonStrict extends TestExtensionBundle {
    override def forStrictMode: Option[ExtensionBundle] = None
  }

  case object RawContent extends TestExtensionBundle {
    override def rawContentDisabled: Option[ExtensionBundle] = None
  }

  case object Both extends TestExtensionBundle {
    override def forStrictMode: Option[ExtensionBundle]      = None
    override def rawContentDisabled: Option[ExtensionBundle] = None
  }

  val flagSetup = {
    val appBundles = Seq(Defaults, NonStrict, RawContent, Both)
    OperationConfig.empty.withBundles(appBundles)
  }

  test("strict/raw flags - remove all raw content bundles in the default settings") {
    assertEquals(flagSetup.filteredBundles, Seq(Defaults, NonStrict))
  }

  test("strict/raw flags - keep all bundles if rawContent is set to true") {
    val finalConfig = flagSetup.forRawContent
    assertEquals(finalConfig.filteredBundles, Seq(Defaults, NonStrict, RawContent, Both))
  }

  test(
    "strict/raw flags - remove all non-strict and raw content bundles if strict is set to true"
  ) {
    val finalConfig = flagSetup.forStrictMode
    assertEquals(finalConfig.filteredBundles, Seq(Defaults))
  }

  test("strict/raw flags - remove all non-strict bundles if both flags are set to true") {
    val finalConfig = flagSetup.forStrictMode.forRawContent
    assertEquals(finalConfig.filteredBundles, Seq(Defaults, RawContent))
  }

}
