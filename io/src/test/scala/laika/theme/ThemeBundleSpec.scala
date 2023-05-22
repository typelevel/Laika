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

package laika.theme

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import laika.api.Transformer
import laika.api.builder.OperationConfig
import laika.ast.DocumentType.{ Markup, Static, Template }
import laika.ast.Path.Root
import laika.ast._
import laika.io.implicits._
import laika.bundle.{ BundleOrigin, BundleProvider, ExtensionBundle }
import laika.format.{ HTML, Markdown }
import laika.io.helper.TestThemeBuilder
import laika.rewrite.OutputContext
import laika.rewrite.nav.NoOpPathTranslator
import munit.FunSuite

/** @author Jens Halm
  */
class ThemeBundleSpec extends FunSuite {

  def config(
      themeBundles: Seq[ExtensionBundle],
      appBundles: Seq[ExtensionBundle],
      themeExtensionBundles: Seq[ExtensionBundle] = Nil
  ): OperationConfig = {
    val baseTheme = TestThemeBuilder.forBundles(themeBundles)
    val theme     =
      if (themeExtensionBundles.isEmpty) baseTheme
      else baseTheme.extendWith(TestThemeBuilder.forBundles(themeExtensionBundles))
    Transformer
      .from(Markdown)
      .to(HTML)
      .using(appBundles: _*)
      .parallel[IO]
      .withTheme(theme)
      .build
      .use(t => IO.pure(t.config))
      .unsafeRunSync()
  }

  test("baseConfig - merged from theme and app extension") {
    val themeBundles = Seq(BundleProvider.forConfigString("foo: 1", BundleOrigin.Theme))
    val appBundles   = Seq(BundleProvider.forConfigString("bar: 2"))

    val baseConfig = config(themeBundles, appBundles).baseConfig
    assertEquals(baseConfig.get[Int]("foo"), Right(1))
    assertEquals(baseConfig.get[Int]("bar"), Right(2))
  }

  test("baseConfig - app config overrides an identical key in the theme config") {
    val themeBundles = Seq(BundleProvider.forConfigString("foo: 1", BundleOrigin.Theme))
    val appBundles   = Seq(BundleProvider.forConfigString("foo: 2"))
    val baseConfig   = config(themeBundles, appBundles).baseConfig
    assertEquals(baseConfig.get[Int]("foo"), Right(2))
  }

  test(
    "baseConfig - theme extension config overrides an identical key in the base theme's config"
  ) {
    val themeBundles    = Seq(BundleProvider.forConfigString("foo: 1", BundleOrigin.Theme))
    val themeExtBundles = Seq(BundleProvider.forConfigString("foo: 2", BundleOrigin.Theme))
    val appBundles      = Nil
    val baseConfig      = config(themeBundles, appBundles, themeExtBundles).baseConfig
    assertEquals(baseConfig.get[Int]("foo"), Right(2))
  }

  test("docTypeMatcher - merged from a markup extension and an app extension") {
    val themeBundles = Seq(BundleProvider.forDocTypeMatcher(BundleOrigin.Theme) {
      case Root / "foo" => Markup
    })
    val appBundles = Seq(BundleProvider.forDocTypeMatcher(BundleOrigin.User) { case Root / "bar" =>
      Markup
    })

    val docTypeMatcher = config(themeBundles, appBundles).docTypeMatcher
    assertEquals(docTypeMatcher(Root / "foo"), Markup)
    assertEquals(docTypeMatcher(Root / "bar"), Markup)
    assertEquals(docTypeMatcher(Root / "baz"), Static())
  }

  test("docTypeMatcher - app config overrides an identical path key in the extension config") {
    val themeBundles = Seq(BundleProvider.forDocTypeMatcher(BundleOrigin.Theme) {
      case Root / "foo" => Markup
    })
    val appBundles = Seq(BundleProvider.forDocTypeMatcher(BundleOrigin.User) { case Root / "foo" =>
      Template
    })

    val docTypeMatcher = config(themeBundles, appBundles).docTypeMatcher
    assertEquals(docTypeMatcher(Root / "foo"), Template)
    assertEquals(docTypeMatcher(Root / "bar"), Static())
  }

  test(
    "path translator - translator from app config is applied after translator from theme config"
  ) {
    val themeBundles       = Seq(BundleProvider.forPathTranslator(BundleOrigin.Theme) { p =>
      p.parent / p.withBasename(p.basename + "-theme").relative
    })
    val appBundles         = Seq(BundleProvider.forPathTranslator() { p =>
      p.parent / p.withBasename(p.basename + "-app").relative
    })
    val testTree           =
      DocumentTreeRoot(DocumentTree(Root, Seq(Document(Root / "doc.md", RootElement.empty))))
    val compoundTranslator = config(themeBundles, appBundles)
      .pathTranslatorFor(testTree, OutputContext("html"))
      .getOrElse(NoOpPathTranslator)
    assertEquals(compoundTranslator.translate(Root / "doc.md"), Root / "doc-theme-app.html")
  }

  test("rewrite rules - merged from a markup extension and an app extension") {
    val themeBundles = Seq(BundleProvider.forSpanRewriteRule(BundleOrigin.Theme) { case s: Strong =>
      Replace(Literal(s.extractText))
    })
    val appBundles   = Seq(BundleProvider.forSpanRewriteRule(BundleOrigin.User) {
      case s: Emphasized => Replace(Literal(s.extractText))
    })

    val doc = Document(Root, RootElement(Strong("foo"), Emphasized("bar")))

    val expected = Document(Root, RootElement(Literal("foo"), Literal("bar")))

    assertEquals(
      config(themeBundles, appBundles).rewriteRulesFor(doc, RewritePhase.Resolve).flatMap(
        doc.rewrite
      ),
      Right(expected)
    )
  }

  test(
    "rewrite rules - apply a rule from an app config and a rule from a markup extension successively"
  ) {
    val themeBundles = Seq(BundleProvider.forSpanRewriteRule(BundleOrigin.Theme) {
      case Literal(text, _) => Replace(Literal(text + "!"))
    })
    val appBundles   = Seq(BundleProvider.forSpanRewriteRule(BundleOrigin.User) {
      case Literal(text, _) => Replace(Literal(text + "?"))
    })

    val doc      = Document(Root, RootElement(Literal("foo")))
    val expected = Document(Root, RootElement(Literal("foo!?")))

    assertEquals(
      config(themeBundles, appBundles).rewriteRulesFor(doc, RewritePhase.Resolve).flatMap(
        doc.rewrite
      ),
      Right(expected)
    )
  }

}
