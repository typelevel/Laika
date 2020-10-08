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
import laika.api.Transformer
import laika.api.builder.OperationConfig
import laika.ast.DocumentType.{Markup, Static, Template}
import laika.ast.Path.Root
import laika.ast._
import laika.bundle.{BundleOrigin, BundleProvider, ExtensionBundle}
import laika.format.{HTML, Markdown}
import laika.io.helper.TestThemeBuilder
import laika.io.{FileIO, IOWordSpec}
import org.scalatest.matchers.should.Matchers

/**
  * @author Jens Halm
  */
class ThemeBundleSpec extends IOWordSpec with Matchers {


  trait BundleSetup {

    import laika.io.implicits._
    
    def themeBundles: Seq[ExtensionBundle]

    def appBundles: Seq[ExtensionBundle]

    def config: OperationConfig = {
      Transformer
        .from(Markdown)
        .to(HTML)
        .using(appBundles:_*)
        .io(FileIO.blocker)
        .parallel[IO]
        .withTheme(TestThemeBuilder.forBundles(themeBundles))
        .build
        .use(t => IO.pure(t.config))
        .unsafeRunSync()
    }

  }

  "The configuration for baseConfig settings" should {

    "merge baseConfig from a theme with the baseConfig from an app extension" in new BundleSetup {
      val themeBundles = Seq(BundleProvider.forConfigString("foo: 1", BundleOrigin.Theme))
      val appBundles = Seq(BundleProvider.forConfigString("bar: 2"))

      val baseConfig = config.baseConfig
      baseConfig.get[Int]("foo") shouldBe Right(1)
      baseConfig.get[Int]("bar") shouldBe Right(2)
    }

    "let an app config override an identical key in the theme config" in new BundleSetup {
      val themeBundles = Seq(BundleProvider.forConfigString("foo: 1", BundleOrigin.Theme))
      val appBundles = Seq(BundleProvider.forConfigString("foo: 2"))
      val baseConfig = config.baseConfig
      baseConfig.get[Int]("foo") shouldBe Right(2)
    }

  }

  "The configuration for the docTypeMatcher" should {

    "merge docTypeMatcher from a markup extension with the docTypeMatcher from an app extension" in new BundleSetup {
      val themeBundles = Seq(BundleProvider.forDocTypeMatcher(BundleOrigin.Theme) { case Root / "foo" => Markup })
      val appBundles =    Seq(BundleProvider.forDocTypeMatcher { case Root / "bar" => Markup })

      val docTypeMatcher = config.docTypeMatcher
      docTypeMatcher(Root / "foo") shouldBe Markup
      docTypeMatcher(Root / "bar") shouldBe Markup
      docTypeMatcher(Root / "baz") shouldBe Static()
    }

    "let an app config override an identical path key in the extension config" in new BundleSetup {
      val themeBundles = Seq(BundleProvider.forDocTypeMatcher(BundleOrigin.Theme) { case Root / "foo" => Markup })
      val appBundles =    Seq(BundleProvider.forDocTypeMatcher { case Root / "foo" => Template })

      val docTypeMatcher = config.docTypeMatcher
      docTypeMatcher(Root / "foo") shouldBe Template
      docTypeMatcher(Root / "bar") shouldBe Static()
    }

  }

  "The configuration for the rewrite rule" should {

    "merge a rewrite rule from a markup extension with the rewrite rule from an app extension" in new BundleSetup {
      val themeBundles = Seq(BundleProvider.forSpanRewriteRule(BundleOrigin.Theme) { case s: Strong => Replace(Literal(s.extractText)) })
      val appBundles =    Seq(BundleProvider.forSpanRewriteRule { case s: Emphasized => Replace(Literal(s.extractText)) })

      val doc = Document(Root, RootElement(Strong("foo"), Emphasized("bar")))

      val expected = Document(Root, RootElement(Literal("foo"), Literal("bar")))

      val rewriteRule = config.rewriteRulesFor(doc)
      doc.rewrite(rewriteRule) shouldBe expected
    }

    "apply a rewrite rule from an app config and a rule from a markup extension successively" in new BundleSetup {
      val themeBundles = Seq(BundleProvider.forSpanRewriteRule(BundleOrigin.Theme) { case Literal(text, _) => Replace(Literal(text+"!")) })
      val appBundles =    Seq(BundleProvider.forSpanRewriteRule { case Literal(text, _) => Replace(Literal(text+"?")) })

      val doc =      Document(Root, RootElement(Literal("foo")))
      val expected = Document(Root, RootElement(Literal("foo!?")))

      val rewriteRule = config.rewriteRulesFor(doc)
      doc.rewrite(rewriteRule) shouldBe expected
    }

  }

}
