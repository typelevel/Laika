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
import laika.ast._
import laika.ast.DocumentType.{Markup, Static, Template}
import laika.ast.Path.Root
import laika.bundle.BundleProvider.TestExtensionBundle
import laika.bundle.ExtensionBundle.LaikaDefaults
import laika.bundle.{BundleProvider, ExtensionBundle}
import laika.factory.MarkupFormat
import laika.markdown.bundle.VerbatimHTML
import laika.markdown.github.GitHubFlavor
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class OperationConfigSpec extends WordSpec with Matchers {


  trait BundleSetup {

    def parserBundles: Seq[ExtensionBundle]

    def appBundles: Seq[ExtensionBundle]

    object Parser extends MarkupFormat {
      val fileSuffixes = Set("foo")
      val blockParsers = Nil
      val spanParsers = Nil
      lazy val extensions = parserBundles
    }

    def config: OperationConfig = {
      val base = OperationConfig.default.withBundlesFor(Parser)
      appBundles.foldLeft(base){ (acc, bundle) => acc.withBundles(Seq(bundle)) }
    }

  }

  "The configuration for extension bundles" should {
    
    object UserBundle1 extends TestExtensionBundle 
    object UserBundle2 extends TestExtensionBundle 
    
    "merge bundles based on their origin and configuration order" in {
      val bundles1 = Seq(UserBundle1, LaikaDefaults, GitHubFlavor, UserBundle2)
      val bundles2 = Seq(VerbatimHTML, UserBundle1, ExtensionBundle.Empty)
      val config1 = OperationConfig(bundles1)
      val config2 = OperationConfig(bundles2)
      config1.merge(config2).bundles shouldBe Seq(
        LaikaDefaults,
        ExtensionBundle.Empty,
        GitHubFlavor,
        VerbatimHTML,
        UserBundle1,
        UserBundle2
      )
    }
    
  }

  "The configuration for baseConfig settings" should {

    "merge baseConfig from a markup extension with the baseConfig from an app extension" in new BundleSetup {
      val parserBundles = Seq(BundleProvider.forConfigString("foo: 1"))
      val appBundles = Seq(BundleProvider.forConfigString("bar: 2"))

      val baseConfig = config.baseConfig
      baseConfig.get[Int]("foo") shouldBe Right(1)
      baseConfig.get[Int]("bar") shouldBe Right(2)
    }

    "let an app config override an identical key in the extension config" in new BundleSetup {
      val parserBundles = Seq(BundleProvider.forConfigString("foo: 1"))
      val appBundles = Seq(BundleProvider.forConfigString("foo: 2"))

      val baseConfig = config.baseConfig
      baseConfig.get[Int]("foo") shouldBe Right(2)
    }

    "let an app config override an identical key in a previously installed app config" in new BundleSetup {
      val parserBundles = Nil
      val appBundles = Seq(BundleProvider.forConfigString("foo: 1"), BundleProvider.forConfigString("foo: 2"))

      val baseConfig = config.baseConfig
      baseConfig.get[Int]("foo") shouldBe Right(2)
    }

  }

  "The configuration for the docTypeMatcher" should {

    "merge docTypeMatcher from a markup extension with the docTypeMatcher from an app extension" in new BundleSetup {
      val parserBundles = Seq(BundleProvider.forDocTypeMatcher { case Root / "foo" => Markup })
      val appBundles =    Seq(BundleProvider.forDocTypeMatcher { case Root / "bar" => Markup })

      val docTypeMatcher = config.docTypeMatcher
      docTypeMatcher(Root / "foo") shouldBe Markup
      docTypeMatcher(Root / "bar") shouldBe Markup
      docTypeMatcher(Root / "baz") shouldBe Static
    }

    "let an app config override an identical path key in the extension config" in new BundleSetup {
      val parserBundles = Seq(BundleProvider.forDocTypeMatcher { case Root / "foo" => Markup })
      val appBundles =    Seq(BundleProvider.forDocTypeMatcher { case Root / "foo" => Template })

      val docTypeMatcher = config.docTypeMatcher
      docTypeMatcher(Root / "foo") shouldBe Template
      docTypeMatcher(Root / "bar") shouldBe Static
    }

    "let an app config override an identical path key in a previously installed app config" in new BundleSetup {
      val parserBundles = Nil
      val appBundles =    Seq(
        BundleProvider.forDocTypeMatcher { case Root / "foo" => Markup },
        BundleProvider.forDocTypeMatcher { case Root / "foo" => Template })

      val docTypeMatcher = config.docTypeMatcher
      docTypeMatcher(Root / "foo") shouldBe Template
      docTypeMatcher(Root / "bar") shouldBe Static
    }

  }

  "The configuration for the rewrite rule" should {

    "merge a rewrite rule from a markup extension with the rewrite rule from an app extension" in new BundleSetup {
      val parserBundles = Seq(BundleProvider.forSpanRewriteRule { case s: Strong => Replace(Literal(s.extractText)) })
      val appBundles =    Seq(BundleProvider.forSpanRewriteRule { case s: Emphasized => Replace(Literal(s.extractText)) })

      val doc = Document(Root, RootElement(Seq(Paragraph(
        Strong("foo"),
        Emphasized("bar")
      ))))

      val expected = Document(Root, RootElement(Seq(Paragraph(
        Literal("foo"),
        Literal("bar")
      ))))

      val rewriteRule = config.rewriteRulesFor(doc)
      doc.rewrite(rewriteRule) shouldBe expected
    }

    "apply a rewrite rule from an app config and a rule from a markup extension successively" in new BundleSetup {
      val parserBundles = Seq(BundleProvider.forSpanRewriteRule { case Literal(text, _) => Replace(Literal(text+"!")) })
      val appBundles =    Seq(BundleProvider.forSpanRewriteRule { case Literal(text, _) => Replace(Literal(text+"?")) })

      val doc =      Document(Root, RootElement(Seq(Paragraph(Literal("foo")))))
      val expected = Document(Root, RootElement(Seq(Paragraph(Literal("foo!?")))))

      val rewriteRule = config.rewriteRulesFor(doc)
      doc.rewrite(rewriteRule) shouldBe expected
    }

    "apply a rewrite rule from an app config and a rule from a previously installed app config successively" in new BundleSetup {
      val parserBundles = Nil
      val appBundles =    Seq(
        BundleProvider.forSpanRewriteRule { case Literal(text, _) => Replace(Literal(text+"!")) },
        BundleProvider.forSpanRewriteRule { case Literal(text, _) => Replace(Literal(text+"?")) }
      )

      val doc =      Document(Root, RootElement(Seq(Paragraph(Literal("foo")))))
      val expected = Document(Root, RootElement(Seq(Paragraph(Literal("foo!?")))))

      val rewriteRule = config.rewriteRulesFor(doc)
      doc.rewrite(rewriteRule) shouldBe expected
    }

  }

  "The configuration flags for strict mode and raw content" should {

    case object Defaults extends TestExtensionBundle

    case object Strict extends TestExtensionBundle { override val useInStrictMode = true }

    case object RawContent extends TestExtensionBundle { override val acceptRawContent = true }

    case object Both extends TestExtensionBundle {
      override val useInStrictMode = true
      override val acceptRawContent = true
    }

    trait FlagSetup {
      val appBundles = Seq(Defaults, Strict, RawContent, Both)
      def config: OperationConfig = OperationConfig.empty.withBundles(appBundles)
    }

    "remove all raw content bundles in the default settings" in new FlagSetup {
      config.bundles.filter(config.bundleFilter) shouldBe Seq(Defaults, Strict)
    }

    "keep all bundles if rawContent is set to true" in new FlagSetup {
      val finalConfig = config.forRawContent
      finalConfig.bundles.filter(finalConfig.bundleFilter) shouldBe Seq(Defaults, Strict, RawContent, Both)
    }

    "remove all non-strict and raw content bundles if strict is set to true" in new FlagSetup {
      val finalConfig = config.forStrictMode
      finalConfig.bundles.filter(finalConfig.bundleFilter) shouldBe Seq(Strict)
    }

    "remove all non-strict bundles if both flags are set to true" in new FlagSetup {
      val finalConfig = config.forStrictMode.forRawContent
      finalConfig.bundles.filter(finalConfig.bundleFilter) shouldBe Seq(Strict,  Both)
    }

  }

}
