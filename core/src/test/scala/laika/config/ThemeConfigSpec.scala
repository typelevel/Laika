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

package laika.config

import laika.ast.Path.Root
import laika.ast._
import laika.bundle.{BundleProvider, ExtensionBundle, StaticDocuments}
import laika.execute.OutputExecutor
import laika.factory.{RenderContext, RenderFormat}
import laika.io.{ByteInput, StringOutput}
import laika.render.{ASTRenderer, Indentation, TextFormatter}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class ThemeConfigSpec extends WordSpec with Matchers {


  trait BundleSetup {

    def appBundles: Seq[ExtensionBundle]

    def config: OperationConfig = {
      val base = OperationConfig.default
      appBundles.foldLeft(base){ (acc, bundle) => acc.withBundles(Seq(bundle)) }
    }

    def template (text: String): TemplateRoot = TemplateRoot(Seq(TemplateString(text)))


    object TestFormat extends RenderFormat[TextFormatter] {

      override val fileSuffix = "test"

      val defaultRenderer: (TextFormatter, Element) => String = ASTRenderer

      val formatterFactory: RenderContext[TextFormatter] => TextFormatter = TextFormatter
      
      override val defaultTheme = Theme(
        customRenderer = customRenderer,
        defaultTemplate = defaultTemplate,
        defaultStyles = defaultStyles,
        staticDocuments = staticDocuments
      )

    }

    def defaultTemplate: Option[TemplateRoot] = None

    def defaultStyles: StyleDeclarationSet = StyleDeclarationSet.empty

    def staticDocuments: StaticDocuments = StaticDocuments(DocumentTree(Path.Root, Nil))

    def customRenderer: PartialFunction[(TextFormatter, Element), String] =  { case (_, _) => "" }

  }


  "The configuration for custom renderers" should {

    trait RenderSetup extends BundleSetup {
      val sb = new StringBuilder
      val out = OutputExecutor.asRenderFunction(StringOutput(sb, Root))
      lazy val testRenderer = config.themeFor(TestFormat).customRenderer
      val formatter = TextFormatter((_,_) => "", List(RootElement(Nil)), Indentation.default)

      def result: String = sb.toString
    }

    "merge renderers from a default theme with the renderers from an app extension" in new RenderSetup {
      override val customRenderer: PartialFunction[(TextFormatter, Element), String] = { case (_, Strong(_, _)) => "strong" }
      val appBundles = Seq(BundleProvider.forTheme(TestFormat.Theme(
        customRenderer = { case (_, Emphasized(_, _)) => "em" }
      )))

      testRenderer((formatter, Strong(Nil))) + testRenderer((formatter, Emphasized(Nil))) shouldBe "strongem"
    }

    "let an app config override the renderer for an identical element in the default theme" in new RenderSetup {
      override val customRenderer: PartialFunction[(TextFormatter, Element), String] = { case (_, Strong(_, _)) => "strong" }
      val appBundles = Seq(BundleProvider.forTheme(TestFormat.Theme(
        customRenderer = { case (_, Strong(_, _)) => "override" }
      )))

      testRenderer((formatter, Strong(Nil))) shouldBe "override"
    }

    "let an app config override the renderer for an identical element in a previously installed app config" in new RenderSetup {
      val appBundles = Seq(
        BundleProvider.forTheme(TestFormat.Theme(
          customRenderer = { case (_, Strong(_, _)) => "strong" }
        )),
        BundleProvider.forTheme(TestFormat.Theme(
          customRenderer = { case (_, Strong(_, _)) => "override" }
        ))
      )

      testRenderer((formatter, Strong(Nil))) shouldBe "override"
    }

  }

  "The configuration for the default template" should {

    "let an app config override the default template in the default theme" in new BundleSetup {
      override lazy val defaultTemplate = Some(template("foo"))
      val appBundles = Seq(BundleProvider.forTheme(TestFormat.Theme(defaultTemplate = Some(template("bar")))))

      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
      template shouldBe template("bar")
    }

    "let an app config override the default template in a previously installed app config" in new BundleSetup {
      override lazy val defaultTemplate = Some(template("foo"))
      val appBundles = Seq(
        BundleProvider.forTheme(TestFormat.Theme(defaultTemplate = Some(template("foo")))),
        BundleProvider.forTheme(TestFormat.Theme(defaultTemplate = Some(template("bar"))))
      )

      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
      template shouldBe template("bar")
    }

    "use the default template from the default theme when no other template is installed" in new BundleSetup {
      override lazy val defaultTemplate = Some(template("foo"))
      val appBundles = Nil

      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
      template shouldBe template("foo")
    }

    "use the fallback template if no other template is installed" in new BundleSetup {
      val appBundles = Nil

      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
      template shouldBe TemplateRoot.fallback
    }

  }

  "The configuration for default styles" should {

    val lowPrecedenceStyles = StyleDeclarationSet(Set.empty[Path], Set(
      StyleDeclaration(StylePredicate.Id("id1"), "foo" -> "red"),
      StyleDeclaration(StylePredicate.Id("id2"), "foo" -> "green")
    ))

    val highPrecedenceStyles = StyleDeclarationSet(Set.empty[Path], Set(
      StyleDeclaration(StylePredicate.Id("id1"), "foo" -> "blue"),
      StyleDeclaration(StylePredicate.Id("id3"), "foo" -> "yellow")
    ))

    "merge styles from the default theme with the styles from an app extension" in new BundleSetup {
      override lazy val defaultStyles = lowPrecedenceStyles
      val appBundles = Seq(BundleProvider.forTheme(TestFormat.Theme(defaultStyles = highPrecedenceStyles)))

      val styles = config.themeFor(TestFormat).defaultStyles

      styles.collectStyles(Text("", Id("id1")), Nil) shouldBe Map("foo" -> "blue")
      styles.collectStyles(Text("", Id("id2")), Nil) shouldBe Map("foo" -> "green")
      styles.collectStyles(Text("", Id("id3")), Nil) shouldBe Map("foo" -> "yellow")
    }

    "let app config styles from two app bundles" in new BundleSetup {
      val appBundles = Seq(
        BundleProvider.forTheme(TestFormat.Theme(defaultStyles = lowPrecedenceStyles)),
        BundleProvider.forTheme(TestFormat.Theme(defaultStyles = highPrecedenceStyles))
      )

      val styles = config.themeFor(TestFormat).defaultStyles

      styles.collectStyles(Text("", Id("id1")), Nil) shouldBe Map("foo" -> "blue")
      styles.collectStyles(Text("", Id("id2")), Nil) shouldBe Map("foo" -> "green")
      styles.collectStyles(Text("", Id("id3")), Nil) shouldBe Map("foo" -> "yellow")
    }

    "provide an empty declaration set when no default styles are installed" in new BundleSetup {
      val appBundles = Nil
      config.themeFor(TestFormat).defaultStyles shouldBe StyleDeclarationSet.empty
    }

  }

  "The configuration for static content" should {

    "merge static content defined in a default theme with static content defined in an app extension" in new BundleSetup {
      val docA = StaticDocument(ByteInput("a", Path.Root / "a"))
      val docB = StaticDocument(ByteInput("b", Path.Root / "b"))
      override lazy val staticDocuments = StaticDocuments(DocumentTree(Path.Root, Nil, additionalContent = Seq(docA)))
      val moreDocuments = StaticDocuments(DocumentTree(Path.Root, Nil, additionalContent = Seq(docB)))
      val appBundles = Seq(BundleProvider.forTheme(TestFormat.Theme(staticDocuments = moreDocuments)))

      val mergedStaticDocuments = config.themeFor(TestFormat).staticDocuments

      mergedStaticDocuments shouldBe StaticDocuments(DocumentTree(Path.Root, Nil, additionalContent = Seq(docB, docA)))
    }

  }


}
