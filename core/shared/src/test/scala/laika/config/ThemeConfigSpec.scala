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
import laika.bundle.{BundleProvider, ExtensionBundle}
import laika.factory.{RenderContext, RenderFormat}
import laika.render.{ASTRenderer, Indentation, TextFormatter}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class ThemeConfigSpec extends AnyWordSpec with Matchers {


  trait BundleSetup {

    def appBundles: Seq[ExtensionBundle]

    def config: OperationConfig = {
      val base = OperationConfig.default
      appBundles.foldLeft(base){ (acc, bundle) => acc.withBundles(Seq(bundle)) }
    }

    object TestFormat extends RenderFormat[TextFormatter] {

      override val fileSuffix = "test"

      val defaultRenderer: (TextFormatter, Element) => String = ASTRenderer

      val formatterFactory: RenderContext[TextFormatter] => TextFormatter = TextFormatter
      
    }

  }


  "The configuration for custom renderers" should {

    trait RenderSetup extends BundleSetup {
      val sb = new StringBuilder
      lazy val testRenderer = config.themeFor(TestFormat).customRenderer
      val formatter = TextFormatter((_,_) => "", RootElement.empty, Nil, Indentation.default)

      def result: String = sb.toString
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

      testRenderer((formatter, Strong.empty)) shouldBe "override"
    }

  }

  "The configuration for the default template" ignore { // TODO - 0.16 - re-activate based on new Theme API

    "let an app config override the default template in the default theme" in new BundleSetup {
      //override lazy val defaultTemplate = Some(TemplateRoot("foo"))
      val appBundles = Seq(
        //BundleProvider.forTheme(TestFormat.Theme(defaultTemplate = Some(TemplateRoot("bar"))))
      )

//      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
//      template shouldBe TemplateRoot("bar")
    }

    "let an app config override the default template in a previously installed app config" in new BundleSetup {
      //override lazy val defaultTemplate = Some(TemplateRoot("foo"))
      val appBundles = Seq(
//        BundleProvider.forTheme(TestFormat.Theme(defaultTemplate = Some(TemplateRoot("foo")))),
//        BundleProvider.forTheme(TestFormat.Theme(defaultTemplate = Some(TemplateRoot("bar"))))
      )

//      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
//      template shouldBe TemplateRoot("bar")
    }

    "use the default template from the default theme when no other template is installed" in new BundleSetup {
      //override lazy val defaultTemplate = Some(TemplateRoot("foo"))
      val appBundles = Nil

//      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
//      template shouldBe TemplateRoot("foo")
    }

    "use the fallback template if no other template is installed" in new BundleSetup {
      val appBundles = Nil

//      val template = config.themeFor(TestFormat).defaultTemplateOrFallback
//      template shouldBe TemplateRoot.fallback
    }

  }

  "The configuration for default styles" ignore { // TODO - 0.16 - re-activate based on new Theme API

    val lowPrecedenceStyles = StyleDeclarationSet(Set.empty[Path], Set(
      StyleDeclaration(StylePredicate.Id("id1"), "foo" -> "red"),
      StyleDeclaration(StylePredicate.Id("id2"), "foo" -> "green")
    ))

    val highPrecedenceStyles = StyleDeclarationSet(Set.empty[Path], Set(
      StyleDeclaration(StylePredicate.Id("id1"), "foo" -> "blue"),
      StyleDeclaration(StylePredicate.Id("id3"), "foo" -> "yellow")
    ))

    "merge styles from the default theme with the styles from an app extension" in new BundleSetup {
      // override lazy val defaultStyles = lowPrecedenceStyles
      val appBundles = Nil //Seq(BundleProvider.forTheme(TestFormat.Theme(defaultStyles = highPrecedenceStyles)))

      val styles = StyleDeclarationSet.empty // config.themeFor(TestFormat).defaultStyles

      styles.collectStyles(Text("", Id("id1")), Nil) shouldBe Map("foo" -> "blue")
      styles.collectStyles(Text("", Id("id2")), Nil) shouldBe Map("foo" -> "green")
      styles.collectStyles(Text("", Id("id3")), Nil) shouldBe Map("foo" -> "yellow")
    }

    "let app config styles from two app bundles" in new BundleSetup {
      val appBundles = Seq(
        //BundleProvider.forTheme(TestFormat.Theme(defaultStyles = lowPrecedenceStyles)),
        //BundleProvider.forTheme(TestFormat.Theme(defaultStyles = highPrecedenceStyles))
      )

      val styles = StyleDeclarationSet.empty // config.themeFor(TestFormat).defaultStyles

      styles.collectStyles(Text("", Id("id1")), Nil) shouldBe Map("foo" -> "blue")
      styles.collectStyles(Text("", Id("id2")), Nil) shouldBe Map("foo" -> "green")
      styles.collectStyles(Text("", Id("id3")), Nil) shouldBe Map("foo" -> "yellow")
    }

    "provide an empty declaration set when no default styles are installed" in new BundleSetup {
      val appBundles = Nil
      // config.themeFor(TestFormat).defaultStyles shouldBe StyleDeclarationSet.empty
    }

  }

}
