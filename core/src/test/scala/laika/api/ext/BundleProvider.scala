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

package laika.api.ext
import com.typesafe.config.Config
import laika.directive.DirectiveRegistry
import laika.directive.Directives.Templates
import laika.factory.RendererFactory
import laika.io.{DocumentType, Input}
import laika.parse.core.Parser
import laika.parse.css.Styles.StyleDeclaration
import laika.render.{FOWriter, HTML, HTMLWriter, XSLFO}
import laika.tree.Paths.Path
import laika.tree.Templates.TemplateRoot

/**
  * @author Jens Halm
  */
object BundleProvider {

  def forConfigString (input: String): ExtensionBundle = new ExtensionBundle {

    override def baseConfig: Config = ConfigProvider.fromInput(Input.fromString(input))

  }

  def forDocTypeMatcher (matcher: PartialFunction[Path, DocumentType]): ExtensionBundle = new ExtensionBundle {

    override def docTypeMatcher: PartialFunction[Path, DocumentType] = matcher

  }

  def forTemplateParser(parser: Parser[TemplateRoot]): ExtensionBundle = new ExtensionBundle {

    override def parserDefinitions: ParserDefinitionBuilders = ParserDefinitionBuilders(
      templateParser = Some(parser)
    )

  }

  def forTemplateDirective(directive: Templates.Directive): ExtensionBundle = new DirectiveRegistry {

    val templateDirectives = Seq(directive)
    val spanDirectives = Seq()
    val blockDirectives = Seq()

  }

  def forStyleSheetParser (parser: Parser[Set[StyleDeclaration]]): ExtensionBundle = new ExtensionBundle {

    override def parserDefinitions: ParserDefinitionBuilders = ParserDefinitionBuilders(
      styleSheetParser = Some(parser)
    )

  }

  def forHTMLTheme (theme: Theme[HTMLWriter]): ExtensionBundle = new ExtensionBundle {

    override def themeFor[Writer](rendererFactory: RendererFactory[Writer]): Theme[Writer] = rendererFactory match {
      case _: HTML => theme
      case _ => Theme[Writer]()
    }

  }

  def forFOTheme (theme: Theme[FOWriter]): ExtensionBundle = new ExtensionBundle {

    override def themeFor[Writer](rendererFactory: RendererFactory[Writer]): Theme[Writer] = rendererFactory match {
      case _: XSLFO => theme
      case _ => Theme[Writer]() // TODO - this might work better as partial function
    }

  }

}
