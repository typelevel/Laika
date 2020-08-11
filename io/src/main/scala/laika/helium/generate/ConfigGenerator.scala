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

package laika.helium.generate

import laika.ast.{DocumentCursor, ExternalTarget, InternalTarget, InvalidElement, NoOpt, Options, Path, Span, SpanResolver, SpanSequence, TemplateElement, TemplateSpan, TemplateSpanSequence, TemplateString, Text}
import laika.config.{ASTValue, Config, ConfigBuilder, ConfigEncoder}
import laika.helium.{Favicon, Helium, LandingPage, MarkupEditLinks, PDFLayout, ReleaseInfo, Teaser, ThemeTarget, TopNavigationBar}
import laika.theme.ThemeFonts

/**
  * @author Jens Halm
  */
object ConfigGenerator {

  implicit val releaseEncoder: ConfigEncoder[ReleaseInfo] = ConfigEncoder[ReleaseInfo] { releaseInfo =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("title", releaseInfo.title)
      .withValue("version", releaseInfo.version)
      .build
  }

  implicit val teaserEncoder: ConfigEncoder[Teaser] = ConfigEncoder[Teaser] { teaser =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("title", teaser.title)
      .withValue("description", teaser.description)
      .build
  }

  implicit val landingPageEncoder: ConfigEncoder[LandingPage] = ConfigEncoder[LandingPage] { landingPage =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("logo", landingPage.logo)
      .withValue("title", landingPage.title)
      .withValue("subtitle", landingPage.subtitle)
      .withValue("latestReleases", landingPage.latestReleases)
      .withValue("license", landingPage.license)
      .withValue("documentationLinks", landingPage.documentationLinks)
      .withValue("projectLinks", landingPage.projectLinks)
      .withValue("teasers", landingPage.teasers) // TODO - change to teaserRows
      .build
  }

  implicit val topNavBarEncoder: ConfigEncoder[TopNavigationBar] = ConfigEncoder[TopNavigationBar] { navBar =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("home", navBar.logo)
      .withValue("links", SpanSequence(navBar.links))
      .build
  }

  implicit val markupEditsEncoder: ConfigEncoder[MarkupEditLinks] = ConfigEncoder[MarkupEditLinks] { links =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("text", links.text)
      .withValue("baseURL", links.baseURL.stripSuffix("/"))
      .build
  }

  implicit val pdfLayoutEncoder: ConfigEncoder[PDFLayout] = ConfigEncoder[PDFLayout] { layout =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("pageHeight", layout.pageHeight.displayValue)
      .withValue("pageWidth", layout.pageWidth.displayValue)
      .withValue("marginTop", layout.marginTop.displayValue)
      .withValue("marginBottom", layout.marginBottom.displayValue)
      .withValue("marginLeft", layout.marginLeft.displayValue)
      .withValue("marginRight", layout.marginRight.displayValue)
      .build
  }

  implicit val themeFontsEncoder: ConfigEncoder[ThemeFonts] = ConfigEncoder[ThemeFonts] { fonts =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("headlines", fonts.headlines)
      .withValue("body", fonts.body)
      .withValue("code", fonts.code)
      .build
  }
  
  case class RelativePath (target: ThemeTarget) extends SpanResolver {
    type Self = RelativePath
    def resolve(cursor: DocumentCursor): Span = target.resolve(cursor) match {
      case Left(msg) => InvalidElement(s"unresolved target ${target.description}: $msg", "<unresolved target>").asSpan
      case Right(InternalTarget(_, relativePath, _)) => Text(relativePath.toString)
      case Right(ExternalTarget(url)) => Text(url)
    } 
    def unresolvedMessage: String = s"Unresolved target '${target.description}'"
    def options: Options = NoOpt
    def withOptions(options: Options): RelativePath = this
  }
  
  implicit val navIconEncoder: ConfigEncoder[Favicon] = ConfigEncoder[Favicon] { icon =>
    val sizes = icon.sizes.fold("")(s => s"""sizes="$s"""")
    val mediaType = icon.mediaType.fold("")(t => s"""type="$t"""")
    val elements: Seq[TemplateSpan] = Seq(
      TemplateString(s"""<link rel="icon" $sizes $mediaType href=""""),
      TemplateElement(RelativePath(icon.target)),
      TemplateString("""" />""")
    )
    ASTValue(TemplateSpanSequence(elements))
  }

  def populateConfig (helium: Helium): Config =
    ConfigBuilder.empty
      .withValue("helium.landingPage", helium.landingPage)
      .withValue("helium.topBar", helium.webLayout.topNavigationBar)
      .withValue("helium.favIcons", helium.webLayout.favIcons)
      .withValue("helium.markupEditLinks", helium.webLayout.markupEditLinks)
      .withValue("helium.pdf", helium.pdfLayout)
      .withValue("helium.themeFonts", helium.themeFonts)
      .withValue(helium.fontResources)
      .build
  
}
