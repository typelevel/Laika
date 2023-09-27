import laika.ast.LengthUnit.px
import laika.ast.Path.Root
import laika.ast._
import laika.helium.Helium
import laika.helium.config.{
  Favicon,
  HeliumIcon,
  IconLink,
  ReleaseInfo,
  Teaser,
  TextLink,
  VersionMenu
}
import laika.config.*
import laika.sbt.LaikaConfig
import laika.theme.ThemeProvider

object ManualSettings {

  private object versions {

    private def version(version: String, label: String = "EOL"): Version = {
      val (pathSegment, canonical) = version match {
        case "1.x" => ("latest", true)
        case _     => (version, false)
      }
      val v                        =
        Version(version, pathSegment).withFallbackLink("/table-of-content.html").withLabel(label)
      if (canonical) v.setCanonical else v
    }

    val v1      = version("1.x", "Dev")
    val v019    = version("0.19", "Stable")
    val v018    = version("0.18")
    val v017    = version("0.17")
    val current = v1
    val all     = Seq(v1, v019, v018, v017)

    val config = Versions
      .forCurrentVersion(current)
      .withOlderVersions(all.dropWhile(_ != current).drop(1) *)
      .withNewerVersions(all.takeWhile(_ != current) *)

  }

  private object paths {
    val images = Root / "img"

    object epub {

      val coverSbt = CoverImage(
        images / "cover" / s"e-book-cover-sbt-${versions.current.displayValue}.png",
        "sbt"
      )

      val coverLib = CoverImage(
        images / "cover" / s"e-book-cover-lib-${versions.current.displayValue}.png",
        "library"
      )

    }

    object pdf {

      val coverSbt =
        CoverImage(images / "pdf" / s"e-book-cover-sbt-${versions.current.displayValue}.png", "sbt")

      val coverLib = CoverImage(
        images / "pdf" / s"e-book-cover-lib-${versions.current.displayValue}.png",
        "library"
      )

    }

    val latestVersion = Root / versions.current.pathSegment
    val downloads     = Root / "downloads.gen"
    val logo          = images / "site" / "laika-dog-big@1.5x.png"
    val favicon       = images / "site" / "laika-favicon.png"
    val siteBaseURL   = "https://typelevel.org/Laika/"
    val apiURL        = "https://javadoc.io/doc/org.typelevel/laika-docs_2.12/latest/"
    val srcURL        = "https://github.com/typelevel/Laika"
    val docsSrcURL    = s"$srcURL/tree/main/docs/src"
    val chatURL       = "https://discord.gg/XF3CXcMzqD"
  }

  private object text {

    val mainDesc =
      "Site and E-book Generator and Customizable Text Markup Transformer for sbt, Scala and Scala.js"

    val downloadDesc =
      "The e-books for the sbt plugin and the library API have exactly the same content apart from the code shown for all" +
        " configuration examples which will be either sbt settings or library API usage, depending on your choice of e-book."

    val teasers = Seq(
      Teaser(
        "No External Tools",
        "Easy setup without any external tools or languages and only minimal library dependencies."
      ),
      Teaser(
        "Flexible Runtime",
        "Laika can be used as an sbt plugin, as a Scala library for the JVM or in the browser via Scala.js."
      ),
      Teaser(
        "Purely Functional",
        "Fully referentially transparent, no exceptions or runtime reflection and integration with cats-effect for polymorphic effect handling."
      ),
      Teaser(
        "Rich Feature Set",
        "Markdown and reStructuredText as input, HTML, EPUB and PDF as output, integrated preview server and syntax highlighting, versioned documentation, and much more."
      ),
      Teaser(
        "Lightweight Theme",
        "The default Helium theme includes only a minimal amount of handcrafted CSS and JS, no Bootstrap, no frameworks."
      ),
      Teaser(
        "Highly Extensible",
        "Process the document AST, adjust rendering for individual AST nodes or extend text markup languages with custom directives."
      )
    )

  }

  val config: LaikaConfig = LaikaConfig.defaults
    .withConfigValue(LinkConfig.empty.addApiLinks(ApiLinks(paths.apiURL)))
    .withConfigValue(LinkValidation.Global(Seq(Root / "api")))
    .withConfigValue(
      Selections(
        SelectionConfig(
          "config",
          ChoiceConfig("sbt", "sbt Plugin"),
          ChoiceConfig("library", "Library API")
        ).withSeparateEbooks
      )
    )
    .withConfigValue(LaikaKeys.artifactBaseName, s"laika-${versions.current.displayValue}")
    .withConfigValue(LaikaKeys.versioned, true)

  val helium: ThemeProvider = Helium.defaults
    .all.metadata(
      title = Some("Laika"),
      description = Some(text.mainDesc),
      version = Some(versions.current.displayValue),
      language = Some("en")
    )
    .all.tableOfContent("Table of Content", depth = 4)
    .site.internalCSS(Root / "css" / "manual.css")
    .epub.internalCSS(Root / "css" / "manual.epub.css")
    .site.topNavigationBar(
      navLinks = Seq(
        IconLink.external(paths.srcURL, HeliumIcon.github),
        IconLink.external(paths.apiURL + "laika/", HeliumIcon.api),
        IconLink.internal(paths.downloads, HeliumIcon.download),
        IconLink.external(paths.chatURL, HeliumIcon.chat)
      ),
      versionMenu = VersionMenu.create(
        "Version",
        "Choose Version",
        additionalLinks =
          Seq(TextLink.internal(Root / "olderVersions" / "README.md", "Older Versions"))
      )
    )
    .site.favIcons(Favicon.internal(paths.favicon, "32x32"))
    .site.pageNavigation(sourceBaseURL = Some(paths.docsSrcURL))
    .site.downloadPage("Documentation Downloads", Some(text.downloadDesc))
    .site.versions(versions.config)
    .site.baseURL(paths.siteBaseURL)
    .site.landingPage(
      logo = Some(
        Image.internal(
          paths.logo,
          width = Some(px(327)),
          height = Some(px(393)),
          alt = Some("Laika Logo")
        )
      ),
      subtitle = Some(text.mainDesc),
      latestReleases = Seq(
        ReleaseInfo("Latest Pre-Release", "1.0.0-M5"),
        ReleaseInfo("Latest Stable Release", "0.19.5")
      ),
      license = Some("Apache 2.0"),
      documentationLinks = Seq(
        TextLink.internal(Root / "01-about-laika" / "01-features.md", "Features"),
        TextLink.internal(Root / "02-running-laika" / "01-sbt-plugin.md", "sbt Plugin"),
        TextLink.internal(Root / "02-running-laika" / "02-library-api.md", "Library API"),
        TextLink.internal(Root / "table-of-content", "Table of Content"),
        TextLink.internal(paths.downloads, "Download (PDF & EPUB)"),
        TextLink.internal(Root / "api" / "laika" / "api" / "index.html", "API (Scaladoc)")
      ),
      projectLinks = Seq(
        TextLink.external(paths.srcURL, "Source on GitHub"),
        TextLink.external(paths.chatURL, "Typelevel Chat")
      ),
      teasers = text.teasers
    )
    .epub.metadata(
      authors = Seq("Jens Halm"),
      identifier = Some(
        s"org.planet42.laika.manual.3.${versions.current.displayValue}"
      ), // TODO - should apply classifier
      title = Some(s"Laika ${versions.current.displayValue}")
    )
    .epub.navigationDepth(2)
    .epub.coverImages(paths.epub.coverSbt, paths.epub.coverLib)
    .pdf.coverImages(paths.pdf.coverSbt, paths.pdf.coverLib)
    .build

}
