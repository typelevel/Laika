import laika.ast.LengthUnit.px
import laika.ast.Path.Root
import laika.ast._
import laika.config.LaikaKeys
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
import laika.rewrite.link.{ ApiLinks, LinkConfig }
import laika.rewrite.{ Version, Versions }
import laika.rewrite.nav.{ ChoiceConfig, CoverImage, SelectionConfig, Selections }
import laika.sbt.LaikaConfig
import laika.theme.ThemeProvider

object ManualSettings {

  private object versions {

    private def version(version: String, stable: Boolean = false): Version = {
      val label       = if (stable) Some("Stable") else Some("EOL")
      val pathSegment = if (stable) "latest" else version
      Version(version, pathSegment, "/table-of-content.html", label, canonical = stable)
    }

    val v019    = version("0.19", stable = true)
    val v018    = version("0.18")
    val v017    = version("0.17")
    val v016    = version("0.16")
    val current = v019
    val all     = Seq(v019, v018, v017, v016)

    val config = Versions(
      currentVersion = current,
      olderVersions = all.dropWhile(_ != current).drop(1),
      newerVersions = all.takeWhile(_ != current)
    )

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
    val api           = Root / "api" / "laika" / "api" / "index.html"
    val downloads     = Root / "downloads.gen"
    val logo          = images / "site" / "laika-dog-big@1.5x.png"
    val favicon       = images / "site" / "laika-favicon.png"
    val docsURL       = "https://typelevel.org/Laika/"
    val srcURL        = "https://github.com/typelevel/Laika"
    val docsSrcURL    = "https://github.com/typelevel/Laika/tree/main/docs/src"
    val demoURL       = "http://planet42.org"
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
    .withConfigValue(
      LinkConfig(
        apiLinks = Seq(
          ApiLinks("../api/")
        ), // TODO - will not work on top level pages, but fine for now - change to absolute path
        excludeFromValidation = Seq(Root / "api")
      )
    )
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
    .site.topNavigationBar(
      navLinks = Seq(
        IconLink.external(paths.srcURL, HeliumIcon.github),
        IconLink.internal(paths.api, HeliumIcon.api),
        IconLink.internal(paths.downloads, HeliumIcon.download),
        IconLink.external(paths.demoURL, HeliumIcon.demo)
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
    .site.baseURL(paths.docsURL)
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
      latestReleases = Seq(ReleaseInfo("Latest Release", "0.19.2")),
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
        TextLink.external(paths.demoURL, "Demo Application")
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
