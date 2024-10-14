import laika.format.Markdown.GitHubFlavor
import laika.config.SyntaxHighlighting
import sbt.Keys.crossScalaVersions
import org.scalajs.linker.interface.ESVersion
import com.typesafe.tools.mima.core.{
  ProblemFilters,
  MissingClassProblem,
  ReversedMissingMethodProblem,
  DirectMissingMethodProblem,
  MissingTypesProblem
}
import Dependencies._

inThisBuild(
  Seq(
    tlBaseVersion        := "1.2",
    homepage             := Some(url("https://typelevel.org/Laika/")),
    organization         := "org.typelevel",
    organizationHomepage := Some(url("http://typelevel.org")),
    description          := "Text Markup Transformer for sbt and Scala applications",
    startYear            := Some(2012),
    licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    crossScalaVersions := Seq(versions.scala2_12, versions.scala2_13, versions.scala3),
    scalaVersion       := versions.scala2_12,
    developers         := List(Developer("jenshalm", "Jens Halm", "", url("http://planet42.org"))),
    tlCiHeaderCheck    := false,
    tlCiDependencyGraphJob := false,
    githubWorkflowJavaVersions += JavaSpec.temurin("17"),
    githubWorkflowBuildMatrixAdditions ~= { matrix =>
      matrix + ("project" -> (matrix("project") :+ "plugin"))
    },
    githubWorkflowBuildMatrixExclusions ++= {
      MatrixExclude(Map("project" -> "plugin", "java" -> JavaSpec.temurin("17").render)) ::
        List("2.13", "3").map(scala => MatrixExclude(Map("project" -> "plugin", "scala" -> scala)))
    },
    githubWorkflowBuild ++= Seq(
      WorkflowStep.Sbt(
        List("docs/mdoc", "docs/laikaSite"),
        name = Some("Build manual"),
        cond = Some(
          "matrix.scala == '2.12' && matrix.project == 'rootJVM' && matrix.java == 'temurin@17'"
        )
      ),
      WorkflowStep.Sbt(
        List("plugin/scripted"),
        name = Some("sbt scripted tests"),
        cond = Some("matrix.project == 'plugin'")
      )
    )
  )
)

def inferOverrideFor2_13(version: String): Seq[String] =
  if (version.startsWith("2.13")) Seq("-Xsource-features:infer-override") else Nil

def disableMissingInterpolatorWarning(options: Seq[String]): Seq[String] =
  options.map { opt =>
    if (opt.startsWith("-Xlint")) opt + ",-missing-interpolator" else opt
  }

def disableUnusedWarningsForMdoc(options: Seq[String]): Seq[String] =
  options.map { opt =>
    if (opt.startsWith("-Ywarn-unused")) opt + ",-locals,-explicits" else opt
  }

val munit = "org.scalameta" %% "munit" % versions.munit % "test"
val jTidy = "net.sf.jtidy"   % "jtidy" % versions.jTidy % "test"

val catsEffect = "org.typelevel" %% "cats-effect"       % versions.catsEffect
val fs2IO      = "co.fs2"        %% "fs2-io"            % versions.fs2
val munitCE3   = "org.typelevel" %% "munit-cats-effect" % versions.munitCE3 % "test"

val fop = "org.apache.xmlgraphics" % "fop" % versions.fop

val http4s = Seq(
  "org.http4s" %% "http4s-dsl"          % versions.http4s,
  "org.http4s" %% "http4s-ember-server" % versions.http4s
)

lazy val root = tlCrossRootProject
  .aggregate(core, pdf, io, preview, api)
  .configureRoot { root =>
    root.aggregate(plugin) // don't include the plugin in rootJVM, only in root
      .settings(
        crossScalaVersions := Nil,
        scalaVersion       := versions.scala2_12
      )
  }

lazy val docs = project.in(file("docs"))
  .dependsOn(plugin)
  .enablePlugins(LaikaPlugin)
  .enablePlugins(MdocPlugin)
  .enablePlugins(SbtPlugin)
  .enablePlugins(NoPublishPlugin)
  .settings(
    name                      := "laika-docs",
    laikaTheme                := ManualSettings.helium,
    laikaConfig               := ManualSettings.config,
    laikaExtensions           := Seq(GitHubFlavor, SyntaxHighlighting, ManualBundle),
    Laika / sourceDirectories := Seq(mdocOut.value),
    Laika / target            := baseDirectory.value / "target",
    mdocIn                    := baseDirectory.value / "src",
    mdocVariables             := Map(
      "LAIKA_VERSION" -> ManualSettings.versions.latestRelease
    ),
    mdocExtraArguments        := Seq("--no-link-hygiene"),
    scalacOptions ~= disableUnusedWarningsForMdoc
  )

lazy val api = project
  .in(file("unidoc"))
  .enablePlugins(TypelevelUnidocPlugin)
  .settings(
    name                                       := "laika-docs",
    ScalaUnidoc / unidoc / unidocProjectFilter := {
      if (scalaBinaryVersion.value == "2.12")
        inProjects(core.jvm, io, pdf, preview, plugin)
      else
        inProjects(core.jvm, io, pdf, preview)
    },
    Compile / packageDoc / mappings            :=
      ScaladocCleanup.removeUnwantedEntries(
        (ScalaUnidoc / packageDoc / mappings).value,
        (ThisBuild / baseDirectory).value,
        scalaBinaryVersion.value
      )
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    name := "laika-core",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"     % versions.munit % "test",
      "org.typelevel" %%% "cats-core" % versions.catsCore
    ),
    scalacOptions ++= inferOverrideFor2_13(scalaVersion.value),
    Test / scalacOptions ~= disableMissingInterpolatorWarning
  )
  .jvmSettings(
    libraryDependencies += jTidy
  )
  .jsSettings(
    Test / scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule).withESFeatures(_.withESVersion(ESVersion.ES2018))
    }
  )

lazy val io = project.in(file("io"))
  .dependsOn(core.jvm % "compile->compile;test->test")
  .settings(
    name := "laika-io",
    libraryDependencies ++= Seq(catsEffect, fs2IO, munit, munitCE3),
    Test / scalacOptions ~= disableMissingInterpolatorWarning,
    mimaBinaryIssueFilters ++= MimaFilters.includeRefactoring
  )

lazy val pdf = project.in(file("pdf"))
  .dependsOn(core.jvm % "compile->compile;test->test", io % "compile->compile;test->test")
  .settings(
    name := "laika-pdf",
    libraryDependencies ++= Seq(fop, munit)
  )

lazy val preview = project.in(file("preview"))
  .dependsOn(core.jvm, io % "compile->compile;test->test", pdf)
  .settings(
    name := "laika-preview",
    libraryDependencies ++= (http4s :+ munit),
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[ReversedMissingMethodProblem](
        "laika.preview.ServerConfig.binaryRenderers"
      ),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
        "laika.preview.ServerConfig.withBinaryRenderers"
      ),
      ProblemFilters.exclude[DirectMissingMethodProblem]("laika.preview.ServerConfig#Impl.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("laika.preview.ServerConfig#Impl.apply"),
      ProblemFilters.exclude[MissingTypesProblem]("laika.preview.ServerConfig$Impl$")
    )
  )

lazy val plugin = project.in(file("sbt"))
  .dependsOn(core.jvm, io, pdf, preview)
  .enablePlugins(SbtPlugin)
  .settings(
    name               := "laika-sbt",
    sbtPlugin          := true,
    crossScalaVersions := Seq(versions.scala2_12),
    scriptedLaunchOpts ++= Seq(
      "-Xmx1024M",
      "-Dplugin.version=" + version.value,
      "-Duser.language=en",
      "-Duser.country=GB"
    ),
    scriptedBufferLog  := false,
    scripted           := scripted.dependsOn(
      core.jvm / publishLocal,
      io / publishLocal,
      pdf / publishLocal,
      preview / publishLocal
    ).evaluated,
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingClassProblem]("laika.sbt.Tasks$OutputFormat"),
      ProblemFilters.exclude[MissingClassProblem]("laika.sbt.Tasks$OutputFormat$"),
      ProblemFilters.exclude[MissingClassProblem]("laika.sbt.Tasks$OutputFormat$AST$"),
      ProblemFilters.exclude[MissingClassProblem]("laika.sbt.Tasks$OutputFormat$EPUB$"),
      ProblemFilters.exclude[MissingClassProblem]("laika.sbt.Tasks$OutputFormat$HTML$"),
      ProblemFilters.exclude[MissingClassProblem]("laika.sbt.Tasks$OutputFormat$PDF$"),
      ProblemFilters.exclude[MissingClassProblem]("laika.sbt.Tasks$OutputFormat$XSLFO$")
    )
  )
