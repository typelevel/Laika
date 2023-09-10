import laika.markdown.github.GitHubFlavor
import laika.parse.code.SyntaxHighlighting
import sbt.Keys.crossScalaVersions
import org.scalajs.linker.interface.ESVersion
import com.typesafe.tools.mima.core.{ ProblemFilters, DirectMissingMethodProblem }
import Dependencies._

lazy val basicSettings = Seq(
  version              := "0.19.4",
  homepage             := Some(new URL("https://typelevel.org/Laika/")),
  organization         := "org.planet42",
  organizationHomepage := Some(new URL("http://typelevel.org")),
  description          := "Text Markup Transformer for sbt and Scala applications",
  startYear            := Some(2012),
  licenses     := Seq("Apache 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion := versions.scala2_12,
  scalacOptions ++= (Opts.compile.encoding("UTF-8") :+
    Opts.compile.deprecation :+
    Opts.compile.unchecked :+
    "-feature" :+
    "-language:implicitConversions" :+
    "-language:postfixOps" :+
    "-language:higherKinds") ++
    (if (priorTo2_13(scalaVersion.value)) Seq("-Ypartial-unification") else Nil)
)

val mimaPreviousVersions = Set("0.19.0", "0.19.1", "0.19.2", "0.19.3")

val previousArtifacts = Seq(
  mimaPreviousArtifacts := mimaPreviousVersions
    .map(v => projectID.value.withRevision(v).withExplicitArtifacts(Vector.empty))
)

def priorTo2_13(version: String): Boolean =
  CrossVersion.partialVersion(version) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val moduleSettings = basicSettings ++ previousArtifacts ++ Seq(
  crossScalaVersions := Seq(versions.scala2_12, versions.scala2_13, versions.scala3)
)

lazy val publishSettings = Seq(
  publishMavenStyle      := true,
  Test / publishArtifact := false,
  pomIncludeRepository   := { _ => false },
  publishTo              := {
    if (version.value.trim.endsWith("SNAPSHOT")) None
    else Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
  },
  pomExtra               := (<scm>
      <url>https://github.com/typelevel/Laika.git</url>
      <connection>scm:git:https://github.com/typelevel/Laika.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jenshalm</id>
        <name>Jens Halm</name>
        <url>http://planet42.org</url>
      </developer>
    </developers>)
)

lazy val noPublishSettings = Seq(
  publish      := (()),
  publishLocal := (()),
  publishTo    := None
)

val munit = "org.scalameta" %% "munit" % versions.munit % "test"
val jTidy = "net.sf.jtidy"   % "jtidy" % versions.jTidy % "test"

val catsEffect = "org.typelevel" %% "cats-effect"         % versions.catsEffect
val fs2IO      = "co.fs2"        %% "fs2-io"              % versions.fs2
val munitCE3   = "org.typelevel" %% "munit-cats-effect-3" % versions.munitCE3 % "test"

val fop = "org.apache.xmlgraphics" % "fop" % versions.fop

val http4s = Seq(
  "org.http4s" %% "http4s-dsl"          % versions.http4s,
  "org.http4s" %% "http4s-ember-server" % versions.http4s
)

lazy val root = project.in(file("."))
  .aggregate(core.js, core.jvm, pdf, io, preview, plugin)
  .settings(basicSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .disablePlugins(MimaPlugin)
  .settings(
    crossScalaVersions                         := Nil,
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
      plugin,
      core.js
    )
  )

lazy val docs = project.in(file("docs"))
  .dependsOn(plugin)
  .enablePlugins(LaikaPlugin)
  .enablePlugins(MdocPlugin)
  .enablePlugins(SbtPlugin)
  .settings(noPublishSettings)
  .settings(
    name                      := "laika-docs",
    laikaTheme                := ManualSettings.helium,
    laikaConfig               := ManualSettings.config,
    laikaExtensions           := Seq(GitHubFlavor, SyntaxHighlighting, ManualBundle),
    Laika / sourceDirectories := Seq(mdocOut.value),
    Laika / target            := baseDirectory.value / "target",
    mdocIn                    := baseDirectory.value / "src",
    mdocVariables             := Map(
      "LAIKA_VERSION" -> "0.19.4"
    ),
    mdocExtraArguments        := Seq("--no-link-hygiene")
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-core",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"     % versions.munit % "test",
      "org.typelevel" %%% "cats-core" % versions.catsCore
    )
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
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-io",
    libraryDependencies ++= Seq(catsEffect, fs2IO, munit, munitCE3),
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "laika.helium.builder.HeliumThemeBuilder#directives.this"
      )
    )
  )

lazy val pdf = project.in(file("pdf"))
  .dependsOn(core.jvm % "compile->compile;test->test", io % "compile->compile;test->test")
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-pdf",
    libraryDependencies ++= Seq(fop, munit)
  )

lazy val preview = project.in(file("preview"))
  .dependsOn(core.jvm, io % "compile->compile;test->test", pdf)
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-preview",
    libraryDependencies ++= (http4s :+ munit)
  )

lazy val plugin = project.in(file("sbt"))
  .dependsOn(core.jvm, io, pdf, preview)
  .enablePlugins(SbtPlugin)
  .settings(basicSettings)
  .settings(previousArtifacts)
  .settings(publishSettings)
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
    scriptedBufferLog  := false
  )
