import laika.markdown.github.GitHubFlavor
import laika.parse.code.SyntaxHighlighting
import sbt.Keys.{artifactPath, crossScalaVersions}

val scala2_12 = "2.12.13"
val scala2_13 = "2.13.5"
val scala3_0 = "3.0.0-RC2"

lazy val basicSettings = Seq(
  version               := "0.18.0-SNAPSHOT",
  homepage              := Some(new URL("http://planet42.github.io/Laika/")),
  organization          := "org.planet42",
  organizationHomepage  := Some(new URL("http://planet42.org")),
  description           := "Text Markup Transformer for sbt and Scala applications",
  startYear             := Some(2012),
  licenses              := Seq("Apache 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion          := scala2_12,
  scalacOptions         := (Opts.compile.encoding("UTF-8") :+ 
                           Opts.compile.deprecation :+ 
                           Opts.compile.unchecked :+ 
                           "-feature" :+ 
                           "-language:implicitConversions" :+ 
                           "-language:postfixOps" :+ 
                           "-language:higherKinds")  ++ 
                             (if (priorTo2_13(scalaVersion.value)) Seq("-Ypartial-unification") else Nil)
                           
)

def priorTo2_13(version: String): Boolean =
  CrossVersion.partialVersion(version) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val moduleSettings = basicSettings ++ Seq(
  crossScalaVersions := Seq(scala2_12, scala2_13, scala3_0)
)

lazy val publishSettings = Seq(
  publishMavenStyle       := true,
  publishArtifact in Test := false,
  pomIncludeRepository    := { _ => false },
  publishTo := {
    if (version.value.trim.endsWith("SNAPSHOT")) None
    else Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <scm>
      <url>https://github.com/planet42/Laika.git</url>
      <connection>scm:git:https://github.com/planet42/Laika.git</connection>
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
  publish := (()),
  publishLocal := (()),
  publishTo := None
)

val scalatest  = "org.scalatest"          %% "scalatest"   % "3.2.7" % "test"
val jTidy      = "net.sf.jtidy"           %  "jtidy"       % "r938"  % "test"

val catsEffect = "org.typelevel"          %% "cats-effect" % "3.0.1"

val fop        = "org.apache.xmlgraphics" %  "fop"         % "2.6"
val http4s     = Seq(
                   "org.http4s"           %% "http4s-dsl"          % "0.21.20",
                   "org.http4s"           %% "http4s-blaze-server" % "0.21.20"
                 )
val http4sM    = Seq(
                    "org.http4s"          %% "http4s-dsl"          % "1.0.0-M21",
                    "org.http4s"          %% "http4s-blaze-server" % "1.0.0-M21"
                 )

lazy val root = project.in(file("."))
  .aggregate(core.js, core.jvm, pdf, io, preview, plugin)
  .settings(basicSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    crossScalaVersions := Nil,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(plugin, core.js, demo.jvm, demo.js)
  )

lazy val docs = project.in(file("docs"))
  .enablePlugins(LaikaPlugin)
  .settings(noPublishSettings)
  .settings(
    name := "laika-docs",
    laikaTheme := ManualSettings.helium,
    laikaConfig := ManualSettings.config,
    laikaExtensions := Seq(GitHubFlavor, SyntaxHighlighting, ManualBundle),
    Laika / sourceDirectories := Seq(baseDirectory.value / "src"),
    Laika / target := baseDirectory.value / "target"
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(basicSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-core",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.7" % "test",
      "org.typelevel" %%% "cats-core" % "2.5.0"
    )
  )
  .jvmSettings(
    libraryDependencies += jTidy, 
    crossScalaVersions := Seq(scala2_12, scala2_13, scala3_0)
  )
  .jsSettings(
    crossScalaVersions := Seq(scala2_12, scala2_13)
  )

lazy val io = project.in(file("io"))
  .dependsOn(core.jvm % "compile->compile;test->test")
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-io",
    libraryDependencies ++= Seq(scalatest, catsEffect)
  )
  
lazy val pdf = project.in(file("pdf"))
  .dependsOn(core.jvm % "compile->compile;test->test", io % "compile->compile;test->test")
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-pdf",
    libraryDependencies ++= Seq(fop, scalatest)
  )

lazy val preview = project.in(file("preview"))
  .dependsOn(core.jvm, io % "compile->compile;test->test", pdf)
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-preview",
    libraryDependencies ++= (http4sM :+ scalatest)
  )

lazy val plugin = project.in(file("sbt"))
  .dependsOn(core.jvm, io, pdf, preview)
  .enablePlugins(SbtPlugin)
  .settings(basicSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-sbt",
    sbtPlugin := true,
    crossScalaVersions := Seq(scala2_12),
    scriptedLaunchOpts ++= Seq("-Xmx1024M", "-Dplugin.version=" + version.value),
    scriptedBufferLog := false
  )

lazy val demo = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("demo"))
  .dependsOn(core)
  .enablePlugins(sbtdocker.DockerPlugin, JavaAppPackaging)
  .settings(basicSettings)
  .settings(
    name := "laika-demo",
    version := "0.16.0.0"
  )
  .jvmSettings(
    libraryDependencies ++= http4s,
    javaOptions in Universal ++= Seq(
      "-J-Xms512M",
      "-J-Xmx896M"
    ),
    buildOptions in docker := BuildOptions (
      cache = false,
      removeIntermediateContainers = BuildOptions.Remove.Always,
      pullBaseImage = BuildOptions.Pull.Always
    ),
    dockerfile in docker := {
      val appDir: File = stage.value
      val targetDir = "/app"

      new Dockerfile {
        from("openjdk:8")
        expose(8080)
        env("VERSION", version.value)
        entryPoint(s"$targetDir/bin/${executableScriptName.value}")
        copy(appDir, targetDir)
      }
    },
    imageNames in docker := Seq(ImageName(
      namespace = None,
      repository = name.value,
      tag = Some(version.value)
    ))
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    artifactPath in (Compile, fastOptJS) :=
      (ThisBuild / baseDirectory).value / "demo" / "client" / "src" / "transformer" / "transformer.mjs", 
    artifactPath in (Compile, fullOptJS) :=
      (ThisBuild / baseDirectory).value / "demo" / "client" / "src" / "transformer" / "transformer-opt.mjs"
  )

  