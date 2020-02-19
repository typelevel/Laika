lazy val basicSettings = Seq(
  version               := "0.14.0-SNAPSHOT",
  homepage              := Some(new URL("http://planet42.github.io/Laika/")),
  organization          := "org.planet42",
  organizationHomepage  := Some(new URL("http://planet42.org")),
  description           := "Text Markup Transformer for sbt and Scala applications",
  startYear             := Some(2012),
  licenses              := Seq("Apache 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion          := "2.12.10",
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
  crossVersion       := CrossVersion.binary,
  crossScalaVersions := Seq("2.12.10", "2.13.1")
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

val scalatest  = "org.scalatest"          %% "scalatest"   % "3.0.8" % "test"
val jTidy      = "net.sf.jtidy"           %  "jtidy"       % "r938"  % "test"
val catsCore   = "org.typelevel"          %% "cats-core"   % "2.1.0"
val catsEffect = "org.typelevel"          %% "cats-effect" % "2.0.0"
val fop        = "org.apache.xmlgraphics" %  "fop"         % "2.3"
val http4s     = Seq(
                   "org.http4s"           %% "http4s-dsl"  % "0.21.0",
                   "org.http4s"           %% "http4s-blaze-server" % "0.21.0"
                 )

lazy val root = project.in(file("."))
  .aggregate(core, pdf, plugin)
  .settings(basicSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(plugin))

lazy val core = project.in(file("core"))
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-core",
    libraryDependencies ++= Seq(catsCore, scalatest, jTidy)
  )

lazy val io = project.in(file("io"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-io",
    libraryDependencies ++= Seq(scalatest, catsEffect)
  )
  
lazy val pdf = project.in(file("pdf"))
  .dependsOn(core % "compile->compile;test->test", io % "compile->compile;test->test")
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-pdf",
    libraryDependencies ++= Seq(fop, scalatest)
  )
  
lazy val plugin = project.in(file("sbt"))
  .dependsOn(core, io, pdf)
  .enablePlugins(SbtPlugin)
  .settings(basicSettings)
  .settings(
    name := "laika-sbt",
    sbtPlugin := true,
    crossScalaVersions := Seq("2.12.8"),
    publishMavenStyle := false,
    bintrayRepository := "sbt-plugins",
    bintrayOrganization := None,
    scriptedLaunchOpts ++= Seq("-Xmx1024M", "-Dplugin.version=" + version.value),
    scriptedBufferLog := false
  )

lazy val demo = project.in(file("demo"))
  .dependsOn(core)
  .enablePlugins(sbtdocker.DockerPlugin, JavaAppPackaging)
  .settings(basicSettings)
  .settings(
    name := "laika-demo",
    version := "0.14.0.1",
    libraryDependencies ++= http4s,
    scalacOptions ++= Seq("-Ypartial-unification"),
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
