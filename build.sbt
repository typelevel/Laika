lazy val basicSettings = Seq(
  version               := "0.12.0-SNAPSHOT",
  homepage              := Some(new URL("http://planet42.github.io/Laika/")),
  organization          := "org.planet42",
  organizationHomepage  := Some(new URL("http://planet42.org")),
  description           := "Text Markup Transformer for sbt and Scala applications",
  startYear             := Some(2012),
  licenses              := Seq("Apache 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion          := "2.12.8",
  scalacOptions         := Opts.compile.encoding("UTF-8") :+ 
                           Opts.compile.deprecation :+ 
                           Opts.compile.unchecked :+ 
                           "-feature" :+ 
                           "-language:implicitConversions" :+ 
                           "-language:postfixOps" :+ 
                           "-language:higherKinds"
)

lazy val moduleSettings = basicSettings ++ Seq(
  crossVersion       := CrossVersion.binary,
  crossScalaVersions := Seq("2.12.8", "2.13.0-RC1")
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

val scalatest  = "org.scalatest"          %% "scalatest"   % "3.0.8-RC2" % "test"
val jTidy      = "net.sf.jtidy"           %  "jtidy"       % "r938"      % "test"
val config     = "com.typesafe"           %  "config"      % "1.2.1"
val catsEffect = "org.typelevel"          %% "cats-effect" % "2.0.0-M1"
val fop        = "org.apache.xmlgraphics" %  "fop"         % "2.3"

lazy val root = project.in(file("."))
  .aggregate(core, pdf, plugin)
  .disablePlugins(ScriptedPlugin)
  .settings(basicSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(plugin))

lazy val core = project.in(file("core"))
  .disablePlugins(ScriptedPlugin)
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-core",
    libraryDependencies ++= Seq(config, scalatest, jTidy)
  )

lazy val io = project.in(file("io"))
  .dependsOn(core % "compile->compile;test->test")
  .disablePlugins(ScriptedPlugin)
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-io",
    libraryDependencies ++= Seq(config, scalatest, catsEffect)
  )
  
lazy val pdf = project.in(file("pdf"))
  .dependsOn(core % "compile->compile;test->test", io % "compile->compile;test->test")
  .disablePlugins(ScriptedPlugin)
  .settings(moduleSettings)
  .settings(publishSettings)
  .settings(
    name := "laika-pdf",
    libraryDependencies ++= Seq(fop, scalatest)
  )
  
lazy val plugin = project.in(file("sbt"))
  .dependsOn(core, io, pdf)
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
