
name := "laika"

organization := "org.planet42"

version := "0.0.1"

scalaVersion := "2.9.2"

libraryDependencies <+= scalaVersion {
  case v if v startsWith "2.10" => "org.scalatest" %% "scalatest" % "1.9.1" % "test"
  case _                        => "org.scalatest" %% "scalatest" % "1.8" % "test"
}

crossVersion := CrossVersion.binary

crossScalaVersions := Seq("2.9.2", "2.10.0")