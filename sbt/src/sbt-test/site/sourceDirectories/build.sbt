import LaikaKeys._

name := "site-sourceDirectories"

version := "0.1"

scalaVersion := "2.10.3"

LaikaPlugin.defaults

sourceDirectories in Laika += baseDirectory.value / "shared"