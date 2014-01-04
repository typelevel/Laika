import LaikaKeys._

name := "site-target"

version := "0.1"

scalaVersion := "2.10.3"

LaikaPlugin.defaults

target in Laika := baseDirectory.value / "docs"