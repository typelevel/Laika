import LaikaKeys._

name := "site-target"

version := "0.1"

scalaVersion := "2.10.6"

LaikaPlugin.defaults

target in Laika := baseDirectory.value / "docs"