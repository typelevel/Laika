import LaikaKeys._

name := "site-fopConfig"

version := "0.1"

scalaVersion := "2.10.6"

LaikaPlugin.defaults

fopConfig in Laika := Some(baseDirectory.value / "customFop.xconf")
