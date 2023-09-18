name := "site-gfm"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaExtensions += laika.format.Markdown.GitHubFlavor
