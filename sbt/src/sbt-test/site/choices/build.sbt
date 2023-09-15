import laika.rewrite.nav._
import cats.data.NonEmptyChain

name := "site-epub"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaConfig := LaikaConfig.defaults
  .withConfigValue(
    config.Selections(
      config.SelectionConfig(
        "config",
        config.ChoiceConfig("sbt", "sbt Plugin"),
        config.ChoiceConfig("library", "Library API")
      ).withSeparateEbooks
    )
  )
