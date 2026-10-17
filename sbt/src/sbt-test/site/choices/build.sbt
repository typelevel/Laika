import laika.config._
import cats.data.NonEmptyChain

name := "site-epub"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

Laika / target := baseDirectory.value / "target" / "docs"

laikaConfig := LaikaConfig.defaults
  .withConfigValue(
    Selections(
      SelectionConfig(
        "config",
        ChoiceConfig("sbt", "sbt Plugin"),
        ChoiceConfig("library", "Library API")
      ).withSeparateEbooks
    )
  )
