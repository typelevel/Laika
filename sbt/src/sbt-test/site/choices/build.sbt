
import laika.rewrite.nav._
import cats.data.NonEmptyChain

name := "site-epub"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaConfig := LaikaConfig.defaults
  .withConfigValue(ChoiceGroupsConfig(Seq(ChoiceGroupConfig("config", NonEmptyChain(
    ChoiceConfig("sbt", "sbt Plugin"),
    ChoiceConfig("library", "Library API")
  ), separateEbooks = true))))
