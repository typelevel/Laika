import laika.config.MessageFilters

name := "site-renderMessageLevel"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

Laika / target := baseDirectory.value / "target" / "docs"

laikaConfig := LaikaConfig.defaults
  .withMessageFilters(MessageFilters.forVisualDebugging)
