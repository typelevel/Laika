
name := "site-targetAPI"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaConfig := LaikaConfig.defaults.withConfigValue(laika.config.LaikaKeys.site.apiPath, laika.ast.Path.Root / "latest" / "api")

laikaIncludeAPI := true
