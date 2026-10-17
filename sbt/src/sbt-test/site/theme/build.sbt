import laika.helium._
import laika.theme.config._

name := "site-theme"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

Laika / target := baseDirectory.value / "target" / "docs"

laikaTheme := Helium.defaults.site.addFontResources(
  FontDefinition(
    Font.withWebCSS("http://home.com/myFont.css"),
    "MyFont",
    FontWeight.Normal,
    FontStyle.Normal
  )
).build
