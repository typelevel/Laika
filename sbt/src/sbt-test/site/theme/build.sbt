import laika.helium._
import laika.theme._

name := "site-theme"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaTheme := Helium.defaults.copy(fontResources = Helium.defaults.fontResources :+
  FontDefinition(Font.webCSS("http://home.com/myFont.css"), "MyFont", FontWeight.Normal, FontStyle.Normal)).build

