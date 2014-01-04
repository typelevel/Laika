import LaikaKeys._
import laika.tree.Elements.Warning

name := "site-renderMessageLevel"

version := "0.1"

scalaVersion := "2.10.3"

LaikaPlugin.defaults

renderMessageLevel in Laika := Some(Warning)