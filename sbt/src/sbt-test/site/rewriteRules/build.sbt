import laika.ast.Emphasized
import laika.ast.Strong
import laika.ast.RewriteAction.Replace

name := "site-rewriteRules"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaExtensions += laikaSpanRewriteRule { case Emphasized(content, _) =>
  Replace(Strong(content))
}
