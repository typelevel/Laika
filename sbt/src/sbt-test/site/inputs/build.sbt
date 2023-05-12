import cats.effect.IO
import laika.ast.Path.Root
import laika.io.model.InputTree

name := "site-inputs"

version := "0.1"

scalaVersion := "2.12.6"

laikaInputs := InputTree[IO]
  .addFile(baseDirectory.value / "src/docs/hello.md", Root / "doc-1.md")
  .addFile(baseDirectory.value / "src/docs/default.template.html", Root / "default.template.html")
  .addString(
    """# Title
      |
      |Hello *World*.""".stripMargin,
    Root / "doc-2.md"
  )

enablePlugins(LaikaPlugin)
