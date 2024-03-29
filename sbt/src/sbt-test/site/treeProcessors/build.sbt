import cats.effect.IO
import laika.ast.Path.Root
import laika.ast._
import laika.format.{ HTML, PDF }
import laika.theme.TreeProcessorBuilder

name := "site-sourceDirectories"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

val addedAST = RootElement(
  Title("Title 3").withId("title").withStyle("title"),
  Paragraph(
    Text("Hello "),
    Emphasized("World 3"),
    Text(".")
  )
)

val removeDoc1 = TreeProcessorBuilder[IO].mapTree { tree =>
  tree.modifyTree(_.removeContent(_ == Root / "hello1.md"))
}

val removeDoc2 = TreeProcessorBuilder[IO].mapTree { tree =>
  tree.modifyTree(_.removeContent(_ == Root / "hello2.md"))
}

val addDoc3 = TreeProcessorBuilder[IO].mapTree { tree =>
  tree.modifyTree(_.prependContent(Document(Root / "hello3.md", addedAST)))
}

laikaTreeProcessors ++= Seq(
  LaikaTreeProcessor(removeDoc1, OutputContext(HTML)),
  LaikaTreeProcessor(removeDoc2, OutputContext(PDF)),
  LaikaTreeProcessor(addDoc3, OutputContext(HTML))
)
