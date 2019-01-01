package laika.render.epub

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.io.Input

trait InputTreeBuilder extends ModelBuilder {

  val uuid = "some-uuid"

  def rootElem(num: Int) = root(title(s"Title $num"), p("zzz"))

  def section(letter: Char) = Section(Header(1, Seq(Text(s"Section $letter")), Id(letter.toString)), Seq(p("zzz")))

  def rootElemWithSections(num: Int) = root(title(s"Title $num"), section('A'), section('B'))

  def configWithTreeTitle (num: Int): Config = ConfigFactory.empty
    .withValue("title", ConfigValueFactory.fromAnyRef(s"Tree $num"))

  def tree (path: Path, titleNum: Int, docs: TreeContent*): DocumentTree =
    DocumentTree(path, docs, config = configWithTreeTitle(titleNum))

}

trait SingleDocument extends InputTreeBuilder {

  val doc = Document(Path.Root / "foo", rootElem(2))

  val input = tree(Path.Root, 1, doc)

}

trait TwoDocuments extends InputTreeBuilder {

  val doc1 = Document(Path.Root / "foo", rootElem(2))
  val doc2 = Document(Path.Root / "bar", rootElem(3))

  val input = tree(Path.Root, 1, doc1, doc2)
}

trait NestedTree extends InputTreeBuilder {

  val doc1 = Document(Path.Root / "foo", rootElem(2))
  val doc2 = Document(Path.Root / "sub" / "bar", rootElem(3))
  val subtree = tree(Path.Root / "sub", 4, doc2)

  val input = tree(Path.Root, 1, doc1, subtree)
}

trait TwoNestedTrees extends InputTreeBuilder {

  val doc1 = Document(Path.Root / "foo", rootElem(2))
  val doc2 = Document(Path.Root / "sub1" / "bar", rootElem(3))
  val doc3 = Document(Path.Root / "sub1" / "baz", rootElem(4))
  val doc4 = Document(Path.Root / "sub2" / "bar", rootElem(5))
  val doc5 = Document(Path.Root / "sub2" / "baz", rootElem(6))
  val subtree1 = tree(Path.Root / "sub1", 2, doc2, doc3)
  val subtree2 = tree(Path.Root / "sub2", 3, doc4, doc5)

  val input = tree(Path.Root, 1, doc1, subtree1, subtree2)
}

trait TreeWithStaticDocuments extends InputTreeBuilder {

  val doc1 = Document(Path.Root / "foo", rootElem(2))
  val doc2 = Document(Path.Root / "sub" / "bar", rootElem(3))
  val static1 = StaticDocument(Input.fromString("", Path("/sub/image.jpg")))
  val static2 = StaticDocument(Input.fromString("", Path("/sub/styles.css")))
  val unknown = StaticDocument(Input.fromString("", Path("/sub/doc.pdf")))
  val subtree = tree(Path.Root / "sub", 4, doc2).copy(additionalContent = Seq(static1, static2, unknown))

  val input = tree(Path.Root, 1, doc1, subtree)
}

trait DocumentsWithSections extends InputTreeBuilder {

  val doc1 = Document(Path.Root / "foo", rootElemWithSections(2))
  val doc2 = Document(Path.Root / "bar", rootElemWithSections(3))

  val input = tree(Path.Root, 1, doc1, doc2)
}
