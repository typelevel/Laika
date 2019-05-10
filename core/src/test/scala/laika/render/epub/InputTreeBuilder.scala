package laika.render.epub

import com.typesafe.config.{Config, ConfigValueFactory}
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.io._

trait InputTreeBuilder extends ModelBuilder {

  val uuid = "some-uuid"

  def doc(path: Path, num: Int): RenderedDocument = RenderedDocument(path, Seq(Text(s"Title $num")), "zzz")

  def section(letter: Char) = Section(Header(1, Seq(Text(s"Section $letter")), Id(letter.toString)), Seq(p("zzz")))

  def rootElemWithSections(num: Int): Int = 8 //root(title(s"Title $num"), section('A'), section('B'))

  def configWithTreeTitle (num: Int): Config = com.typesafe.config.ConfigFactory.empty
    .withValue("title", ConfigValueFactory.fromAnyRef(s"Tree $num"))
  
  def titleSpans (text: String): Seq[Span] = Seq(Text(text))

  def rootTree (path: Path, titleNum: Int, docs: RenderContent*): RenderResult2 = {
    RenderResult2(None, tree(path, titleNum, docs: _*), TemplateRoot(Nil), com.typesafe.config.ConfigFactory.empty)
  }

  def tree (path: Path, titleNum: Int, docs: RenderContent*): RenderedTree = 
    RenderedTree(path, titleSpans(s"Tree $titleNum"), docs)

}

trait SingleDocument extends InputTreeBuilder {

  val docRef = doc(Path.Root / "foo", 2)

  val input = rootTree(Path.Root, 1, docRef)

}

trait TwoDocuments extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "bar", 3)

  val input = rootTree(Path.Root, 1, doc1, doc2)
}

trait DocumentPlusTitle extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "title", 2)
  val doc2 = doc(Path.Root / "bar", 3)

  val input = rootTree(Path.Root, 1, doc1, doc2)
}

trait DocumentPlusCover extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "cover", 2)
  val doc2 = doc(Path.Root / "bar", 3)
  val coverImage = CopiedDocument(ByteInput("", Root / "cover.png"))

  val input = rootTree(Path.Root, 1, doc1, doc2, coverImage)
}

trait DocumentPlusStyle extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val css = CopiedDocument(ByteInput("{}", Path.Root / "test-style.css"))

  val input = rootTree(Path.Root, 1, doc1, css)
}

trait NestedTree extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub" / "bar", 3)
  val subtree = rootTree(Path.Root / "sub", 4, doc2)

  val input = rootTree(Path.Root, 1, doc1, subtree.rootTree)
}

trait NestedTreeWithTitleDoc extends InputTreeBuilder {

  val titleDoc = doc(Path.Root / "sub" / "title", 0)
  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub" / "bar", 3)
  val subtree = tree(Path.Root / "sub", 4, doc2)

  val input = rootTree(Path.Root, 1, doc1, subtree.copy(content = titleDoc +: subtree.content))
}

trait TwoNestedTrees extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub1" / "bar", 3)
  val doc3 = doc(Path.Root / "sub1" / "baz", 4)
  val doc4 = doc(Path.Root / "sub2" / "bar", 5)
  val doc5 = doc(Path.Root / "sub2" / "baz", 6)
  val subtree1 = rootTree(Path.Root / "sub1", 2, doc2, doc3)
  val subtree2 = rootTree(Path.Root / "sub2", 3, doc4, doc5)

  val input = rootTree(Path.Root, 1, doc1, subtree1.rootTree, subtree2.rootTree)
}

trait TreeWithStaticDocuments extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub" / "bar", 3)
  val static1 = CopiedDocument(ByteInput("", Path("/sub/image.jpg")))
  val static2 = CopiedDocument(ByteInput("", Path("/sub/styles.css")))
  val unknown = CopiedDocument(ByteInput("", Path("/sub/doc.pdf")))
  val subtree = tree(Path.Root / "sub", 4, doc2, static1, static2, unknown)

  val input = rootTree(Path.Root, 1, doc1, subtree)
}

trait DocumentsWithSections extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", rootElemWithSections(2))
  val doc2 = doc(Path.Root / "bar", rootElemWithSections(3))

  val input = rootTree(Path.Root, 1, doc1, doc2)
}
