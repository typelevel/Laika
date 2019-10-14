package laika.render.epub

import java.io.File

import laika.api.config.{Config, ConfigBuilder}
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.io.model._
import laika.io.helper.InputBuilder

trait InputTreeBuilder extends ModelBuilder with InputBuilder {

  val uuid = "some-uuid"

  def doc(path: Path, num: Int, sections: Seq[SectionInfo] = Nil): RenderedDocument = RenderedDocument(path.withSuffix("xhtml"), Seq(Text(s"Title $num")), sections, "zzz")

  def section(letter: Char) = SectionInfo(letter.toString, TitleInfo(Seq(Text(s"Section $letter"))), Nil)

  def sections: Seq[SectionInfo] = Seq(section('A'), section('B'))

  def configWithTreeTitle (num: Int): Config = ConfigBuilder.empty.withValue("title", s"Tree $num").build
  
  def titleSpans (text: String): Seq[Span] = Seq(Text(text))

  def rootTree (path: Path, titleNum: Int, docs: RenderContent*): RenderedTreeRoot = {
    RenderedTreeRoot(tree(path, titleNum, docs: _*), TemplateRoot(Nil), Config.empty)
  }

  def tree (path: Path, titleNum: Int, docs: RenderContent*): RenderedTree = {
    val titleDoc = docs.collectFirst { case doc: RenderedDocument if doc.path.basename == "title" => doc }
    val content = docs.filterNot(_.path.basename == "title")
    val title = titleDoc.fold(titleSpans(s"Tree $titleNum")) { doc => titleSpans(s"From TitleDoc") }
    RenderedTree(path, title, content, titleDoc)
  }

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

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "bar", 3)
  val cover = doc(Path.Root / "cover", 0)

  val input = rootTree(Path.Root, 1, doc1, doc2).copy(
    coverDocument = Some(cover), 
    staticDocuments = Seq(BinaryFileInput(new File("cover.png"), Root / "cover.png"))
  )
}

trait DocumentPlusStyle extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val css = ByteInput("{}", Path.Root / "test-style.css")

  val input = rootTree(Path.Root, 1, doc1).copy(staticDocuments = Seq(css))
}

trait NestedTree extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub" / "bar", 3)
  val subtree = rootTree(Path.Root / "sub", 4, doc2)

  val input = rootTree(Path.Root, 1, doc1, subtree.tree)
}

trait NestedTreeWithTitleDoc extends InputTreeBuilder {

  val titleDoc = doc(Path.Root / "sub" / "title", 0)
  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub" / "bar", 3)
  val subtree = tree(Path.Root / "sub", 4, doc2, titleDoc)

  val input = rootTree(Path.Root, 1, doc1, subtree)
}

trait TwoNestedTrees extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub1" / "bar", 3)
  val doc3 = doc(Path.Root / "sub1" / "baz", 4)
  val doc4 = doc(Path.Root / "sub2" / "bar", 5)
  val doc5 = doc(Path.Root / "sub2" / "baz", 6)
  val subtree1 = rootTree(Path.Root / "sub1", 2, doc2, doc3)
  val subtree2 = rootTree(Path.Root / "sub2", 3, doc4, doc5)

  val input = rootTree(Path.Root, 1, doc1, subtree1.tree, subtree2.tree)
}

trait TreeWithStaticDocuments extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "sub" / "bar", 3)
  val static1 = ByteInput("", Path("/sub/image.jpg"))
  val static2 = ByteInput("", Path("/sub/styles.css"))
  val unknown = ByteInput("", Path("/sub/doc.pdf"))
  val subtree = tree(Path.Root / "sub", 4, doc2)

  val input = rootTree(Path.Root, 1, doc1, subtree).copy(staticDocuments = Seq(static1, static2, unknown))
}

trait DocumentsWithSections extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2, sections)
  val doc2 = doc(Path.Root / "bar", 3, sections)

  val input = rootTree(Path.Root, 1, doc1, doc2)
}
