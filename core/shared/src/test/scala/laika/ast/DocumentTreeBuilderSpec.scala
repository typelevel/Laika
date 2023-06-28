package laika.ast

import laika.ast.Path.Root
import laika.config.Origin.TreeScope
import laika.config.{ ConfigBuilder, Origin }
import munit.FunSuite

class DocumentTreeBuilderSpec extends FunSuite {

  test("empty tree") {
    assertEquals(DocumentTree.builder.build, DocumentTree.empty)
  }

  test("tree with documents in root and sub-trees") {
    val doc1     = Document(Root / "doc-1.md", RootElement.empty)
    val doc2     = Document(Root / "doc-2.md", RootElement.empty)
    val doc3     = Document(Root / "tree" / "doc-3.md", RootElement.empty)
    val doc4     = Document(Root / "tree" / "doc-4.md", RootElement.empty)
    val tree     = DocumentTree.builder
      .addDocuments(List(doc1, doc2, doc3, doc4))
      .build
    val expected = DocumentTree(
      Root,
      Seq(
        doc1,
        doc2,
        DocumentTree(Root / "tree", Seq(doc3, doc4))
      )
    )
    assertEquals(tree, expected)
  }

  test("tree with documents and templates") {
    val doc1      = Document(Root / "doc-1.md", RootElement.empty)
    val doc2      = Document(Root / "tree" / "doc-2.md", RootElement.empty)
    val template1 = TemplateDocument(Root / "tpl-1.template.html", TemplateRoot.empty)
    val template2 = TemplateDocument(Root / "tree" / "tpl-2.template.html", TemplateRoot.empty)
    val tree      = DocumentTree.builder
      .addDocument(doc1)
      .addDocument(doc2)
      .addTemplate(template1)
      .addTemplate(template2)
      .build
    val expected  = DocumentTree(
      Root,
      Seq(
        doc1,
        DocumentTree(Root / "tree", Seq(doc2), templates = Seq(template2))
      ),
      templates = Seq(template1)
    )
    assertEquals(tree, expected)
  }

  test("tree with title documents") {
    val doc1     = Document(Root / "doc-1.md", RootElement.empty)
    val doc2     = Document(Root / "tree" / "doc-2.md", RootElement.empty)
    val title1   = Document(Root / "README.md", RootElement.empty)
    val title2   = Document(Root / "tree" / "README.md", RootElement.empty)
    val tree     = DocumentTree.builder
      .addDocument(doc1)
      .addDocument(doc2)
      .addDocument(title1)
      .addDocument(title2)
      .build
    val expected = DocumentTree(
      Root,
      Seq(
        doc1,
        DocumentTree(Root / "tree", Seq(doc2), titleDocument = Some(title2))
      ),
      titleDocument = Some(title1)
    )
    assertEquals(tree, expected)
  }

  test("root tree with cover document") {
    val doc      = Document(Root / "doc", RootElement.empty)
    val cover    = Document(Root / "cover", RootElement.empty)
    val tree     = DocumentTree.builder
      .addDocument(doc)
      .addDocument(cover)
      .buildRoot
    val expected = DocumentTreeRoot(
      tree = DocumentTree(Root, Seq(doc)),
      coverDocument = Some(cover)
    )
    assertEquals(tree, expected)
  }

  test("document config inherits from tree config") {
    val docPath    = Root / "tree" / "doc"
    val docConfig  = ConfigBuilder.empty.withValue("foo.bar", 7).build
    val treeOrigin = Origin(TreeScope, Root / "tree" / "directory.conf")
    val treeConfig =
      ConfigBuilder.withOrigin(treeOrigin).withValue("foo.baz", 9).build
    val doc        = Document(docPath, RootElement.empty, config = docConfig)
    val tree       = DocumentTree.builder
      .addDocument(doc)
      .addConfig(treeConfig)
      .build
    val configs    = tree.selectDocument(docPath.relative).map { doc =>
      (doc.config.get[Int]("foo.bar").getOrElse(0), doc.config.get[Int]("foo.baz").getOrElse(0))
    }
    assertEquals(configs, Some((7, 9)))
  }

  test("document config inherits from base config") {
    val docPath    = Root / "tree" / "doc"
    val docConfig  = ConfigBuilder.empty.withValue("foo.bar", 7).build
    val baseConfig = ConfigBuilder.empty.withValue("foo.baz", 9).build
    val doc        = Document(docPath, RootElement.empty, config = docConfig)
    val tree       = DocumentTree.builder
      .addDocument(doc)
      .build(baseConfig)
    val configs    = tree.selectDocument(docPath.relative).map { doc =>
      (doc.config.get[Int]("foo.bar").getOrElse(0), doc.config.get[Int]("foo.baz").getOrElse(0))
    }
    assertEquals(configs, Some((7, 9)))
  }

  test("documents override existing instances with the same path") {
    val docA     = Document(Root / "doc-1", RootElement(Paragraph("AAA")))
    val docB     = Document(Root / "doc-2", RootElement(Paragraph("BBB")))
    val docC     = Document(Root / "doc-1", RootElement(Paragraph("CCC")))
    val docs     = DocumentTree.builder
      .addDocument(docA)
      .addDocument(docB)
      .addDocument(docC)
      .build
      .content
      .sortBy(_.path.name)
    val expected = Seq(docC, docB)
    assertEquals(docs, expected)
  }

}
