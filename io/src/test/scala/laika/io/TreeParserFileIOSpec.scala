/*
 * Copyright 2012-2021 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.io

import cats.effect.{Async, IO, Resource, Sync}
import laika.ast.DocumentType.Ignored
import laika.ast.{/, Document, DocumentTree, DocumentTreeRoot, Paragraph, Path, RootElement, TemplateDocument, TemplateRoot, TemplateString}
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.sample.{ParagraphCompanionShortcuts, SampleSixDocuments, SampleTrees}
import laika.bundle.BundleProvider
import laika.config.{ConfigBuilder, Origin}
import laika.config.Origin.TreeScope
import laika.io.api.TreeParser
import laika.io.helper.TestThemeBuilder
import laika.io.model.{InputTree, InputTreeBuilder}
import munit.CatsEffectSuite

import java.io.ByteArrayInputStream

class TreeParserFileIOSpec 
  extends CatsEffectSuite
  with ParserSetup
  with FileIO
  with ParagraphCompanionShortcuts
  with IOTreeAssertions {


  test("parse a directory using the fromDirectory method") {
    val dirname: String = getClass.getResource("/trees/a/").getFile

    val expected = SampleTrees.sixDocuments
      .docContent(key => Seq(p("Doc" + key.num)))
      .suffix("md")
      .build

    defaultParser.use(_.fromDirectory(dirname).parse).map(_.root).assertEquals(expected)
  }

  test("read a directory containing a file with non-ASCII characters") {
    val dirname: String = getClass.getResource("/trees/c/").getFile

    def doc (num: Int, path: Path = Root) = Document(path / s"doc-$num.md", RootElement(p(s"Doc$num äöü")))

    val treeResult = DocumentTreeRoot(DocumentTree(Root, List(doc(1))))
    defaultParser.use(_.fromDirectory(dirname).parse).map(_.root).assertEquals(treeResult)
  }

  test("read a directory using a custom document type matcher") {
    val dirname: String = getClass.getResource("/trees/a/").getFile

    val baseTree = SampleTrees.sixDocuments
      .docContent(key => Seq(p("Doc" + key.num)))
      .suffix("md")
      .build

    val expected = baseTree.copy(
      tree = baseTree.tree.copy(
        content = baseTree.tree.content.filter(_.path.name != "doc-1.md")
      )
    )

    val parser = parserWithBundle(BundleProvider.forDocTypeMatcher { case Root / "doc-1.md" => Ignored })
    parser
      .use(_.fromDirectory(dirname).parse)
      .map(_.root)
      .assertEquals(expected)
  }

  test("read a directory using a custom exclude filter") {
    val dirname: String = getClass.getResource("/trees/a/").getFile

    val baseTree = SampleTrees.sixDocuments
      .docContent(key => Seq(p("Doc" + key.num)))
      .suffix("md")
      .build

    val expected = baseTree.copy(
      tree = baseTree.tree.copy(
        content = baseTree.tree.content.filter(c => c.path.name != "doc-1.md" && c.path.name != "tree-1")
      )
    )

    defaultParser
      .use(_.fromDirectory(dirname, { (f: java.io.File) => f.getName == "doc-1.md" || f.getName == "tree-1" }).parse)
      .map(_.root)
      .assertEquals(expected)
  }



  trait CustomInputSetup {

    val dirname: String = getClass.getResource("/trees/a/").getFile
    
    def expected (customizeSample: SampleSixDocuments => SampleSixDocuments = identity,
                  customizeTree: DocumentTreeRoot => DocumentTreeRoot = identity): DocumentTreeRoot = 
      customizeTree(SampleTrees.sixDocuments
        .docContent(key => Seq(p("Doc" + key.num)))
        .suffix("md")
        .apply(customizeSample)
        .build)

    protected def runWith (parser: Resource[IO, TreeParser[IO]],
                           input: InputTreeBuilder[IO],
                           expected: DocumentTreeRoot,
                           extraCheck: DocumentTreeRoot => Unit = _ => ()): IO[Unit] = {
      parser
        .use(_.fromInput(input).parse)
        .map(_.root)
        .flatTap { root =>
          IO(extraCheck(root))
        }
        .assertEquals(expected)
    }
  }

  object CustomInput extends CustomInputSetup {
    lazy val parser: Resource[IO, TreeParser[IO]] = defaultBuilder.build

    def run (addDoc: InputTreeBuilder[IO] => InputTreeBuilder[IO],
             expected: DocumentTreeRoot,
             extraCheck: DocumentTreeRoot => Unit = _ => ()): IO[Unit] = {
      val input = addDoc(InputTree[IO].addDirectory(dirname))
      runWith(parser, input, expected, extraCheck)
    }
  }

  object CustomTheme extends CustomInputSetup {
    lazy val input: InputTreeBuilder[IO] = InputTree[IO].addDirectory(dirname)
    
    trait Builder {
      def addDoc[F[_]: Sync] (builder: InputTreeBuilder[F]): InputTreeBuilder[F]
    }
    
    def run (builder: Builder,
             expected: DocumentTreeRoot,
             themeExtension: Option[Builder] = None,
             extraCheck: DocumentTreeRoot => Unit = _ => ()): IO[Unit] = {
      
      val themeInputs: TestThemeBuilder.Inputs = new TestThemeBuilder.Inputs {
        def build[G[_]: Async] = builder.addDoc(InputTree[G])
      }
      val baseTheme = TestThemeBuilder.forInputs(themeInputs)
      val theme = themeExtension.fold(baseTheme) { extBuilder =>
        val themeExtInputs: TestThemeBuilder.Inputs = new TestThemeBuilder.Inputs {
          def build[G[_]: Async] = extBuilder.addDoc(InputTree[G])
        }
        baseTheme.extendWith(TestThemeBuilder.forInputs(themeExtInputs))
      }
      val parser = defaultBuilder.withTheme(theme).build
      runWith(parser, input, expected, extraCheck)
    }
  }

  object ExtraDoc extends CustomInputSetup {
    val path: Path = Root / "tree-2" / "doc-7.md"
    val extraDoc: Document = Document(path, RootElement(p("Doc7")))
    def customizeTree (sample: DocumentTreeRoot): DocumentTreeRoot = sample.copy(
      tree = sample.tree.copy(
        content = sample.tree.content.map {
          case tree: DocumentTree if tree.path.name == "tree-2" => tree.appendContent(extraDoc)
          case other => other
        }
      )
    )
    lazy val expected: DocumentTreeRoot = expected(customizeTree = customizeTree)
  }

  object ExtraTemplate extends CustomInputSetup {
    val path: Path = Root / "tree-2" / "tmpl.template.html"
    
    lazy val expected: DocumentTreeRoot = expected(
      customizeSample = _.tree2.template(path.name, TemplateString("Template"))
    )
  }

  object ExtraConfig extends CustomInputSetup {
    val path: Path = Root / "tree-2" / "directory.conf"

    def checkConfig(root: DocumentTreeRoot): Unit = { 
      val actual = root
        .tree
        .selectSubtree(CurrentTree / "tree-2")
        .flatMap(_.config.get[Int]("foo").toOption)
      assertEquals(actual, Some(7))
    }
  }

  test("read a directory from the file system plus one AST input") {
    CustomInput.run(
      _.addDocument(Document(ExtraDoc.path, RootElement(Paragraph("Doc7")))), 
      ExtraDoc.expected
    )
  }

  test("read a directory from the file system plus one AST input from a theme") {
    object Builder extends CustomTheme.Builder {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] =
        input.addDocument(Document(ExtraDoc.path, RootElement(Paragraph("Doc7"))))
    }
    CustomTheme.run(Builder, ExtraDoc.expected)
  }

  test("read a directory from the file system plus one AST input from a theme extension overriding a theme input") {
    object Builder extends CustomTheme.Builder {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] =
        input.addDocument(Document(ExtraDoc.path, RootElement(Paragraph("Doc99"))))
    }
    object ExtBuilder extends CustomTheme.Builder {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] =
        input.addDocument(Document(ExtraDoc.path, RootElement(Paragraph("Doc7"))))
    }
    CustomTheme.run(Builder, ExtraDoc.expected, themeExtension = Some(ExtBuilder))
  }

  test("read a directory from the file system plus one string input") {
    CustomInput.run(
      _.addString("Doc7", ExtraDoc.path),
      ExtraDoc.expected
    )
  }

  test("read a directory from the file system plus one document from an input stream") {
    CustomInput.run(
      _.addStream(IO.delay(new ByteArrayInputStream("Doc7".getBytes)), ExtraDoc.path),
      ExtraDoc.expected
    )
  }

  test("read a directory from the file system plus one extra file") {
    lazy val filename: String = getClass.getResource("/trees/d/doc-7.md").getFile

    CustomInput.run(
      _.addFile(filename, ExtraDoc.path),
      ExtraDoc.expected
    )
  }

  test("read a directory from the file system plus one extra template from a string") {
    CustomInput.run(
      _.addString("Template", ExtraTemplate.path),
      ExtraTemplate.expected
    )
  }

  test("read a directory from the file system plus one extra template from a string in a theme") {
    object Builder extends CustomTheme.Builder {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] =
        input.addString("Template", ExtraTemplate.path)
    }
    CustomTheme.run(Builder, ExtraTemplate.expected)
  }

  test("read a directory from the file system plus one extra template from an AST") {
    CustomInput.run(
      _.addTemplate(TemplateDocument(ExtraTemplate.path, TemplateRoot(TemplateString("Template")))),
      ExtraTemplate.expected
    )
  }

  test("read a directory from the file system plus one extra config document from a string") {
    CustomInput.run(
      _.addString("foo = 7", ExtraConfig.path),
      ExtraConfig.expected(),
      ExtraConfig.checkConfig
    )
  }

  test("read a directory from the file system plus one extra config document from a string in a theme") {
    object Builder extends CustomTheme.Builder {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] =
        input.addString("foo = 7", ExtraConfig.path)
    }
    CustomTheme.run(Builder, ExtraConfig.expected(), extraCheck = ExtraConfig.checkConfig(_))
  }

  test("read a directory from the file system plus one extra config document built programmatically") {
    val config = ConfigBuilder
      .withOrigin(Origin(TreeScope, ExtraConfig.path))
      .withValue("foo", 7).build
    
    CustomInput.run(
      _.addConfig(config, ExtraConfig.path),
      ExtraConfig.expected(),
      ExtraConfig.checkConfig
    )
  }
  

  trait DirectorySetup {
    val dir1 = new java.io.File(getClass.getResource("/trees/a/").getFile)
    val dir2 = new java.io.File(getClass.getResource("/trees/b/").getFile)

    val baseTree: DocumentTreeRoot = SampleTrees.sixDocuments
      .docContent(key => Seq(p("Doc" + key.num)))
      .suffix("md")
      .build
  }

  object TopLevelMergeSetup extends DirectorySetup {
    private val doc7 = Document(Root / "tree-2" / "doc-7.md", RootElement("Doc7"))
    private val doc8 = Document(Root / "tree-3" / "doc-8.md", RootElement("Doc8"))
    private val doc9 = Document(Root / "doc-9.md", RootElement("Doc9"))

    private val tree2 = baseTree.tree.content(3).asInstanceOf[DocumentTree]

    val expected: DocumentTreeRoot = baseTree.modifyTree(tree =>
      tree.copy(content =
        tree.content.take(2) :+
          doc9 :+
          tree.content(2) :+
          tree2.appendContent(doc7) :+
          DocumentTree(Root / "tree-3", Seq(doc8))
      )
    )
  }
  
  object MergedDirectorySetup extends DirectorySetup

  test("merge two directories from the file system using the fromDirectories method") {
    import TopLevelMergeSetup._
    defaultParser.use(_.fromDirectories(Seq(dir1, dir2)).parse).map(_.root).assertEquals(expected)
  }

  test("merge two directories from the file system using an InputTreeBuilder") {
    import TopLevelMergeSetup._
    
    val treeInput = InputTree[IO].addDirectory(dir1).addDirectory(dir2)

    defaultParser.use(_.fromInput(treeInput).parse).map(_.root).assertEquals(expected)
  }

  test("merge a directory at a specific mount-point using an InputTreeBuilder") {
    import MergedDirectorySetup._
    
    val treeInput = InputTree[IO].addDirectory(dir1).addDirectory(dir2, Root / "tree-1")

    val doc7 = Document(Root / "tree-1" / "tree-2" / "doc-7.md", RootElement("Doc7"))
    val doc8 = Document(Root / "tree-1" / "tree-3" / "doc-8.md", RootElement("Doc8"))
    val doc9 = Document(Root / "tree-1" / "doc-9.md", RootElement("Doc9"))

    val tree1 = baseTree.tree.content(2).asInstanceOf[DocumentTree]
    val tree1Merged = tree1.copy(
      content = tree1.content :+
        doc9 :+
        DocumentTree(Root / "tree-1" / "tree-2", Seq(doc7)) :+
        DocumentTree(Root / "tree-1" / "tree-3", Seq(doc8))
    )

    val expected = baseTree.copy(
      tree = baseTree.tree.copy(
        content =
          baseTree.tree.content.take(2) :+
            tree1Merged :+
            baseTree.tree.content.drop(3).head
      )
    )
    defaultParser.use(_.fromInput(treeInput).parse).map(_.root).assertEquals(expected)
  }
  
}
