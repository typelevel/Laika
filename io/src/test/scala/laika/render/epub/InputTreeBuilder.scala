/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.render.epub

import cats.effect.IO
import laika.config.{ Config, ConfigBuilder, LaikaKeys }
import laika.ast.Path.Root
import laika.ast._
import laika.format.EPUB.ScriptedTemplate
import laika.io.model._
import laika.io.helper.InputBuilder
import laika.rewrite.OutputContext
import laika.rewrite.nav.BasicPathTranslator

trait InputTreeBuilder extends InputBuilder {

  val uuid = "some-uuid"

  def doc(path: Path, title: String): RenderedDocument =
    RenderedDocument(
      path.withSuffix("xhtml"),
      Some(SpanSequence(Text(title))),
      Nil,
      "zzz",
      Config.empty
    )

  def doc(
      path: Path,
      num: Int,
      sections: Seq[SectionInfo] = Nil,
      config: Config = Config.empty
  ): RenderedDocument =
    RenderedDocument(
      path.withSuffix("xhtml"),
      Some(SpanSequence(Text(s"Title $num"))),
      sections,
      "zzz",
      config
    )

  def section(letter: Char): SectionInfo =
    SectionInfo(letter.toString, SpanSequence(s"Section $letter"), Nil)

  def sections: Seq[SectionInfo] = Seq(section('A'), section('B'))

  def configWithTreeTitle(num: Int): Config =
    ConfigBuilder.empty.withValue(LaikaKeys.title, s"Tree $num").build

  def rootTree(path: Path, titleNum: Int, docs: RenderContent*): RenderedTreeRoot[IO] = {
    val outputContext  = OutputContext("ignored")
    val pathTranslator = BasicPathTranslator(outputContext.fileSuffix)
    RenderedTreeRoot(
      tree(path, titleNum, docs: _*),
      TemplateRoot.empty,
      Config.empty,
      outputContext,
      pathTranslator
    )
  }

  def tree(path: Path, titleNum: Int, docs: RenderContent*): RenderedTree = {
    val titleDoc = docs.collectFirst {
      case doc: RenderedDocument if doc.path.basename == "title" => doc
    }
    val content  = docs.filterNot(_.path.basename == "title")
    val title    = titleDoc.fold(SpanSequence(s"Tree $titleNum")) { _ =>
      SpanSequence(s"From TitleDoc")
    }
    RenderedTree(path, Some(title), content, titleDoc)
  }

}

object EmptyTree extends InputTreeBuilder {

  val input = rootTree(Path.Root, 1)

}

object SingleDocument extends InputTreeBuilder {

  val docRef = doc(Path.Root / "foo", 2)

  val input = rootTree(Path.Root, 1, docRef)

}

object DocumentNameStartingWithDigit extends InputTreeBuilder {

  val docRef = doc(Path.Root / "01-foo", 2)

  val input = rootTree(Path.Root, 1, docRef)

}

object DocumentWithSpecialChars extends InputTreeBuilder {

  val docRef = doc(Path.Root / "foo", "This & That")

  val input = rootTree(Path.Root, 1, docRef)

}

object TwoDocuments extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val doc2 = doc(Path.Root / "bar", 3)

  val input = rootTree(Path.Root, 1, doc1, doc2)
}

object DocumentPlusTitle extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "title", 2)
  val doc2 = doc(Path.Root / "bar", 3)

  val input = rootTree(Path.Root, 1, doc1, doc2)
}

object DocumentPlusCover extends InputTreeBuilder {

  val doc1  = doc(Path.Root / "foo", 2)
  val doc2  = doc(Path.Root / "bar", 3)
  val cover = doc(Path.Root / "cover", 0)

  val input = rootTree(Path.Root, 1, doc1, doc2).copy[IO](
    coverDocument = Some(cover),
    staticDocuments = Seq(ByteInput("", Root / "cover.png"))
  )

}

object DocumentPlusStyle extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2)
  val css  = ByteInput("{}", Path.Root / "test-style.css")

  val input = rootTree(Path.Root, 1, doc1).copy[IO](staticDocuments = Seq(css))
}

object NestedTree extends InputTreeBuilder {

  val doc1    = doc(Path.Root / "foo", 2)
  val doc2    = doc(Path.Root / "sub" / "bar", 3)
  val subtree = rootTree(Path.Root / "sub", 4, doc2)

  val input = rootTree(Path.Root, 1, doc1, subtree.tree)
}

object NestedTreeWithTitleDoc extends InputTreeBuilder {

  val titleDoc = doc(Path.Root / "sub" / "title", 0)
  val doc1     = doc(Path.Root / "foo", 2)
  val doc2     = doc(Path.Root / "sub" / "bar", 3)
  val subtree  = tree(Path.Root / "sub", 4, doc2, titleDoc)

  val input = rootTree(Path.Root, 1, doc1, subtree)
}

object TwoNestedTrees extends InputTreeBuilder {

  val doc1     = doc(Path.Root / "foo", 2)
  val doc2     = doc(Path.Root / "sub1" / "bar", 3)
  val doc3     = doc(Path.Root / "sub1" / "baz", 4)
  val doc4     = doc(Path.Root / "sub2" / "bar", 5)
  val doc5     = doc(Path.Root / "sub2" / "baz", 6)
  val subtree1 = rootTree(Path.Root / "sub1", 2, doc2, doc3)
  val subtree2 = rootTree(Path.Root / "sub2", 3, doc4, doc5)

  val input = rootTree(Path.Root, 1, doc1, subtree1.tree, subtree2.tree)
}

object TreeWithStaticDocuments extends InputTreeBuilder {

  val doc1    = doc(Path.Root / "foo", 2)
  val doc2    = doc(Path.Root / "sub" / "bar", 3)
  val static1 = ByteInput("", Path.parse("/sub/image-1.5x.jpg"))
  val static2 = ByteInput("", Path.parse("/sub/styles.epub.css"))
  val unknown = ByteInput("", Path.parse("/sub/doc.pdf"))
  val subtree = tree(Path.Root / "sub", 4, doc2)

  val input =
    rootTree(Path.Root, 1, doc1, subtree).copy[IO](staticDocuments = Seq(static1, static2, unknown))

}

object DocumentsWithSections extends InputTreeBuilder {

  val doc1 = doc(Path.Root / "foo", 2, sections)
  val doc2 = doc(Path.Root / "bar", 3, sections)

  val input = rootTree(Path.Root, 1, doc1, doc2)
}
