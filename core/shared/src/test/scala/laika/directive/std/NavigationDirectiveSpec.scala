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

package laika.directive.std

import cats.data.NonEmptySet
import laika.ast.Path.Root
import laika.ast.helper.ModelBuilder
import laika.ast.sample.{BuilderKey, SampleContent}
import laika.ast._
import laika.rewrite.nav.TargetFormats
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NavigationDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder {

  trait NavModel {

    def hasTitleDocs: Boolean

    def maxLevels: Int = Int.MaxValue
    def excludeSections: Boolean = false

    def itemStyles: Options = NoOpt

    val refPath: Path = Root / "tree-2" / "doc-6"

    def styles (level: Int): Options = Style.level(level) + itemStyles

    def sectionList (path: Path, section: Int, level: Int): NavigationItem = NavigationItem(
      SpanSequence(s"Section $section"),
      if (section % 2 == 0 || level == maxLevels) Nil else Seq(
        sectionList(path, section + 1, level+1)
      ),
      Some(NavigationLink(InternalTarget(path.withFragment(s"section-$section")).relativeTo(refPath))),
      options = styles(level)
    )

    def docList (path: Path, doc: Int, level: Int, title: Option[String] = None): NavigationItem = NavigationItem(
      SpanSequence(title.getOrElse(s"Title $doc")),
      if (level == maxLevels || excludeSections) Nil else Seq(
        sectionList(path, 1, level+1),
        sectionList(path, 3, level+1)
      ),
      Some(NavigationLink(InternalTarget(path).relativeTo(refPath), path == refPath)),
      TargetFormats.All,
      styles(level)
    )

    def treeList (tree: Int, docStartNum: Int, level: Int, excludeSelf: Boolean = false): NavigationItem = {
      val children = if (level == maxLevels) Nil else List(
        docList(Root / s"tree-$tree" / s"doc-$docStartNum", docStartNum, level + 1),
        docList(Root / s"tree-$tree" / s"doc-${docStartNum + 1}", docStartNum + 1, level + 1),
      )
      if (hasTitleDocs) NavigationItem(
        SpanSequence(s"Tree $tree"),
        children,
        Some(NavigationLink(InternalTarget(Root / s"tree-$tree" / "README").relativeTo(refPath))),
        options = styles(level)
      )
      else NavigationItem(
        SpanSequence(s"Tree $tree"),
        if (excludeSelf) children.take(1) else children,
        options = styles(level)
      )
    }

    val rootEntry: NavigationItem = NavigationItem(SpanSequence("/"), Nil, None, TargetFormats.All, styles(1))

    def rootList: NavigationItem =
      NavigationItem(SpanSequence("/"), List(
        docList(Root / "doc-1", 1, 2),
        docList(Root / "doc-2", 2, 2),
        treeList(1, 3, 2),
        treeList(2, 5, 2)
      ), options = styles(1))

    def templateResult (items: NavigationItem*): RootElement = buildResult(NavigationList(items, itemStyles))

    def error (msg: String, fragment: String, input: String): RootElement = {
      buildResult(InvalidSpan(msg, source(fragment, input)))
    }

    private def buildResult (element: Element): RootElement = {
      root(TemplateRoot(
        t("aaa "),
        TemplateElement(element),
        t(" bbb "),
        EmbeddedRoot(SampleContent.fourSections(BuilderKey.Doc(6)))
      ))
    }

    def blockResult (items: NavigationItem*): RootElement = root(
      Title("Title 6").withOptions(Id("title-6") + Style.title),
      p("aaa"),
      NavigationList(items, itemStyles),
      p("bbb")
    )

    def extLink (num: Int): NavigationItem = NavigationItem(
      SpanSequence(s"Link $num"),
      Nil,
      Some(NavigationLink(ExternalTarget(s"http://domain-$num.com/")))
    )
  }

  "The template nav directive" should "produce two manual entries" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Link 2, target = "http://domain-2.com/"} 
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(extLink(1), extLink(2)))
  }

  it should "produce a manual entry and a generated entry" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(extLink(1), docList(Root / "tree-2" / "doc-6", 6, 1)))
  }

  it should "produce an entry generated from the root of the tree" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(rootList))
  }

  it should "produce an entry generated from the root of the tree with documents filtered by target format" in new RewriteSetup with NavModel {

    override def includeTargetFormatConfig = true

    val filteredRootList: NavigationItem =
      NavigationItem(SpanSequence("/"), List(
        docList(Root / "doc-1", 1, 2),
        NavigationItem(SpanSequence("Tree 1"),
          List(
            docList(Root / s"tree-1" / s"doc-3", 3, 3).copy(targetFormats = TargetFormats.Selected(NonEmptySet.of("html", "txt")))
          ), options = styles(2)),
        treeList(2, 5, 2)
      ), options = styles(1))

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(filteredRootList))
  }

  it should "produce an entry generated from the root of the tree with title documents" in new RewriteSetup with NavModel {

    override val hasTitleDocs: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(rootList))
  }

  it should "produce an entry generated from the current tree" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with a maximum depth" in new RewriteSetup with NavModel {

    override def maxLevels: Int = 2

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", depth = 2 }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with the root excluded" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeRoot = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 0).content: _*))
  }

  it should "produce an entry generated from the current tree with the self link excluded" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  excludeSelf = true
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1, excludeSelf = true)))
  }

  it should "produce an entry generated from the current tree with sections excluded" in new RewriteSetup with NavModel {

    override def excludeSections: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeSections = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current document" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(docList(Root / "tree-2" / "doc-6", 6, 1)))
  }

  it should "produce an entry generated from the current document with a custom title" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#", title = Custom }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(docList(Root / "tree-2" / "doc-6", 6, 1, title = Some("Custom"))))
  }

  it should "produce an entry generated from a document referred to with an absolute path" in new RewriteSetup with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/tree-1/doc-3" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(docList(Root / "tree-1" / "doc-3", 3, 1)))
  }

  it should "fail when referring to a path that does not exist" in new RewriteSetup with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/tree-2/doc99" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors generating navigation: Unable to resolve document or tree with path: /tree-2/doc99"
    parseTemplateAndRewrite(template) shouldBe Right(error(msg, directive, template))
  }

  it should "fail with an invalid depth attribute" in new RewriteSetup with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/", depth = foo }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors decoding array elements: not an integer: foo"
    parseTemplateAndRewrite(template) shouldBe Right(error(msg, directive, template))
  }

  it should "fail with a manual node without title" in new RewriteSetup with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "http://foo.bar" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors decoding array elements: Not found: 'title'"
    parseTemplateAndRewrite(template) shouldBe Right(error(msg, directive, template))
  }

  "The block nav directive" should "produce two manual entries" in new RewriteSetup with NavModel {

    val input =
      """Title 6
        |=======
        |
        |aaa
        |
        |@:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Link 2, target = "http://domain-2.com/"} 
        |  ] 
        |}
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) shouldBe Right(blockResult(extLink(1), extLink(2)))
  }

  it should "produce an entry generated from a document referred to with a relative path" in new RewriteSetup with NavModel {

    val input =
      """Title 6
        |=======
        |
        |aaa
        |
        |@:navigationTree { 
        |  entries = [
        |    { target = "../tree-1/doc-3" }
        |  ] 
        |}
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) shouldBe Right(blockResult(docList(Root / "tree-1" / "doc-3", 3, 1)))
  }

  "The template breadcrumb directive" should "produce three entries" in new RewriteSetup with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb

    val input = "aaa @:breadcrumb bbb ${cursor.currentDocument.content}"

    parseTemplateAndRewrite(input) shouldBe Right(templateResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "tree-2" / "doc-6", 6, 1)
    ))
  }

  "The block breadcrumb directive" should "produce three entries" in new RewriteSetup with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb

    val input =
      """Title 6
        |=======
        |
        |aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) shouldBe Right(blockResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "tree-2" / "doc-6", 6, 1)
    ))
  }

  it should "produce three entries with title documents" in new RewriteSetup with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb
    override def hasTitleDocs: Boolean = true

    val input =
      """Title 6
        |=======
        |
        |aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) shouldBe Right(blockResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "tree-2" / "doc-6", 6, 1)
    ))
  }

}
