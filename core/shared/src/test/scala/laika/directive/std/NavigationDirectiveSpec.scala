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
import laika.ast.{Element, EmbeddedRoot, ExternalTarget, Header, Id, InternalTarget, InvalidSpan, NavigationItem, NavigationLink, NavigationList, NoOpt, Options, Path, RootElement, Section, SpanSequence, Style, TemplateElement, TemplateRoot, Text, Title}
import laika.ast.helper.ModelBuilder
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

    val refPath: Path = Root / "sub2" / "doc6"

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
      SpanSequence(title.getOrElse(s"Doc $doc")),
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
        docList(Root / s"sub$tree" / s"doc$docStartNum", docStartNum, level + 1),
        docList(Root / s"sub$tree" / s"doc${docStartNum + 1}", docStartNum + 1, level + 1),
      )
      if (hasTitleDocs) NavigationItem(
        SpanSequence("TitleDoc"),
        children,
        Some(NavigationLink(InternalTarget(Root / s"sub$tree" / "title").relativeTo(refPath))),
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
        docList(Root / "doc1", 1, 2),
        docList(Root / "doc2", 2, 2),
        treeList(1, 3, 2),
        treeList(2, 5, 2)
      ), options = styles(1))

    def templateResult (items: NavigationItem*): RootElement = buildResult(NavigationList(items, itemStyles))

    def error (msg: String, fragment: String, input: String): RootElement = {
      buildResult(InvalidSpan(msg, source(fragment, input)))
    }

    private val sections = Seq(
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(Header(1, List(Text("Section 1")), Id("section-1") + Style.section), Seq(
        Section(Header(2, List(Text("Section 2")), Id("section-2") + Style.section), Nil)
      )),
      Section(Header(1, List(Text("Section 3")), Id("section-3") + Style.section), Seq(
        Section(Header(2, List(Text("Section 4")), Id("section-4") + Style.section), Nil)
      ))
    )

    private def buildResult (element: Element): RootElement = {
      root(TemplateRoot(
        t("aaa "),
        TemplateElement(element),
        t(" bbb "),
        EmbeddedRoot(sections)
      ))
    }

    def blockResult (items: NavigationItem*): RootElement = root(
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

  "The template nav directive" should "produce two manual entries" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Link 2, target = "http://domain-2.com/"} 
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(extLink(1), extLink(2)))
  }

  it should "produce a manual entry and a generated entry" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(extLink(1), docList(Root / "sub2" / "doc6", 6, 1)))
  }

  it should "produce an entry generated from the root of the tree" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(rootList))
  }

  it should "produce an entry generated from the root of the tree with documents filtered by target format" in new TreeModel with NavModel {

    override def includeTargetFormatConfig = true

    val filteredRootList: NavigationItem =
      NavigationItem(SpanSequence("/"), List(
        docList(Root / "doc1", 1, 2),
        NavigationItem(SpanSequence("Tree 1"),
          List(
            docList(Root / s"sub1" / s"doc3", 3, 3).copy(targetFormats = TargetFormats.Selected(NonEmptySet.of("html", "txt")))
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

  it should "produce an entry generated from the root of the tree with title documents" in new TreeModel with NavModel {

    override val hasTitleDocs: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(rootList))
  }

  it should "produce an entry generated from the current tree" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with a maximum depth" in new TreeModel with NavModel {

    override def maxLevels: Int = 2

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", depth = 2 }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with the root excluded" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeRoot = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 0).content: _*))
  }

  it should "produce an entry generated from the current tree with the self link excluded" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  excludeSelf = true
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1, excludeSelf = true)))
  }

  it should "produce an entry generated from the current tree with sections excluded" in new TreeModel with NavModel {

    override def excludeSections: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeSections = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current document" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(docList(Root / "sub2" / "doc6", 6, 1)))
  }

  it should "produce an entry generated from the current document with a custom title" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#", title = Custom }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(docList(Root / "sub2" / "doc6", 6, 1, title = Some("Custom"))))
  }

  it should "produce an entry generated from a document referred to with an absolute path" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/sub1/doc3" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) shouldBe Right(templateResult(docList(Root / "sub1" / "doc3", 3, 1)))
  }

  it should "fail when referring to a path that does not exist" in new TreeModel with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/sub2/doc99" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors generating navigation: Unable to resolve document or tree with path: /sub2/doc99"
    parseTemplateAndRewrite(template) shouldBe Right(error(msg, directive, template))
  }

  it should "fail with an invalid depth attribute" in new TreeModel with NavModel {

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

  it should "fail with a manual node without title" in new TreeModel with NavModel {

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

  "The block nav directive" should "produce two manual entries" in new TreeModel with NavModel {

    val input =
      """aaa
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

  "The template breadcrumb directive" should "produce three entries" in new TreeModel with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb

    val input = "aaa @:breadcrumb bbb ${cursor.currentDocument.content}"

    parseTemplateAndRewrite(input) shouldBe Right(templateResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "sub2" / "doc6", 6, 1)
    ))
  }

  "The block breadcrumb directive" should "produce three entries" in new TreeModel with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb

    val input =
      """aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) shouldBe Right(blockResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "sub2" / "doc6", 6, 1)
    ))
  }

  it should "produce three entries with title documents" in new TreeModel with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb
    override def hasTitleDocs: Boolean = true

    val input =
      """aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) shouldBe Right(blockResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "sub2" / "doc6", 6, 1)
    ))
  }

  it should "produce an entry generated from a document referred to with a relative path" in new TreeModel with NavModel {

    val input =
      """aaa
        |
        |@:navigationTree { 
        |  entries = [
        |    { target = "../sub1/doc3" }
        |  ] 
        |}
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) shouldBe Right(blockResult(docList(Root / "sub1" / "doc3", 3, 1)))
  }
  
}
