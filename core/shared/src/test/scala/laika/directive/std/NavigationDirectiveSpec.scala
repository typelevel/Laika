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
import laika.ast.sample.{BuilderKey, ParagraphCompanionShortcuts, SampleContent, TestSourceBuilders}
import laika.ast._
import laika.rewrite.nav.TargetFormats
import munit.FunSuite
import RewriteSetup._

class NavigationDirectiveSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {

  object NavModel {

    private val refPath: Path = Root / "tree-2" / "doc-6"

    def styles (level: Int)(implicit options: NavOptions): Options = Style.level(level) + options.itemStyles
    
    def rootEntry(implicit options: NavOptions): NavigationItem = NavigationItem(SpanSequence("/"), Nil, None, TargetFormats.All, styles(1))

    private def sectionList (path: Path, section: Int, level: Int)(implicit options: NavOptions): NavigationItem = NavigationItem(
      SpanSequence(s"Section $section"),
      if (section % 2 == 0 || level == options.maxLevels) Nil else Seq(
        sectionList(path, section + 1, level+1)
      ),
      Some(NavigationLink(InternalTarget(path.withFragment(s"section-$section")).relativeTo(refPath))),
      options = styles(level)
    )

    def docList (path: Path, doc: Int, level: Int, title: Option[String] = None)(implicit options: NavOptions): NavigationItem = NavigationItem(
      SpanSequence(title.getOrElse(s"Title $doc")),
      if (level == options.maxLevels || options.excludeSections) Nil else Seq(
        sectionList(path, 1, level+1),
        sectionList(path, 3, level+1)
      ),
      Some(NavigationLink(InternalTarget(path).relativeTo(refPath), path == refPath)),
      TargetFormats.All,
      styles(level)
    )

    def treeList (tree: Int, docStartNum: Int, level: Int, 
                  excludeSelf: Boolean = false, 
                  selfLink: Boolean = false)(implicit options: NavOptions): NavigationItem = {
      
      val titleDocPath    = Root / s"tree-$tree" / "README"
      val titleDocRefPath = if (selfLink) titleDocPath else refPath
      val title           = if (selfLink) "Title 6" else s"Tree $tree"
      
      val children = if (level == options.maxLevels) Nil else List(
        docList(Root / s"tree-$tree" / s"doc-$docStartNum", docStartNum, level + 1),
        docList(Root / s"tree-$tree" / s"doc-${docStartNum + 1}", docStartNum + 1, level + 1),
      )
      if (options.hasTitleDocs) NavigationItem(
        SpanSequence(title),
        children,
        Some(NavigationLink(InternalTarget(titleDocPath).relativeTo(titleDocRefPath), selfLink = selfLink)),
        options = styles(level)
      )
      else NavigationItem(
        SpanSequence(title),
        if (excludeSelf) children.take(1) else children,
        options = styles(level)
      )
    }

    def rootList(implicit options: NavOptions): NavigationItem =
      NavigationItem(SpanSequence("/"), List(
        docList(Root / "doc-1", 1, 2),
        docList(Root / "doc-2", 2, 2),
        treeList(1, 3, 2),
        treeList(2, 5, 2)
      ), options = styles(1))

    def templateResult (items: NavigationItem*)(implicit options: NavOptions): RootElement = 
      buildResult(NavigationList(items, options.itemStyles))

    def error (msg: String, fragment: String, input: String): RootElement = {
      buildResult(InvalidSpan(msg, source(fragment, input)))
    }

    private def buildResult (element: Element): RootElement = {
      RootElement(TemplateRoot(
        TemplateString("aaa "),
        TemplateElement(element),
        TemplateString(" bbb "),
        EmbeddedRoot(SampleContent.fourSections(BuilderKey.Doc(6)))
      ))
    }

    def blockResult (items: NavigationItem*)(implicit options: NavOptions): RootElement = RootElement(
      Title("Title 6").withOptions(Id("title-6") + Style.title),
      p("aaa"),
      NavigationList(items, options.itemStyles),
      p("bbb")
    )

    def extLink (num: Int, level: Int): NavigationItem = NavigationItem(
      SpanSequence(s"Link $num"),
      Nil,
      Some(NavigationLink(ExternalTarget(s"http://domain-$num.com/")))
    ).withOptions(Style.level(level))
    
    def section (title: String, entries: NavigationItem*): NavigationItem = NavigationItem(
      SpanSequence(title),
      entries,
      None
    ).withOptions(Style.level(1))
  }
  
  import NavModel._
  
  val defaultNavOptions: NavOptions = NavOptions()

  def runDocument (input: String, expectedNav: NavigationItem*)(implicit options: NavOptions): Unit =
    runDocument(input, docUnderTestIsTitle = false, expectedNav)

  def runTitleDocument (input: String, expectedNav: NavigationItem*)(implicit options: NavOptions): Unit =
    runDocument(input, docUnderTestIsTitle = true, expectedNav)

  private def runDocument (input: String, docUnderTestIsTitle: Boolean, expectedNav: Seq[NavigationItem])(implicit options: NavOptions): Unit = {
    val res = parseDocumentAndRewrite(input,
      hasTitleDocs = options.hasTitleDocs,
      includeTargetFormatConfig = options.includeTargetFormatConfig,
      docUnderTestIsTitle = docUnderTestIsTitle
    )
    assertEquals(
      res,
      Right(NavModel.blockResult(expectedNav:_*)(options))
    )
  }
  
  def runTemplate (input: String, expectedNav: NavigationItem*)(implicit options: NavOptions): Unit =
    assertEquals(
      parseTemplateAndRewrite(input, 
        hasTitleDocs = options.hasTitleDocs, 
        includeTargetFormatConfig = options.includeTargetFormatConfig
      ),
      Right(NavModel.templateResult(expectedNav:_*)(options))
    )

  def runTemplateError (input: String, directive: String, expectedMessage: String): Unit =
    assertEquals(parseTemplateAndRewrite(input), Right(error(expectedMessage, directive, input)))

  case class NavOptions (hasTitleDocs: Boolean = false,
                         maxLevels: Int = Int.MaxValue,
                         excludeSections: Boolean = false,
                         itemStyles: Options = NoOpt,
                         includeTargetFormatConfig: Boolean = false)
  
  object NavOptions {
    implicit lazy val defaults: NavOptions = NavOptions()
  }
  
  test("template nav - two manual entries") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Link 2, target = "http://domain-2.com/"} 
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, extLink(1, 1), extLink(2, 1))
  }

  test("template nav - one manual entry and a section with two manual entries") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Section, entries = [
        |      { title = Link 2, target = "http://domain-2.com/"}
        |      { title = Link 3, target = "http://domain-3.com/"}
        |    ]}   
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, extLink(1, 1), section("Section", extLink(2, 2), extLink(3, 2)))
  }

  test("template nav - a manual entry and a generated entry") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, extLink(1, 1), docList(Root / "tree-2" / "doc-6", 6, 1))
  }

  test("template nav - an entry generated from the root of the tree") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, rootList)
  }

  test("template nav - an entry generated from the root of the tree with documents filtered by target format") {

    implicit val options: NavOptions = NavOptions(includeTargetFormatConfig = true)

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

    runTemplate(template, filteredRootList)
  }

  test("template nav - an entry generated from the root of the tree with title documents") {

    implicit val options: NavOptions = NavOptions(hasTitleDocs = true)

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, rootList)
  }

  test("template nav - an entry generated from the current tree") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, treeList(2, 5, 1))
  }

  test("template nav - an entry generated from the current tree with a maximum depth") {

    implicit val options: NavOptions = NavOptions(maxLevels = 2)

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", depth = 2 }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, treeList(2, 5, 1))
  }

  test("template nav - an entry generated from the current tree with the root excluded") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeRoot = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, treeList(2, 5, 0).content: _*)
  }

  test("template nav - an entry generated from the current tree with the self link excluded") {

    val template =
      """aaa @:navigationTree { 
        |  excludeSelf = true
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, treeList(2, 5, 1, excludeSelf = true))
  }

  test("template nav - an entry generated from the current tree with sections excluded") {

    implicit val options: NavOptions = NavOptions(excludeSections = true)

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeSections = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, treeList(2, 5, 1))
  }

  test("template nav - an entry generated from the current document") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, docList(Root / "tree-2" / "doc-6", 6, 1))
  }

  test("template nav - an entry generated from the current document with a custom title") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#", title = Custom }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, docList(Root / "tree-2" / "doc-6", 6, 1, title = Some("Custom")))
  }

  test("template nav - an entry generated from a document referred to with an absolute path") {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/tree-1/doc-3" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    runTemplate(template, docList(Root / "tree-1" / "doc-3", 3, 1))
  }

  test("template nav - fail when referring to a path that does not exist") {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/tree-2/doc99" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors generating navigation: Unable to resolve document or tree with path: /tree-2/doc99"
    runTemplateError(template, directive, msg)
  }

  test("template nav - fail with an invalid depth attribute") {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/", depth = foo }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': Error decoding 'entries': One or more errors decoding array elements: Error decoding 'depth': not an integer: foo"
    runTemplateError(template, directive, msg)
  }

  test("template nav - fail with a manual node without title") {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "http://foo.bar" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': Error decoding 'entries': One or more errors decoding array elements: Not found: 'title'"
    runTemplateError(template, directive, msg)
  }

  
  
  test("block nav - two manual entries") {

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

    runDocument(input, extLink(1, 1), extLink(2, 1))
  }

  test("block nav - entry generated from a document referred to with a relative path") {

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

    runDocument(input, docList(Root / "tree-1" / "doc-3", 3, 1))
  }

  test("template breadcrumb directive - three entries") {

    implicit val options: NavOptions = NavOptions(maxLevels = 1, itemStyles = Style.breadcrumb)
    
    val input = "aaa @:breadcrumb bbb ${cursor.currentDocument.content}"

    runTemplate(input,
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "tree-2" / "doc-6", 6, 1)
    )
  }

  test("template breadcrumb directive - avoid duplicate entries when title documents are present") {

    implicit val options: NavOptions = NavOptions(maxLevels = 1, itemStyles = Style.breadcrumb, hasTitleDocs = true)

    val input = "aaa @:breadcrumb bbb ${cursor.currentDocument.content}"

    runTemplate(input,
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "tree-2" / "doc-6", 6, 1)
    )
  }

  test("block breadcrumb directive - three entries") {

    implicit val options: NavOptions = NavOptions(maxLevels = 1, itemStyles = Style.breadcrumb)
    
    val input =
      """Title 6
        |=======
        |
        |aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    runDocument(input,
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "tree-2" / "doc-6", 6, 1)
    )
  }

  test("block breadcrumb directive - three entries with title documents") {

    implicit val options: NavOptions = NavOptions(maxLevels = 1, itemStyles = Style.breadcrumb, hasTitleDocs = true)

    val input =
      """Title 6
        |=======
        |
        |aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    runTitleDocument(input,
      rootEntry,
      treeList(2, 0, 1, selfLink = true)
    )
  }

}
