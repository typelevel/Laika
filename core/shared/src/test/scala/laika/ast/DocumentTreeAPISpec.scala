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

package laika.ast

import cats.data.NonEmptyChain
import laika.api.builder.OperationConfig
import laika.config.{
  ArrayValue,
  Config,
  ConfigParser,
  DocumentConfigErrors,
  InvalidType,
  Key,
  LongValue,
  Origin,
  TreeConfigErrors,
  ValidationError
}
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.sample.{
  BuilderKey,
  MunitDocumentTreeAssertions,
  ParagraphCompanionShortcuts,
  SampleTrees,
  TestSourceBuilders
}
import laika.config.Config.ConfigResult
import laika.config.Origin.{ DocumentScope, Scope, TreeScope }
import laika.format.HTML
import laika.parse.GeneratedSource
import laika.rewrite.{ OutputContext, TemplateRewriter }
import munit.FunSuite

class DocumentTreeAPISpec extends FunSuite
    with ParagraphCompanionShortcuts
    with MunitDocumentTreeAssertions
    with TestSourceBuilders {

  def rootElement(b: Block): RootElement = RootElement(b, p("b"), p("c"))

  def createConfig(path: Path, source: Option[String], scope: Scope = DocumentScope): Config =
    source.map(c => ConfigParser.parse(c).resolve(Origin(scope, path)).toOption.get)
      .getOrElse(Config.empty)

  def treeWithDoc(
      path: Path,
      name: String,
      root: RootElement,
      config: Option[String] = None
  ): DocumentTree = {
    val doc = Document(path / name, root, config = createConfig(path / name, config))
    if (name == "README") DocumentTree(path, Nil, titleDocument = Some(doc))
    else DocumentTree(path, List(doc))
  }

  def treeWithTitleDoc(path: Path, root: RootElement, config: Option[String] = None): DocumentTree =
    DocumentTree(
      path,
      Nil,
      Some(Document(path / "title", root, config = createConfig(path / "title", config)))
    )

  def treeWithSubtree(
      path: Path,
      treeName: String,
      docName: String,
      root: RootElement,
      config: Option[String] = None
  ): DocumentTree =
    DocumentTree(path, List(treeWithDoc(path / treeName, docName, root, config)))

  def treeWithTwoSubtrees(
      contextRef: Option[String] = None,
      includeRuntimeMessage: Boolean = false
  ): DocumentTreeRoot = {
    val refNode                                 = contextRef.fold(Seq.empty[Span])(ref =>
      Seq(MarkupContextReference(Key.parse(ref), required = false, GeneratedSource))
    )
    def targetRoot(key: BuilderKey): Seq[Block] = {
      val msgNode =
        if (includeRuntimeMessage)
          Seq(
            InvalidSpan(
              s"Message ${key.defaultTitle}",
              generatedSource(s"Message ${key.defaultTitle}")
            )
          )
        else Nil
      Seq(Paragraph(refNode ++ msgNode))
    }
    SampleTrees.sixDocuments
      .titleDocuments(includeRoot = false)
      .docContent(targetRoot _)
      .build
  }

  def leafDocCursor(
      contextRef: Option[String] = None,
      includeRuntimeMessage: Boolean = false
  ): ConfigResult[DocumentCursor] =
    RootCursor(treeWithTwoSubtrees(contextRef, includeRuntimeMessage)).map(
      _.tree
        .children(3).asInstanceOf[TreeCursor]
        .children.last.asInstanceOf[DocumentCursor]
    )

  def firstDocCursor(tree: DocumentTree): ConfigResult[DocumentCursor] =
    TreeCursor(tree)
      .map(_.children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor])

  def rulesFor(cursor: DocumentCursor): ConfigResult[RewriteRules] =
    OperationConfig.default
      .rewriteRulesFor(cursor.root.target, RewritePhase.Render(OutputContext(HTML)))
      .apply(cursor)

  def rewriteWithReference(ref: String): ConfigResult[RootElement] = for {
    cursor <- leafDocCursor(Some(ref))
    rules  <- rulesFor(cursor)
    result <- cursor.target.rewrite(rules)
  } yield result.content

  test("access to the root tree when rewriting a document in the root tree") {
    val tree     = treeWithDoc(Root, "doc", rootElement(p("a")))
    val expected = treeWithDoc(Root, "doc", rootElement(p("/")))
    tree.rewrite { cursor =>
      Right(RewriteRules.forBlocks { case Paragraph(Seq(Text("a", _)), _) =>
        Replace(p(cursor.root.target.tree.path.toString))
      })
    }.assertEquals(expected)
  }

  test("access to the parent tree when rewriting a document in the root tree") {
    val tree     = treeWithDoc(Root, "doc", rootElement(p("a")))
    val expected = treeWithDoc(Root, "doc", rootElement(p("/")))
    tree.rewrite { cursor =>
      Right(RewriteRules.forBlocks { case Paragraph(Seq(Text("a", _)), _) =>
        Replace(p(cursor.parent.target.path.toString))
      })
    }.assertEquals(expected)
  }

  test("access to the root tree when rewriting a document in a child tree") {
    val tree     = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
    val expected = treeWithSubtree(Root, "sub", "doc", rootElement(p("/")))
    tree.rewrite { cursor =>
      Right(RewriteRules.forBlocks { case Paragraph(Seq(Text("a", _)), _) =>
        Replace(p(cursor.root.target.tree.path.toString))
      })
    }.assertEquals(expected)
  }

  test("access to the parent tree when rewriting a document in a child tree") {
    val tree     = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
    val expected = treeWithSubtree(Root, "sub", "doc", rootElement(p("/sub")))
    tree.rewrite { cursor =>
      Right(RewriteRules.forBlocks { case Paragraph(Seq(Text("a", _)), _) =>
        Replace(p(cursor.parent.target.path.toString))
      })
    }.assertEquals(expected)
  }

  test("obtain the tree title from the title document if present") {
    val title = Seq(Text("Title"))
    val tree  = treeWithTitleDoc(Root, RootElement(laika.ast.Title(title)))
    assertEquals(tree.title, Some(SpanSequence(title)))
  }

  test("obtain the title from the document config if present") {
    val title = Seq(Text("from-content"))
    val tree  = treeWithDoc(
      Root,
      "doc",
      RootElement(laika.ast.Title(title)),
      Some("laika.title: from-config")
    )
    assertEquals(tree.content.head.title, Some(SpanSequence("from-config")))
  }

  test("do not inherit the tree title as the document title") {
    val title      = Seq(Text("from-content"))
    val treeConfig = createConfig(Root, Some("laika.title: from-config"), TreeScope)
    val docConfig  = createConfig(Root / "doc", Some("foo: bar")).withFallback(treeConfig)
    val tree       = DocumentTree(
      Root,
      List(
        Document(Root / "doc", RootElement(laika.ast.Title(title)), config = docConfig)
      ),
      config = treeConfig
    )
    assertEquals(tree.content.head.title, Some(SpanSequence(title)))
  }

  test("allow to select a document from a subdirectory using a relative path") {
    val tree = treeWithSubtree(Root, "sub", "doc", RootElement.empty)
    assertEquals(
      tree.selectDocument(CurrentTree / "sub" / "doc").map(_.path),
      Some(Root / "sub" / "doc")
    )
  }

  test("allow to select a document in the current directory using a relative path") {
    val tree = treeWithDoc(Root, "doc", RootElement.empty)
    assertEquals(
      tree.selectDocument(CurrentTree / "doc").map(_.path),
      Some(Root / "doc")
    )
  }

  test("allow to select a title document in the current directory using a relative path") {
    val tree = treeWithDoc(Root, "README", RootElement.empty)
    assertEquals(
      tree.selectDocument(CurrentTree / "README").map(_.path),
      Some(Root / "README")
    )
  }

  test("allow to select a subtree in a child directory using a relative path") {
    val tree     = treeWithSubtree(Root / "top", "sub", "doc", RootElement.empty)
    val treeRoot = DocumentTree(Root, List(tree))
    assertEquals(
      treeRoot.selectSubtree(CurrentTree / "top" / "sub").map(_.path),
      Some(Root / "top" / "sub")
    )
  }

  test("allow to select a subtree in the current directory using a relative path") {
    val tree = treeWithSubtree(Root, "sub", "doc", RootElement.empty)
    assertEquals(
      tree.selectSubtree(CurrentTree / "sub").map(_.path),
      Some(Root / "sub")
    )
  }

  test("allow to specify a template for a document using an absolute path") {
    val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
    val tree     = treeWithSubtree(
      Root,
      "sub",
      "doc",
      RootElement.empty,
      Some("laika.template: /main.template.html")
    ).copy(templates = List(template))
    val result   = firstDocCursor(tree).flatMap(TemplateRewriter.selectTemplate(_, "html"))
    assertEquals(result, Right(Some(template)))
  }

  test("allow to specify a template for a document for a specific output format") {
    val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
    val tree     = treeWithSubtree(
      Root,
      "sub",
      "doc",
      RootElement.empty,
      Some("laika.html.template: /main.template.html")
    ).copy(templates = List(template))
    val result   = firstDocCursor(tree).flatMap(TemplateRewriter.selectTemplate(_, "html"))
    assertEquals(result, Right(Some(template)))
  }

  test("allow to specify a template for a document using a relative path") {
    val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
    val tree     = treeWithSubtree(
      Root,
      "sub",
      "doc",
      RootElement.empty,
      Some("laika.template: ../main.template.html")
    ).copy(templates = List(template))
    val result   = firstDocCursor(tree).flatMap(TemplateRewriter.selectTemplate(_, "html"))
    assertEquals(result, Right(Some(template)))
  }

  test("fail if a specified template does not exist") {
    val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
    val tree     = treeWithSubtree(
      Root,
      "sub",
      "doc",
      RootElement.empty,
      Some("laika.template: ../missing.template.html")
    ).copy(templates = List(template))
    val result   = firstDocCursor(tree).flatMap(TemplateRewriter.selectTemplate(_, "html"))
    assertEquals(
      result,
      Left(ValidationError("Template with path '/missing.template.html' not found"))
    )
  }

  test("allow to rewrite the tree using a custom rule") {
    val tree      = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
    val rewritten = tree rewrite { _ =>
      Right(RewriteRules.forSpans { case Text("a", _) =>
        Replace(Text("x"))
      })
    }
    val target    = rewritten.map(_.selectDocument("sub/doc").map(_.content))
    assertEquals(target, Right(Some(RootElement(p("x"), p("b"), p("c")))))
  }

  test("give access to the previous sibling in a hierarchical view") {
    assertEquals(
      leafDocCursor().toOption.flatMap(_.previousDocument.map(_.path)),
      Some(Root / "tree-2" / "doc-5")
    )
  }

  test("return None for the next document in the final leaf node of the tree") {
    assertEquals(
      leafDocCursor().toOption.flatMap(_.nextDocument),
      None
    )
  }

  test("give access to the previous title document in a hierarchical view for a title document") {
    assertEquals(
      leafDocCursor().toOption.flatMap(_.parent.titleDocument.get.previousDocument.map(_.path)),
      Some(Root / "tree-1" / "README")
    )
  }

  test("give access to the previous sibling in a flattened view") {
    assertEquals(
      leafDocCursor().toOption.flatMap(
        _.flattenedSiblings.previousDocument
          .flatMap(_.flattenedSiblings.previousDocument)
          .flatMap(_.flattenedSiblings.previousDocument)
          .map(_.path)
      ),
      Some(Root / "tree-1" / "doc-4")
    )
  }

  test("resolve a substitution reference to the source path of the previous document") {
    assertEquals(
      rewriteWithReference("cursor.previousDocument.sourcePath"),
      Right(RootElement(p("/tree-2/doc-5")))
    )
  }

  test("resolve a substitution reference to the output path of the previous document") {
    assertEquals(
      rewriteWithReference("cursor.previousDocument.path"),
      Right(RootElement(RawLink.internal(Root / "tree-2" / "doc-5")))
    )
  }

  test("be empty for the next document in the final leaf node of the tree") {
    assertEquals(
      rewriteWithReference("cursor.nextDocument.path"),
      Right(RootElement(p("")))
    )
  }

  test("resolve a substitution reference to the source path of the parent document") {
    assertEquals(
      rewriteWithReference("cursor.parentDocument.sourcePath"),
      Right(RootElement(p("/tree-2/README")))
    )
  }

  test("resolve a substitution reference to the output path of the parent document") {
    assertEquals(
      rewriteWithReference("cursor.parentDocument.path"),
      Right(RootElement(RawLink.internal(Root / "tree-2" / "README")))
    )
  }

  test(
    "resolve a substitution reference to the source path of the previous document in a flattened view"
  ) {
    val cursor = leafDocCursor(Some("cursor.flattenedSiblings.previousDocument.sourcePath")).map(
      _
        .flattenedSiblings.previousDocument
        .flatMap(_.flattenedSiblings.previousDocument)
        .get
    )
    assertEquals(
      cursor.flatMap(c => rulesFor(c).flatMap(c.target.rewrite).map(_.content)),
      Right(RootElement(p("/tree-1/doc-4")))
    )
  }

  test(
    "resolve a substitution reference to the output path of the previous document in a flattened view"
  ) {
    val cursor = leafDocCursor(Some("cursor.flattenedSiblings.previousDocument.path")).map(
      _
        .flattenedSiblings.previousDocument
        .flatMap(_.flattenedSiblings.previousDocument)
        .get
    )
    assertEquals(
      cursor.flatMap(c => rulesFor(c).flatMap(c.target.rewrite).map(_.content)),
      Right(RootElement(RawLink.internal(Root / "tree-1" / "doc-4")))
    )
  }

  import BuilderKey._
  val keys = Seq(Doc(1), Doc(2), Tree(1), Doc(3), Doc(4), Tree(2), Doc(5), Doc(6))

  test("provide all runtime messages in the tree") {
    val root     = leafDocCursor(includeRuntimeMessage = true).map(_.root.target)
    val expected = keys.map { key =>
      RuntimeMessage(MessageLevel.Error, s"Message ${key.defaultTitle}")
    }
    assertEquals(
      root.map(_.tree.runtimeMessages(MessageFilter.Warning)),
      Right(expected)
    )
  }

  test("provide all invalid spans in the tree") {
    val root     = leafDocCursor(includeRuntimeMessage = true).map(_.root.target)
    val expected = keys.map { key =>
      InvalidSpan(s"Message ${key.defaultTitle}", generatedSource(s"Message ${key.defaultTitle}"))
    }
    assertEquals(
      root.map(_.tree.invalidElements(MessageFilter.Warning)),
      Right(expected)
    )
  }

  test("fail if cursor creation fails due to invalid configuration entries") {
    val tree          =
      treeWithSubtree(Root, "sub", "doc", rootElement(p("a")), Some("laika.versioned: [1,2,3]"))
    val expectedError = TreeConfigErrors(
      NonEmptyChain(
        DocumentConfigErrors(
          Root / "sub" / "doc",
          NonEmptyChain(
            InvalidType("Boolean", ArrayValue(List(LongValue(1), LongValue(2), LongValue(3))))
          )
        )
      )
    )
    val res           = tree.rewrite { cursor =>
      Right(RewriteRules.forBlocks { case Paragraph(Seq(Text("a", _)), _) =>
        Replace(p(cursor.root.target.tree.path.toString))
      })
    }
    assertEquals(res, Left(expectedError))
  }

}
