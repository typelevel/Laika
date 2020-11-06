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

import laika.config.{Config, ConfigParser, Key, Origin, ValidationError}
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.helper.ModelBuilder
import laika.ast.sample.{BuilderKey, DocumentTreeAssertions, SampleTrees}
import laika.config.Origin.{DocumentScope, Scope, TreeScope}
import laika.parse.GeneratedSource
import laika.rewrite.TemplateRewriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentTreeAPISpec extends AnyFlatSpec 
                      with Matchers
                      with ModelBuilder 
                      with DocumentTreeAssertions {
  
  trait TreeModel {
    def rootElement (b: Block): RootElement = root(b, p("b"), p("c"))
    
    def createConfig (path: Path, source: Option[String], scope: Scope = DocumentScope): Config =
      source.map(c => ConfigParser.parse(c).resolve(Origin(scope, path)).toOption.get)
      .getOrElse(Config.empty)

    def treeWithDoc (path: Path, name: String, root: RootElement, config: Option[String] = None): DocumentTree = {
      val doc = Document(path / name, root, config = createConfig(path / name, config))
      if (name == "README") DocumentTree(path, Nil, titleDocument = Some(doc))
      else DocumentTree(path, List(doc))
    }
    
    def treeWithTitleDoc (path: Path, root: RootElement, config: Option[String] = None): DocumentTree =
      DocumentTree(path, Nil, Some(Document(path / "title", root, config = createConfig(path / "title", config))))
    
    def treeWithSubtree (path: Path, treeName: String, docName: String, root: RootElement, config: Option[String] = None): DocumentTree =
      DocumentTree(path, List(treeWithDoc(path / treeName, docName, root, config)))

    def treeWithTwoSubtrees (contextRef: Option[String] = None, includeRuntimeMessage: Boolean = false): DocumentTreeRoot = {
      val refNode = contextRef.fold(Seq.empty[Span])(ref => Seq(MarkupContextReference(Key.parse(ref), required = false, GeneratedSource)))
      def targetRoot (key: BuilderKey): Seq[Block] = {
        val msgNode = 
          if (includeRuntimeMessage) Seq(InvalidSpan(s"Message ${key.defaultTitle}", generatedSource(s"Message ${key.defaultTitle}"))) 
          else Nil
        Seq(Paragraph(refNode ++ msgNode))
      }
      SampleTrees.sixDocuments
        .titleDocuments(includeRoot = false)
        .docContent(targetRoot _)
        .build
    }
    def leafDocCursor (contextRef: Option[String] = None, includeRuntimeMessage: Boolean = false): DocumentCursor = 
      RootCursor(treeWithTwoSubtrees(contextRef, includeRuntimeMessage)).tree
        .children.last.asInstanceOf[TreeCursor]
        .children.last.asInstanceOf[DocumentCursor]
    
  }
  
  "The DocumentTree API" should "give access to the root tree when rewriting a document in the root tree" in {
    new TreeModel {
      val tree     = treeWithDoc(Root, "doc", rootElement(p("a")))
      val expected = treeWithDoc(Root, "doc", rootElement(p("/")))
      tree.rewrite { cursor => RewriteRules.forBlocks { 
        case Paragraph(Seq(Text("a",_)),_) => Replace(p(cursor.root.target.tree.path.toString)) 
      }}.assertEquals(expected)
    }
  }
  
  it should "give access to the parent tree when rewriting a document in the root tree" in {
    new TreeModel {
      val tree     = treeWithDoc(Root, "doc", rootElement(p("a")))
      val expected = treeWithDoc(Root, "doc", rootElement(p("/")))
      tree.rewrite { cursor => RewriteRules.forBlocks { 
        case Paragraph(Seq(Text("a",_)),_) => Replace(p(cursor.parent.target.path.toString)) 
      }}.assertEquals(expected)
    }
  }
  
  it should "give access to the root tree when rewriting a document in a child tree" in {
    new TreeModel {
      val tree     = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val expected = treeWithSubtree(Root, "sub", "doc", rootElement(p("/")))
      tree.rewrite { cursor => RewriteRules.forBlocks { 
        case Paragraph(Seq(Text("a",_)),_) => Replace(p(cursor.root.target.tree.path.toString)) 
      }}.assertEquals(expected)
    }
  }
  
  it should "give access to the parent tree when rewriting a document in a child tree" in {
    new TreeModel {
      val tree     = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val expected = treeWithSubtree(Root, "sub", "doc", rootElement(p("/sub")))
      tree.rewrite { cursor => RewriteRules.forBlocks { 
        case Paragraph(Seq(Text("a",_)),_) => Replace(p(cursor.parent.target.path.toString)) 
      }}.assertEquals(expected)
    }
  }

  it should "obtain the tree title from the title document if present" in {
    new TreeModel {
      val title = Seq(Text("Title"))
      val tree = treeWithTitleDoc(Root, root(laika.ast.Title(title)))
      tree.title should be (Some(SpanSequence(title)))
    }
  }

  it should "obtain the title from the document config if present" in {
    new TreeModel {
      val title = Seq(Text("from-content"))
      val tree = treeWithDoc(Root, "doc", root(laika.ast.Title(title)), Some("laika.title: from-config"))
      tree.content.head.title should be (Some(SpanSequence("from-config")))
    }
  }

  it should "not inherit the tree title as the document title" in {
    new TreeModel {
      val title = Seq(Text("from-content"))
      val treeConfig = createConfig(Root, Some("laika.title: from-config"), TreeScope)
      val docConfig = createConfig(Root / "doc", Some("foo: bar")).withFallback(treeConfig)
      val tree = DocumentTree(Root, List(
        Document(Root / "doc", root(laika.ast.Title(title)), config = docConfig)
      ), config = treeConfig)
      tree.content.head.title should be (Some(SpanSequence(title)))
    }
  }
  
  it should "allow to select a document from a subdirectory using a relative path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", root())
      tree.selectDocument(CurrentTree / "sub" / "doc").map(_.path) should be (Some(Root / "sub" / "doc"))
    }
  }
  
  it should "allow to select a document in the current directory using a relative path" in {
    new TreeModel {
      val tree = treeWithDoc(Root, "doc", root())
      tree.selectDocument(CurrentTree / "doc").map(_.path) should be (Some(Root / "doc"))
    }
  }

  it should "allow to select a title document in the current directory using a relative path" in {
    new TreeModel {
      val tree = treeWithDoc(Root, "README", root())
      tree.selectDocument(CurrentTree / "README").map(_.path) should be (Some(Root / "README"))
    }
  }
  
  it should "allow to select a subtree in a child directory using a relative path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root / "top", "sub", "doc", root())
      val treeRoot = DocumentTree(Root, List(tree))
      treeRoot.selectSubtree(CurrentTree / "top" / "sub").map(_.path) should be (Some(Root / "top" / "sub"))
    }
  }
  
  it should "allow to select a subtree in the current directory using a relative path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", root())
      tree.selectSubtree(CurrentTree / "sub").map(_.path) should be (Some(Root / "sub"))
    }
  }
  
  it should "allow to specify a template for a document using an absolute path" in {
    new TreeModel {
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
      val tree = treeWithSubtree(Root, "sub", "doc", root(), Some("laika.template: /main.template.html")).copy(templates = List(template))
      val cursor = TreeCursor(tree).children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor]
      TemplateRewriter.selectTemplate(cursor,  "html") should be (Right(Some(template)))
    }
  }
  
  it should "allow to specify a template for a document for a specific output format" in {
    new TreeModel {
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
      val tree = treeWithSubtree(Root, "sub", "doc", root(), Some("laika.html.template: /main.template.html")).copy(templates = List(template))
      val cursor = TreeCursor(tree).children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor]
      TemplateRewriter.selectTemplate(cursor,  "html") should be (Right(Some(template)))
    }
  }
  
  it should "allow to specify a template for a document using a relative path" in {
    new TreeModel {
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
      val tree = treeWithSubtree(Root, "sub", "doc", root(), Some("laika.template: ../main.template.html")).copy(templates = List(template))
      val cursor = TreeCursor(tree).children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor]
      TemplateRewriter.selectTemplate(cursor,  "html") should be (Right(Some(template)))
    }
  }

  it should "fail if a specified template does not exist" in {
    new TreeModel {
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot.empty)
      val tree = treeWithSubtree(Root, "sub", "doc", root(), Some("laika.template: ../missing.template.html")).copy(templates = List(template))
      val cursor = TreeCursor(tree).children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor]
      TemplateRewriter.selectTemplate(cursor,  "html") should be (Left(ValidationError("Template with path '/missing.template.html' not found")))
    }
  }
  
  it should "allow to rewrite the tree using a custom rule" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val rewritten = tree rewrite { _ => RewriteRules.forSpans {
        case Text("a",_) => Replace(Text("x"))
      }}
      val target = rewritten.selectDocument("sub/doc")
      target.get.content should be (root(p("x"), p("b"), p("c")))
    }
  }

  it should "give access to the previous sibling in a hierarchical view" in new TreeModel {
    leafDocCursor().previousDocument.map(_.path) shouldBe Some(Root / "tree-2" / "doc-5")  
  }

  it should "return None for the next document in the final leaf node of the tree" in new TreeModel {
    leafDocCursor().nextDocument shouldBe None
  }

  it should "give access to the previous title document in a hierarchical view for a title document" in new TreeModel {
    leafDocCursor().parent.titleDocument.get.previousDocument.map(_.path) shouldBe Some(Root / "tree-1" / "README")
  }

  it should "give access to the previous sibling in a flattened view" in new TreeModel {
    leafDocCursor().flattenedSiblings.previousDocument
      .flatMap(_.flattenedSiblings.previousDocument)
      .flatMap(_.flattenedSiblings.previousDocument)
      .map(_.path) shouldBe Some(Root / "tree-1" / "doc-4")
  }
  
  it should "resolve a substitution reference to the previous document" in new TreeModel {
    val cursor = leafDocCursor(Some("cursor.previousDocument.relativePath"))
    cursor.target.rewrite(TemplateRewriter.rewriteRules(cursor)).content shouldBe root(p("doc-5"))
  }

  it should "be empty for the next document in the final leaf node of the tree" in new TreeModel {
    val cursor = leafDocCursor(Some("cursor.nextDocument.relativePath"))
    cursor.target.rewrite(TemplateRewriter.rewriteRules(cursor)).content shouldBe root(p(""))
  }

  it should "resolve a substitution reference to the parent document" in new TreeModel {
    val cursor = leafDocCursor(Some("cursor.parentDocument.relativePath"))
    cursor.target.rewrite(TemplateRewriter.rewriteRules(cursor)).content shouldBe root(p("README"))
  }

  it should "resolve a substitution reference to the previous document in a flattened view" in new TreeModel {
    val cursor = leafDocCursor(Some("cursor.flattenedSiblings.previousDocument.relativePath"))
      .flattenedSiblings.previousDocument
      .flatMap(_.flattenedSiblings.previousDocument)
      .get
    cursor.target.rewrite(TemplateRewriter.rewriteRules(cursor)).content shouldBe root(p("../tree-1/doc-4"))
  }
  
  import BuilderKey._
  val keys = Seq(Doc(1), Doc(2), Tree(1), Doc(3), Doc(4), Tree(2), Doc(5), Doc(6))
  
  it should "provide all runtime messages in the tree" in new TreeModel {
    val root = leafDocCursor(includeRuntimeMessage = true).root.target
    val expected = keys.map { key =>
      RuntimeMessage(MessageLevel.Error, s"Message ${key.defaultTitle}")
    }
    root.tree.runtimeMessages(MessageFilter.Warning) shouldBe expected
  }

  it should "provide all invalid spans in the tree" in new TreeModel {
    val root = leafDocCursor(includeRuntimeMessage = true).root.target
    val expected = keys.map { key =>
      InvalidSpan(s"Message ${key.defaultTitle}", generatedSource(s"Message ${key.defaultTitle}"))
    }
    root.tree.invalidElements(MessageFilter.Warning) shouldBe expected
  }

}
