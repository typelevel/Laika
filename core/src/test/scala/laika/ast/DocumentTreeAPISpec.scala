/*
 * Copyright 2013-2016 the original author or authors.
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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import laika.io.DocumentType.Markup
import laika.ast.Path.{Current, Root}
import laika.ast.helper.ModelBuilder
import laika.ast.helper.DocumentViewBuilder.{Documents => Docs}
import laika.ast.helper.DocumentViewBuilder._
import laika.rewrite.DocumentCursor
import laika.rewrite.TemplateRewriter
import laika.rewrite.TreeCursor
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigParseOptions

class DocumentTreeAPISpec extends FlatSpec 
                      with Matchers
                      with ModelBuilder {
  
  trait TreeModel {
    def rootElement (b: Block): RootElement = root(b, p("b"), p("c"))
    
    def createConfig (path: Path, source: Option[String]): Config =
      source.map(c =>ConfigFactory.parseString(c, ConfigParseOptions.defaults().setOriginDescription(s"path:$path")))
      .getOrElse(ConfigFactory.empty)
    
    def treeWithDoc (path: Path, name: String, root: RootElement, config: Option[String] = None): DocumentTree =
      DocumentTree(path, List(Document(path / name, root, config = createConfig(path / name, config))))
    def treeWithSubtree (path: Path, treeName: String, docName: String, root: RootElement, config: Option[String] = None): DocumentTree =
      DocumentTree(path, List(treeWithDoc(path / treeName, docName, root, config)))
    
    def treeViewWithDoc (path: Path, name: String, root: RootElement): TreeView =
      TreeView(path, List(Docs(Markup, List(DocumentView(path / name, List(Content(root.content)))))))
    def treeViewWithSubtree (path: Path, treeName: String, docName: String, root: RootElement): TreeView =
      TreeView(path, List(Subtrees(List(treeViewWithDoc(path / treeName, docName, root)))))
  }
  
  "The DocumentTree API" should "give access to the root tree when rewriting a document in the root tree" in {
    new TreeModel {
      val tree = treeWithDoc(Root, "doc", rootElement(p("a")))
      val treeResult = treeViewWithDoc(Root, "doc", rootElement(p("/")))
      viewOf(tree rewrite { cursor => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(cursor.root.target.path.toString)) 
      }}) should be (treeResult)
    }
  }
  
  it should "give access to the parent tree when rewriting a document in the root tree" in {
    new TreeModel {
      val tree = treeWithDoc(Root, "doc", rootElement(p("a")))
      val treeResult = treeViewWithDoc(Root, "doc", rootElement(p("/")))
      viewOf(tree rewrite { cursor => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(cursor.parent.target.path.toString)) 
      }}) should be (treeResult)
    }
  }
  
  it should "give access to the root tree when rewriting a document in a child tree" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val treeResult = treeViewWithSubtree(Root, "sub", "doc", rootElement(p("/")))
      viewOf(tree rewrite { cursor => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(cursor.root.target.path.toString)) 
      }}) should be (treeResult)
    }
  }
  
  it should "give access to the parent tree when rewriting a document in a child tree" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val treeResult = treeViewWithSubtree(Root, "sub", "doc", rootElement(p("/sub")))
      viewOf(tree rewrite { cursor => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(cursor.parent.target.path.toString)) 
      }}) should be (treeResult)
    }
  }
  
  it should "allow to select a document from a subdirectory using a relative path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", root())
      tree.selectDocument(Current / "sub" / "doc").map(_.path) should be (Some(Root / "sub" / "doc"))
    }
  }
  
  it should "not allow to select a document using an absolute path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", root())
      tree.selectDocument(Root / "sub" / "doc").map(_.path) should be (None)
    }
  }
  
  it should "allow to select a document in the current directory using a relative path" in {
    new TreeModel {
      val tree = treeWithDoc(Root, "doc", root())
      tree.selectDocument(Current / "doc").map(_.path) should be (Some(Root / "doc"))
    }
  }
  
  it should "allow to select a subtree in a child directory using a relative path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root / "top", "sub", "doc", root())
      val treeRoot = DocumentTree(Root, List(tree))
      treeRoot.selectSubtree(Current / "top" / "sub").map(_.path) should be (Some(Root / "top" / "sub"))
    }
  }
  
  it should "allow to select a subtree in the current directory using a relative path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", root())
      tree.selectSubtree(Current / "sub").map(_.path) should be (Some(Root / "sub"))
    }
  }
  
  it should "not allow to select a subtree using an absolute path" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", root())
      tree.selectSubtree(Root / "sub").map(_.path) should be (None)
    }
  }
  
  it should "allow to specify a template for a document using an absolute path" in {
    new TreeModel {
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot(Nil))
      val tree = treeWithSubtree(Root, "sub", "doc", root(), Some("template: /main.template.html")).copy(templates = List(template))
      val targetDoc = tree.selectDocument("sub/doc").get
      val cursor = TreeCursor(tree).children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor]
      TemplateRewriter.selectTemplate(cursor,  "html") should be (Some(template))
    }
  }
  
  it should "allow to specify a template for a document for a specific output format" in {
    new TreeModel {
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot(Nil))
      val tree = treeWithSubtree(Root, "sub", "doc", root(), Some("html.template: /main.template.html")).copy(templates = List(template))
      val targetDoc = tree.selectDocument("sub/doc").get
      val cursor = TreeCursor(tree).children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor]
      TemplateRewriter.selectTemplate(cursor,  "html") should be (Some(template))
    }
  }
  
  it should "allow to specify a template for a document using a relative path" in {
    new TreeModel {
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot(Nil))
      val tree = treeWithSubtree(Root, "sub", "doc", root(), Some("template: ../main.template.html")).copy(templates = List(template))
      val targetDoc = tree.selectDocument("sub/doc").get
      val cursor = TreeCursor(tree).children.head.asInstanceOf[TreeCursor].children.head.asInstanceOf[DocumentCursor]
      TemplateRewriter.selectTemplate(cursor,  "html") should be (Some(template))
    }
  }
  
  it should "allow to rewrite the tree using a custom rule" in {
    new TreeModel {
      val tree = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val rewritten = tree rewrite { doc => {
        case Text("a",_) => Some(Text("x"))
      }}
      val target = rewritten.selectDocument("sub/doc")
      target.get.content should be (root(p("x"), p("b"), p("c")))
    }
  }
 

}
