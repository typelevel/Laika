/*
 * Copyright 2013 the original author or authors.
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

package laika.tree

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import laika.tree.helper.ModelBuilder
import laika.tree.Elements._
import laika.tree.Documents._
import laika.tree.DocumentTreeHelper.{Documents => Docs}
import laika.tree.DocumentTreeHelper._

class DocumentTreeAPISpec extends FlatSpec 
                      with ShouldMatchers
                      with ModelBuilder {
  
  trait RewriteModel {
    def rootElement (b: Block) = doc(b, p("b"), p("c"))
    def treeWithDoc (path: Path, name: String, root: RootElement) =
      new DocumentTree(path, List(new Document(path / name, root)))
    def treeViewWithDoc (path: Path, name: String, root: RootElement) =
      TreeView(path, List(Docs(Markup, List(DocumentView(path / name, List(Content(root.content)))))))
    def treeWithSubtree (path: Path, treeName: String, docName: String, root: RootElement) =
      new DocumentTree(path, Nil, subtrees = List(treeWithDoc(path / treeName, docName, root)))
    def treeViewWithSubtree (path: Path, treeName: String, docName: String, root: RootElement) =
      TreeView(path, List(Subtrees(List(treeViewWithDoc(path / treeName, docName, root)))))
  }
  
  "The tree rewriter" should "give access to the root tree when rewriting a document in the root tree" in {
    new RewriteModel {
      val tree = treeWithDoc(Root, "doc", rootElement(p("a")))
      val treeResult = treeViewWithDoc(Root, "doc", rootElement(p("/")))
      viewOf(tree rewrite { context => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(context.root.path.toString)) 
      }}) should be (treeResult)
    }
  }
  
  it should "give access to the parent tree when rewriting a document in the root tree" in {
    new RewriteModel {
      val tree = treeWithDoc(Root, "doc", rootElement(p("a")))
      val treeResult = treeViewWithDoc(Root, "doc", rootElement(p("/")))
      viewOf(tree rewrite { context => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(context.parent.path.toString)) 
      }}) should be (treeResult)
    }
  }
  
  it should "give access to the root tree when rewriting a document in a child tree" in {
    new RewriteModel {
      val tree = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val treeResult = treeViewWithSubtree(Root, "sub", "doc", rootElement(p("/")))
      viewOf(tree rewrite { context => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(context.root.path.toString)) 
      }}) should be (treeResult)
    }
  }
  
  it should "give access to the parent tree when rewriting a document in a child tree" in {
    new RewriteModel {
      val tree = treeWithSubtree(Root, "sub", "doc", rootElement(p("a")))
      val treeResult = treeViewWithSubtree(Root, "sub", "doc", rootElement(p("/sub")))
      viewOf(tree rewrite { context => { 
        case Paragraph(Seq(Text("a",_)),_) => Some(p(context.parent.path.toString)) 
      }}) should be (treeResult)
    }
  }
 

}