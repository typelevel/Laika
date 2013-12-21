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

class CrossReferenceSpec extends FlatSpec 
                      with ShouldMatchers
                      with ModelBuilder {

  
  trait TreeModel {
    def rootWithLink (id: String, text: String, path: PathInfo) = rootElement(p(CrossLink(List(Text(text)), id, path)))
    def rootWithRef (id: String, text: String) = rootElement(p(LinkReference(List(Text(text)), id, s"[$id]")))
    def rootWithTarget (id: String) = rootElement(InternalLinkTarget(Id(id)))
    def rootElement (b: Block) = root(p("A"), b, p("B"))

    def treeWithDocs (path: Path, name: String, root1: RootElement, root2: RootElement) =
      new DocumentTree(path, List(new Document(path / (name+"1"), root1), new Document(path / (name+"2"), root2)))
    def treeWithDoc (path: Path, name: String, root: RootElement, subtrees: List[DocumentTree] = Nil) =
      new DocumentTree(path, List(new Document(path / name, root)), subtrees = subtrees)
    
    def treeViewWithDocs (path: Path, name: String, root1: RootElement, root2: RootElement) =
      TreeView(path, List(Docs(Markup, List(
        DocumentView(path / (name+"1"), List(Content(root1.content))), 
        DocumentView(path / (name+"2"), List(Content(root2.content)))
      ))))
    def treeViewWithDoc (path: Path, name: String, root: RootElement, subtree: Option[TreeView] = None) =
      TreeView(path, List(Docs(Markup, List(DocumentView(path / name, List(Content(root.content)))))) ::: (subtree map (t => Subtrees(List(t)))).toList)
    
    def treeWithSubtrees (path: Path, trees: DocumentTree*) =
      new DocumentTree(path, Nil, subtrees = trees)
    def treeViewWithSubtrees (path: Path, trees: TreeView*) =
      TreeView(path, List(Subtrees(trees)))
  }
  
  "The reference resolver" should "resolve a cross reference to a target in another document in the same directory" in {
     new TreeModel {
      val tree = treeWithDocs(Root, "doc", rootWithRef("ref", "text"), rootWithTarget("ref"))
      val treeResult = treeViewWithDocs(Root, "doc", rootWithLink("ref", "text", PathInfo(Root / "doc2",Current / "doc2")), rootWithTarget("ref"))
      viewOf(tree.rewrite) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a parent directory" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc1", rootWithRef("ref", "text"))
      val rootTree = treeWithDoc(Root, "doc2", rootWithTarget("ref"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc1", rootWithLink("ref", "text", PathInfo(Root / "doc2", Parent(1) / "doc2")))
      val treeResult = treeViewWithDoc(Root, "doc2", rootWithTarget("ref"), Some(subtreeResult))
      viewOf(rootTree.rewrite) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a child directory" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val rootTree = treeWithDoc(Root, "doc1", rootWithRef("ref", "text"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val treeResult = treeViewWithDoc(Root, "doc1", rootWithLink("ref", "text", PathInfo(Root / "sub" / "doc2", Current / "sub" / "doc2")), Some(subtreeResult))
      viewOf(rootTree.rewrite) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a sibling directory" in {
     new TreeModel {
      val subtree1 = treeWithDoc(Root / "sub1", "doc1", rootWithRef("ref", "text"))
      val subtree2 = treeWithDoc(Root / "sub2", "doc2", rootWithTarget("ref"))
      val rootTree = treeWithSubtrees(Root, subtree1, subtree2)
      
      val subtreeResult1 = treeViewWithDoc(Root / "sub1", "doc1", rootWithLink("ref", "text", PathInfo(Root / "sub2" / "doc2", Parent(1) / "sub2" / "doc2")))
      val subtreeResult2 = treeViewWithDoc(Root / "sub2", "doc2", rootWithTarget("ref"))
      val treeResult = treeViewWithSubtrees(Root, subtreeResult1, subtreeResult2)
      viewOf(rootTree.rewrite) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in another document in the same directory using an explicit path" in {
     new TreeModel {
      val tree = treeWithDocs(Root, "doc", rootWithRef("doc2:ref", "text"), rootWithTarget("ref"))
      val treeResult = treeViewWithDocs(Root, "doc", rootWithLink("ref", "text", PathInfo(Root / "doc2",Current / "doc2")), rootWithTarget("ref"))
      viewOf(tree.rewrite) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a parent directory using an explicit path" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc1", rootWithRef("../doc2:ref", "text"))
      val rootTree = treeWithDoc(Root, "doc2", rootWithTarget("ref"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc1", rootWithLink("ref", "text", PathInfo(Root / "doc2", Parent(1) / "doc2")))
      val treeResult = treeViewWithDoc(Root, "doc2", rootWithTarget("ref"), Some(subtreeResult))
      viewOf(rootTree.rewrite) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a child directory using an explicit path" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val rootTree = treeWithDoc(Root, "doc1", rootWithRef("sub/doc2:ref", "text"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val treeResult = treeViewWithDoc(Root, "doc1", rootWithLink("ref", "text", PathInfo(Root / "sub" / "doc2", Current / "sub" / "doc2")), Some(subtreeResult))
      viewOf(rootTree.rewrite) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a sibling directory using an explicit path" in {
     new TreeModel {
      val subtree1 = treeWithDoc(Root / "sub1", "doc1", rootWithRef("../sub2/doc2:ref", "text"))
      val subtree2 = treeWithDoc(Root / "sub2", "doc2", rootWithTarget("ref"))
      val rootTree = treeWithSubtrees(Root, subtree1, subtree2)
      
      val subtreeResult1 = treeViewWithDoc(Root / "sub1", "doc1", rootWithLink("ref", "text", PathInfo(Root / "sub2" / "doc2", Parent(1) / "sub2" / "doc2")))
      val subtreeResult2 = treeViewWithDoc(Root / "sub2", "doc2", rootWithTarget("ref"))
      val treeResult = treeViewWithSubtrees(Root, subtreeResult1, subtreeResult2)
      viewOf(rootTree.rewrite) should be (treeResult)
    } 
  }
  
  
}