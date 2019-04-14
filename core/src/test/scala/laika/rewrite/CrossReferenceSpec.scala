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

package laika.rewrite

import laika.ast.DocumentType.Markup
import laika.ast.Path._
import laika.ast._
import laika.ast.helper.DocumentViewBuilder.{Documents => Docs, _}
import laika.ast.helper.ModelBuilder
import laika.config.OperationConfig
import org.scalatest.{FlatSpec, Matchers}

class CrossReferenceSpec extends FlatSpec 
                      with Matchers
                      with ModelBuilder {

  
  trait TreeModel {
    def rootWithLink (id: String, text: String, path: PathInfo): RootElement = rootElement(p(CrossLink(List(Text(text)), id, path)))
    def rootWithRef (id: String, text: String): RootElement = rootElement(p(LinkReference(List(Text(text)), id, s"[$id]")))
    def rootWithTarget (id: String): RootElement = rootElement(InternalLinkTarget(Id(id)))
    def rootElement (b: Block): RootElement = root(p("A"), b, p("B"))

    def treeWithDocs (path: Path, name: String, root1: RootElement, root2: RootElement): DocumentTree =
      DocumentTree(path, List(Document(path / (name+"1"), root1), Document(path / (name+"2"), root2)))
    def treeWithDoc (path: Path, name: String, root: RootElement, subtrees: List[DocumentTree] = Nil): DocumentTree =
      DocumentTree(path, List(Document(path / name, root)) ++ subtrees)
    def treeWithSubtrees (path: Path, trees: DocumentTree*): DocumentTree =
      DocumentTree(path, trees)
    
    def treeViewWithDocs (path: Path, name: String, root1: RootElement, root2: RootElement): TreeView =
      TreeView(path, List(Docs(Markup, List(
        DocumentView(path / (name+"1"), List(Content(root1.content))), 
        DocumentView(path / (name+"2"), List(Content(root2.content)))
      ))))
    
    def treeViewWithDoc (path: Path, name: String, root: RootElement, subtree: Option[TreeView] = None): TreeView =
      TreeView(path, List(Docs(Markup, List(DocumentView(path / name, List(Content(root.content)))))) ::: (subtree map (t => Subtrees(List(t)))).toList)
    
    def treeViewWithSubtrees (path: Path, trees: TreeView*): TreeView =
      TreeView(path, List(Subtrees(trees)))
      
    def rewrite (tree: DocumentTree): DocumentTree = tree.rewrite(OperationConfig.default.rewriteRules)
  }
  
  "The reference resolver" should "resolve a cross reference to a target in another document in the same directory" in {
     new TreeModel {
      val tree = treeWithDocs(Root, "doc", rootWithRef("ref", "text"), rootWithTarget("ref"))
      val treeResult = treeViewWithDocs(Root, "doc", rootWithLink("ref", "text", PathInfo(Root / "doc2",Current / "doc2")), rootWithTarget("ref"))
      viewOf(rewrite(tree)) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a parent directory" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc1", rootWithRef("ref", "text"))
      val rootTree = treeWithDoc(Root, "doc2", rootWithTarget("ref"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc1", rootWithLink("ref", "text", PathInfo(Root / "doc2", Parent(1) / "doc2")))
      val treeResult = treeViewWithDoc(Root, "doc2", rootWithTarget("ref"), Some(subtreeResult))
      viewOf(rewrite(rootTree)) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a child directory" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val rootTree = treeWithDoc(Root, "doc1", rootWithRef("ref", "text"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val treeResult = treeViewWithDoc(Root, "doc1", rootWithLink("ref", "text", PathInfo(Root / "sub" / "doc2", Current / "sub" / "doc2")), Some(subtreeResult))
      viewOf(rewrite(rootTree)) should be (treeResult)
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
      viewOf(rewrite(rootTree)) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in another document in the same directory using an explicit path" in {
     new TreeModel {
      val tree = treeWithDocs(Root, "doc", rootWithRef("doc2:ref", "text"), rootWithTarget("ref"))
      val treeResult = treeViewWithDocs(Root, "doc", rootWithLink("ref", "text", PathInfo(Root / "doc2",Current / "doc2")), rootWithTarget("ref"))
      viewOf(rewrite(tree)) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a parent directory using an explicit path" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc1", rootWithRef("../doc2:ref", "text"))
      val rootTree = treeWithDoc(Root, "doc2", rootWithTarget("ref"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc1", rootWithLink("ref", "text", PathInfo(Root / "doc2", Parent(1) / "doc2")))
      val treeResult = treeViewWithDoc(Root, "doc2", rootWithTarget("ref"), Some(subtreeResult))
      viewOf(rewrite(rootTree)) should be (treeResult)
    } 
  }
  
  it should "resolve a cross reference to a target in a document in a child directory using an explicit path" in {
     new TreeModel {
      val subtree = treeWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val rootTree = treeWithDoc(Root, "doc1", rootWithRef("sub/doc2:ref", "text"), List(subtree))
      
      val subtreeResult = treeViewWithDoc(Root / "sub", "doc2", rootWithTarget("ref"))
      val treeResult = treeViewWithDoc(Root, "doc1", rootWithLink("ref", "text", PathInfo(Root / "sub" / "doc2", Current / "sub" / "doc2")), Some(subtreeResult))
      viewOf(rewrite(rootTree)) should be (treeResult)
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
      viewOf(rewrite(rootTree)) should be (treeResult)
    } 
  }
  
}
