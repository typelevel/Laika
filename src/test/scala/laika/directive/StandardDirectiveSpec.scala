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

package laika.directive

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import laika.tree.helper.ModelBuilder
import laika.api.Parse
import laika.parse.markdown.Markdown
import laika.tree.Elements._
import laika.template.ParseTemplate
import laika.template.DefaultTemplate
import com.typesafe.config.ConfigFactory
import laika.tree.Templates.TemplateDocument
import laika.tree.Documents._
import scala.collection.JavaConversions._
import laika.tree.Templates.TemplateSpanSequence

class StandardDirectiveSpec extends FlatSpec
                            with ShouldMatchers
                            with ModelBuilder {

  
  def parse (input: String) = (Parse as Markdown fromString input)

  def parseWithFragments (input: String) = {
    val doc = parse(input)
    (doc.fragments, doc.content)
  }
  
  def parseTemplate (input: String) = (ParseTemplate as DefaultTemplate fromString input).content
  
  def parseTemplateWithConfig (input: String, config: String) = {
    val root = parseTemplate(input)
    val template = new TemplateDocument(Root, root)
    (template rewrite DocumentContext(new Document(Root, doc(), config = ConfigFactory.parseString(config)))).content
  }
  

  "The fragment directive" should "parse a fragment with a single paragraph" in {
    val input = """aa
      |
      |@:fragment foo:
      |  Fragment Text
      |
      |bb""".stripMargin
    parseWithFragments(input) should be ((Map("foo" -> BlockSequence(List(p("Fragment Text")))),doc(p("aa"),p("bb"))))
  }
  
  it should "parse a fragment with a two paragraphs" in {
    val input = """aa
      |
      |@:fragment foo:
      |  Line 1
      |
      |  Line 2
      |
      |bb""".stripMargin
    parseWithFragments(input) should be ((Map("foo" -> BlockSequence(List(p("Line 1"), p("Line 2")))),doc(p("aa"),p("bb"))))
  }
  
  
  "The for directive" should "process the default body once if the referenced object is a map" in {
    val input = """aaa @:for "config.person": { {{name}} {{age}} } bbb"""
    val config = "person: { name: Mary, age: 35 }" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(
        tt(" "),tt("Mary"),tt(" "),tt("35"),tt(" ")
      )),
      tt(" bbb")
    )))  
  } 
  
  it should "process the default body multiple times if the referenced object is a list" in {
    val input = """aaa @:for "config.persons": { {{name}} {{age}} } bbb"""
    val config = "persons: [{ name: Mary, age: 35 },{ name: Lucy, age: 32 },{ name: Anna, age: 42 }]" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(
        TemplateSpanSequence(List(
          tt(" "),tt("Mary"),tt(" "),tt("35"),tt(" ")
        )),
        TemplateSpanSequence(List(
          tt(" "),tt("Lucy"),tt(" "),tt("32"),tt(" ")
        )),
        TemplateSpanSequence(List(
          tt(" "),tt("Anna"),tt(" "),tt("42"),tt(" ")
        ))
      )),
      tt(" bbb")
    )))  
  } 
  
  it should "not process the default body if the referenced object is an empty collection" in {
    val input = """aaa @:for "config.persons": { {{name}} {{age}} } bbb"""
    val config = "persons: []" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(Nil),
      tt(" bbb")
    )))  
  } 
  
  it should "process the fallback body if the referenced object is an empty collection" in {
    val input = """aaa @:for "config.persons": { {{name}} {{age}} } ~empty: { none } bbb"""
    val config = "persons: []" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" none "))),
      tt(" bbb")
    )))  
  } 
  
  it should "process the default body once if the referenced object is a scalar value" in {
    val input = """aaa @:for "config.person": { text } bbb"""
    val config = "person: Mary" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" text "))),
      tt(" bbb")
    )))  
  } 
  
  "The if directive" should "process the default body once if the referenced object is the string 'true'" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "monday: true" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" text "))),
      tt(" bbb")
    )))  
  } 
  
  it should "process the default body once if the referenced object is the string 'on'" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "monday: on" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" text "))),
      tt(" bbb")
    )))  
  } 
  
  it should "not process the default body if the referenced object does not exist" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "tuesday: on" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(Nil),
      tt(" bbb")
    )))  
  } 
  
  it should "not process the default body if the referenced object is not a string recognized as true" in {
    val input = """aaa @:if "config.monday": { text } bbb"""
    val config = "monday: off" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(Nil),
      tt(" bbb")
    )))  
  } 
  
  it should "process the fallback body if the referenced object does not exist" in {
    val input = """aaa @:if "config.monday": { text } ~else: { none } bbb"""
    val config = "tuesday: on" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" none "))),
      tt(" bbb")
    )))  
  } 
  
  it should "process the fallback body if the referenced object is not a string recognized as true" in {
    val input = """aaa @:if "config.monday": { text } ~else: { none } bbb"""
    val config = "monday: off" 
    parseTemplateWithConfig(input, config) should be (doc(tRoot(
      tt("aaa "),
      TemplateSpanSequence(List(tt(" none "))),
      tt(" bbb")
    )))  
  } 
  
  
  trait TreeModel {
    
    val pathUnderTest = Root / "sub2" / "doc7"
    
    def header (level: Int, title: Int, style: String = "section") =
      Header(level,List(Text("Section "+title)),Id("title"+title) + Styles(style))
      
    val sectionsWithoutTitle = RootElement(
      header(1,1) ::
      header(2,2) ::
      header(1,3) ::
      header(2,4) ::
      Nil
    )
    
    def docs (path: Path, nums: Int*) = nums map { 
      n => new Document(path / ("doc"+n), sectionsWithoutTitle, config = ConfigFactory.parseString("title: Doc "+n))
    }

    def buildTree (template: TemplateDocument, markup: Document) = {
      new DocumentTree(Root, docs(Root, 1,2), templates = List(template), subtrees = List(
        new DocumentTree(Root / "sub1", docs(Root / "sub1",3,4), config = Some(ConfigFactory.parseString("title: Tree 1"))),
        new DocumentTree(Root / "sub2", docs(Root / "sub2",5,6) ++ List(markup), config = Some(ConfigFactory.parseString("title: Tree 2")))
      ))
    }
    
    def parseAndRewrite (template: String, markup: String) = {
      val templateDoc = new TemplateDocument(Root / "test.html", parseTemplate(template))
      val doc = new Document(pathUnderTest, parse(markup).content, config = ConfigFactory.parseString("title: Doc 7, template: /test.html"))
      val tree = buildTree(templateDoc, doc)
      tree.rewrite.selectDocument(Current / "sub2" / "doc7").get.content
    }
    
    def markup = """# Headline 1
        |
        |# Headline 2""".stripMargin
  }
  
  trait TocModel {
    import laika.tree.Elements.TitledBlock
    
    val treeUnderTest = Root / "sub2"
    
    def title = "Contents"
    
    def sectionCrossLink (path: Path, section: Int, level: Int) = 
      p(CrossLink(List(txt("Section "+section)), "title"+section, PathInfo(path, path.relativeTo(treeUnderTest)), options = Styles("toc","level"+level)))
      
    def leafLink (path: Path, section: Int, level: Int) = 
      BulletListItem(List(sectionCrossLink(path, section, level)), StringBullet("*"))
      
    def sectionNode (path: Path, section: Int, level: Int) = 
      BulletListItem(List(sectionCrossLink(path, section, level), BulletList(List(leafLink(path, section+1, level+1)), StringBullet("*"))), StringBullet("*"))
      
    def docCrossLink (path: Path, doc: Int, level: Int) =
      p(CrossLink(List(txt("Doc "+doc)), "", PathInfo(path, path.relativeTo(treeUnderTest)), options = Styles("toc","level"+level)))
      
    def docList (path: Path, doc: Int, level: Int) = 
      BulletListItem(List(docCrossLink(path, doc, level), BulletList(List(
        sectionNode(path, 1, level+1),
        sectionNode(path, 3, level+1)
      ), StringBullet("*"))), StringBullet("*"))
      
    def docListFirstLevel (path: Path, doc: Int, level: Int) = 
      BulletListItem(List(docCrossLink(path, doc, level), BulletList(List(
        leafLink(path, 1, level+1),
        leafLink(path, 3, level+1)
      ), StringBullet("*"))), StringBullet("*"))
      
    def internalLink (section: Int, level: Int) = 
      BulletListItem(List(
        p(InternalLink(List(txt("Headline "+section)), "headline-"+section, options = Styles("toc","level"+level)))
      ), StringBullet("*"))
      
    def extraDoc (treeNum: Int, level: Int) = 
      if (treeNum == 1) Nil
      else List(BulletListItem(List(
        Paragraph(List(txt("Doc 7")), Styles("toc","level"+level,"active")), 
        BulletList(List(
          internalLink(1, level+1),
          internalLink(2, level+1)
        ), StringBullet("*"))), StringBullet("*"))) 
      
    def treeList (treeNum: Int, docStart: Int) = 
      BulletListItem(List(
        Paragraph(List(Text("Tree "+treeNum)), Styles("toc","level1")), 
        BulletList(List(
          docList(Root / ("sub"+treeNum) / ("doc"+docStart),     docStart,   2),
          docList(Root / ("sub"+treeNum) / ("doc"+(docStart+1)), docStart+1, 2)
        ) ++ extraDoc(treeNum,2), StringBullet("*"))
      ), StringBullet("*"))
      
    def rootList = 
      BulletList(List(
        docList(Root / ("doc1"), 1, 1),
        docList(Root / ("doc2"), 2, 1),
        treeList(1, 3),
        treeList(2, 5)
      ), StringBullet("*"))
      
    def currentList =
      BulletList(List(
        docList(Root / ("sub2") / ("doc5"), 5, 1),
        docList(Root / ("sub2") / ("doc6"), 6, 1)
      ) ++ extraDoc(2,1), StringBullet("*"))
      
    def firstTree =
      BulletList(List(
        docList(Root / ("sub1") / ("doc3"), 3, 1),
        docList(Root / ("sub1") / ("doc4"), 4, 1)
      ), StringBullet("*"))
      
    def firstTreeFirstLevel =
      BulletList(List(
        docListFirstLevel(Root / ("sub1") / ("doc3"), 3, 1),
        docListFirstLevel(Root / ("sub1") / ("doc4"), 4, 1)
      ), StringBullet("*"))
     
    def currentDoc =
      BulletList(List(
        internalLink(1, 1),
        internalLink(2, 1)
      ), StringBullet("*"))
      
    def result (list: BulletList) = doc(tRoot(
      tt("aaa "),
      tElem(TitledBlock(List(txt(title)), List(list), options=Styles("toc"))),
      tt(" bbb "),
      tElem(doc(
        Section(Header(1, List(txt("Headline 1")), Id("headline-1") + Styles("section")), Nil),
        Section(Header(1, List(txt("Headline 2")), Id("headline-2") + Styles("section")), Nil)
      ))
    ))
    
    def markupTocResult = doc(tRoot(
      tElem(doc(
        TitledBlock(List(txt(title)), List(currentDoc), options=Styles("toc")),
        Section(Header(1, List(txt("Headline 1")), Id("headline-1") + Styles("section")), Nil),
        Section(Header(1, List(txt("Headline 2")), Id("headline-2") + Styles("section")), Nil)
      ))
    ))
  }

  "The template toc directive" should "produce a table of content starting from the root tree" in {
    new TreeModel with TocModel {
      
      val template = """aaa @:toc. bbb {{document.content}}"""
      
      parseAndRewrite(template, markup) should be (result(rootList))  
    }
  } 
  
  it should "produce a table of content starting from the root tree with an explicit title" in {
    new TreeModel with TocModel {
      
      override val title = "Some Title"
      
      val template = """aaa @:toc title="Some Title". bbb {{document.content}}"""
      
      parseAndRewrite(template, markup) should be (result(rootList))  
    }
  } 
  
  it should "produce a table of content starting from the current tree" in {
    new TreeModel with TocModel {
      
      val template = """aaa @:toc root=#currentTree. bbb {{document.content}}"""
      
      parseAndRewrite(template, markup) should be (result(currentList))  
    }
  } 
  
  it should "produce a table of content starting from the root of the current document" in {
    new TreeModel with TocModel {
      
      val template = """aaa @:toc root=#currentDocument. bbb {{document.content}}"""
      
      parseAndRewrite(template, markup) should be (result(currentDoc))  
    }
  } 
  
  it should "produce a table of content starting from an explicit absolute path" in {
    new TreeModel with TocModel {
      
      val template = """aaa @:toc root=/sub1. bbb {{document.content}}"""
      
      parseAndRewrite(template, markup) should be (result(firstTree))  
    }
  } 
  
  it should "produce a table of content starting from an explicit relative path" in {
    new TreeModel with TocModel {
      
      val template = """aaa @:toc root="../sub1". bbb {{document.content}}"""
      
      parseAndRewrite(template, markup) should be (result(firstTree))  
    }
  } 
  
  it should "produce a table of content starting from an explicit relative path with depth 2" in {
    new TreeModel with TocModel {
      
      val template = """aaa @:toc root="../sub1" depth=2. bbb {{document.content}}"""
      
      parseAndRewrite(template, markup) should be (result(firstTreeFirstLevel))  
    }
  } 
  
  "The document toc directive"  should "produce a table of content starting from the root of the current document" in {
    new TreeModel with TocModel {
      
      override val markup = """@:toc.
        |
        |# Headline 1
        |
        |# Headline 2""".stripMargin
      
      val template = """{{document.content}}"""
      
      parseAndRewrite(template, markup) should be (markupTocResult)  
    }
  } 
  
  
}