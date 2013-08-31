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

package laika.parse.rst.ext

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import laika.tree.helper.ModelBuilder
import laika.tree.Elements._
import laika.parse.rst.ReStructuredText
import laika.api.Parse

/**
 * @author Jens Halm
 */
class StandardBlockDirectivesSpec extends FlatSpec 
                                  with ShouldMatchers 
                                  with ModelBuilder {

   val simplePars = List(p("1st Para"), p("2nd Para"))
   
   def parse (input: String) = (Parse as ReStructuredText fromString input).content
  
  "The compound directive" should "parse a sequence of two paragraphs" in {
    val input = """.. compound::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (BlockSequence(simplePars, Styles("compound")))
    parse(input) should be (result)
  }
  
  it should "allow to set an id for the sequence" in {
    val input = """.. compound::
      | :name: foo
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (BlockSequence(simplePars, Styles("compound") + Id("foo")))
    parse(input) should be (result)
  }
  
  it should "allow to set a style for the sequence" in {
    val input = """.. compound::
      | :class: foo
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (BlockSequence(simplePars, Styles("foo", "compound")))
    parse(input) should be (result)
  }
  
  
  "The container directive" should "parse a sequence of two paragraphs" in {
    val input = """.. container::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (BlockSequence(simplePars))
    parse(input) should be (result)
  }
  
  it should "parse a sequence of two paragraphs with two custom styles" in {
    val input = """.. container:: foo bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (BlockSequence(simplePars, Styles("foo", "bar")))
    parse(input) should be (result)
  }
  
  
  "The admonition directives" should "parse a generic admonition" in {
    val input = """.. admonition:: TITLE
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("TITLE")), simplePars, Styles("admonition")))
    parse(input) should be (result)
  }
  
  it should "allow to set id and style for the admonition" in {
    val input = """.. admonition:: TITLE
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("TITLE")), simplePars, Id("foo") + Styles("bar","admonition")))
    parse(input) should be (result)
  }
  
  it should "parse the attention admonition" in {
    val input = """.. attention::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Attention!")), simplePars, Styles("attention")))
    parse(input) should be (result)
  }
  
  it should "allow to set id and style for the attention admonition" in {
    val input = """.. attention::
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Attention!")), simplePars, Id("foo") + Styles("bar","attention")))
    parse(input) should be (result)
  }
  
  it should "correctly parse nested blocks and inline markup" in {
    val input = """.. attention::
      |
      | 1st *Para*
      |
      |  2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Attention!")), List(p(txt("1st "),em("Para")), quote("2nd Para")), Styles("attention")))
    parse(input) should be (result)
  }
  
  it should "parse the caution admonition" in {
    val input = """.. caution::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Caution!")), simplePars, Styles("caution")))
    parse(input) should be (result)
  }
  
  it should "parse the danger admonition" in {
    val input = """.. danger::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("!DANGER!")), simplePars, Styles("danger")))
    parse(input) should be (result)
  }
  
  it should "parse the error admonition" in {
    val input = """.. error::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Error")), simplePars, Styles("error")))
    parse(input) should be (result)
  }
  
  it should "parse the hint admonition" in {
    val input = """.. hint::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Hint")), simplePars, Styles("hint")))
    parse(input) should be (result)
  }
  
  it should "parse the important admonition" in {
    val input = """.. important::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Important")), simplePars, Styles("important")))
    parse(input) should be (result)
  }
  
  it should "parse the note admonition" in {
    val input = """.. note::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Note")), simplePars, Styles("note")))
    parse(input) should be (result)
  }
  
  it should "parse the tip admonition" in {
    val input = """.. tip::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Tip")), simplePars, Styles("tip")))
    parse(input) should be (result)
  }
  
  it should "parse the warning admonition" in {
    val input = """.. warning::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Warning")), simplePars, Styles("warning")))
    parse(input) should be (result)
  }
  
  it should "match the name of the directive case-insensitively" in {
    val input = """.. Warning::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("Warning")), simplePars, Styles("warning")))
    parse(input) should be (result)
  }
  
  
  "The topic directive" should "parse a sequence of two paragraphs" in {
    val input = """.. topic:: TITLE
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("TITLE")), simplePars, Styles("topic")))
    parse(input) should be (result)
  }
  
  it should "allow to set id and style for the admonition" in {
    val input = """.. topic:: TITLE
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("TITLE")), simplePars, Id("foo") + Styles("bar","topic")))
    parse(input) should be (result)
  }
  
  
  "The sidebar directive" should "parse a sequence of two paragraphs" in {
    val input = """.. sidebar:: TITLE
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("TITLE")), simplePars, Styles("sidebar")))
    parse(input) should be (result)
  }
  
  it should "allow to set id and style for the admonition" in {
    val input = """.. sidebar:: TITLE
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("TITLE")), simplePars, Id("foo") + Styles("bar","sidebar")))
    parse(input) should be (result)
  }
  
  it should "allow to set the id and a subtitle for the admonition" in {
    val input = """.. sidebar:: TITLE
      | :name: foo
      | :subtitle: some *text*
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    val result = doc (TitledBlock(List(txt("TITLE")), Paragraph(List(txt("some "),em("text")), Styles("subtitle")) :: simplePars, 
        Id("foo") + Styles("sidebar")))
    parse(input) should be (result)
  }
  
  
  "The rubric directive" should "parse spans without other options" in {
    val input = """.. rubric:: some *text*"""
    val result = doc (Paragraph(List(txt("some "),em("text")), Styles("rubric")))
    parse(input) should be (result)
  }
  
  it should "allow to set id and styles" in {
    val input = """.. rubric:: some *text*
      | :name: foo
      | :class: bar""".stripMargin
    val result = doc (Paragraph(List(txt("some "),em("text")), Id("foo") + Styles("bar","rubric")))
    parse(input) should be (result)
  }
  
  
  "The epigraph directive" should "parse a single line" in {
    val input = """.. epigraph:: some *text*"""
    val result = doc (QuotedBlock(List(p(txt("some "),em("text"))), Nil, Styles("epigraph")))
    parse(input) should be (result)
  }
  
  it should "parse multiple lines" in {
    val input = """.. epigraph:: 
      |
      | some *text*
      | some more""".stripMargin
    val result = doc (QuotedBlock(List(p(txt("some "),em("text"),txt("\nsome more"))), Nil, Styles("epigraph")))
    parse(input) should be (result)
  }
  
  it should "support attributions" in {
    val input = """.. epigraph:: 
      |
      | some *text*
      | some more
      | 
      | -- attr""".stripMargin
    val result = doc (QuotedBlock(List(p(txt("some "),em("text"),txt("\nsome more"))), List(Text("attr",Styles("attribution"))), Styles("epigraph")))
    parse(input) should be (result)
  }
  
  "The highlights directive" should "parse a single line" in {
    val input = """.. highlights:: some *text*"""
    val result = doc (QuotedBlock(List(p(txt("some "),em("text"))), Nil, Styles("highlights")))
    parse(input) should be (result)
  }
  
  "The pull-quote directive" should "parse a single line" in {
    val input = """.. pull-quote:: some *text*"""
    val result = doc (QuotedBlock(List(p(txt("some "),em("text"))), Nil, Styles("pull-quote")))
    parse(input) should be (result)
  }
  
  
  "The parsed-literal directive" should "parse multiple lines" in {
    val input = """.. parsed-literal::
      | 
      | some *text*
      | some more""".stripMargin
    val result = doc (ParsedLiteralBlock(List(txt("some "),em("text"),txt("\nsome more"))))
    parse(input) should be (result)
  }
  
  it should "allow to set id and style" in {
    val input = """.. parsed-literal::
      | :name: foo
      | :class: bar
      |
      | some *text*
      | some more""".stripMargin
    val result = doc (ParsedLiteralBlock(List(txt("some "),em("text"),txt("\nsome more")), Id("foo") + Styles("bar")))
    parse(input) should be (result)
  }
  
  it should "parse multiple lines separated by spaces, preserving indentation" in {
    val input = """.. parsed-literal::
      | 
      | some *text*
      | some more
      |
      |  indented""".stripMargin
    val result = doc (ParsedLiteralBlock(List(txt("some "),em("text"),txt("\nsome more\n\n indented"))))
    parse(input) should be (result)
  }
  
  
  "The code directive" should "parse multiple lines" in {
    val input = """.. code:: banana-script
      | 
      | some banana
      | some more""".stripMargin
    val result = doc (CodeBlock("banana-script", List(txt("some banana\nsome more"))))
    parse(input) should be (result)
  }
  
  it should "allow to set id and style" in {
    val input = """.. code:: banana-script
      | :name: foo
      | :class: bar
      |
      | some banana
      | some more""".stripMargin
    val result = doc (CodeBlock("banana-script", List(txt("some banana\nsome more")), Id("foo") + Styles("bar")))
    parse(input) should be (result)
  }
  
  it should "parse multiple lines separated by spaces, preserving indentation" in {
    val input = """.. code:: banana-script
      | 
      | some banana
      | some more
      |
      |  indented""".stripMargin
    val result = doc (CodeBlock("banana-script", List(txt("some banana\nsome more\n\n indented"))))
    parse(input) should be (result)
  }
  
  
  "The table directive" should "parse a grid table with caption" in {
    val input = """.. table:: *caption*
      | 
      | +---+---+
      | | a | b |
      | +---+---+
      | | c | d |
      | +---+---+""".stripMargin
    val result = doc (table(strrow("a","b"), strrow("c","d")).copy(caption = Caption(List(em("caption")))))
    parse(input) should be (result)
  }
  
  it should "parse a simple table with caption" in {
    val input = """.. table:: *caption*
      | 
      | ===  ===
      |  a    b
      |  c    d
      | ===  ===""".stripMargin
    val result = doc (table(strrow("a","b"), strrow("c","d")).copy(caption = Caption(List(em("caption")))))
    parse(input) should be (result)
  }
  
  it should "parse a simple table without caption" in {
    val input = """.. table::
      | 
      | ===  ===
      |  a    b
      |  c    d
      | ===  ===""".stripMargin
    val result = doc (table(strrow("a","b"), strrow("c","d")))
    parse(input) should be (result)
  }
  
  
  "The image directive" should "parse the URI argument" in {
    val input = """.. image:: picture.jpg"""
    val result = doc (p(img("", "picture.jpg")))
    parse(input) should be (result)
  }
  
  it should "support the alt option" in {
    val input = """.. image:: picture.jpg
      | :alt: alt""".stripMargin
    val result = doc (p(img("alt", "picture.jpg")))
    parse(input) should be (result)
  }
  
  it should "support the target option with a simple reference" in {
    val input = """.. image:: picture.jpg
      | :target: ref_
      |
      |.. _ref: http://foo.com/""".stripMargin
    val result = doc (p(ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    parse(input) should be (result)
  }
  
  it should "support the target option with a phrase reference" in {
    val input = """.. image:: picture.jpg
      | :target: `some ref`_
      |
      |.. _`some ref`: http://foo.com/""".stripMargin
    val result = doc (p(ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    parse(input) should be (result)
  }
  
  it should "support the target option with a uri" in {
    val input = """.. image:: picture.jpg
      | :target: http://foo.com/""".stripMargin
    val result = doc (p(ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    parse(input) should be (result)
  }
  
  it should "support the class option" in {
    val input = """.. image:: picture.jpg
      | :class: foo""".stripMargin
    val result = doc (p(Image("","picture.jpg",options=Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The figure directive" should "parse the URI argument" in {
    val input = """.. figure:: picture.jpg"""
    val result = doc (Figure(img("", "picture.jpg"),Nil,Nil))
    parse(input) should be (result)
  }
  
  it should "support a caption" in {
    val input = """.. figure:: picture.jpg
      |
      | This is the *caption*""".stripMargin
    val result = doc (Figure(img("", "picture.jpg"), List(txt("This is the "),em("caption")), Nil))
    parse(input) should be (result)
  }
  
  it should "support a caption and a legend" in {
    val input = """.. figure:: picture.jpg
      |
      | This is the *caption*
      |
      | And this is the legend""".stripMargin
    val result = doc (Figure(img("", "picture.jpg"), List(txt("This is the "),em("caption")), List(p("And this is the legend"))))
    parse(input) should be (result)
  }
  
  it should "support the class option for the figure and the image" in {
    val input = """.. figure:: picture.jpg
      | :class: img
      | :figclass: fig""".stripMargin
    val result = doc (Figure(Image("", "picture.jpg", options=Styles("img")), Nil, Nil, Styles("fig")))
    parse(input) should be (result)
  }
  
  it should "support the target option with a simple reference and a caption" in {
    val input = """.. figure:: picture.jpg
      | :target: ref_
      |
      | This is the *caption*
      |
      |.. _ref: http://foo.com/""".stripMargin
    val result = doc (Figure(ExternalLink(List(img("", "picture.jpg")), "http://foo.com/"), List(txt("This is the "),em("caption")), Nil))
    parse(input) should be (result)
  }
  
  
  "The raw directive" should "support raw input with one format" in {
    val input = """.. raw:: format
      |
      | some input
      |
      | some more""".stripMargin
    val result = doc (RawContent(List("format"), "some input\n\nsome more"))
    (Parse as ReStructuredText.withRawContent fromString input).content should be (result)
  }
  
  
}