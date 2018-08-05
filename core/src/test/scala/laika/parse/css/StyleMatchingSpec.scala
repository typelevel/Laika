/*
 * Copyright 2014-2016 the original author or authors.
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

package laika.parse.css

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import laika.ast._
import laika.ast.Path.Root
import laika.ast.StylePredicate._

class StyleMatchingSpec extends FlatSpec 
                        with Matchers 
                        with StyleBuilders {

  
  val styles = new StyleDeclarationSet(Set(Root), Set(
      styleDecl(Map("selector"->"type", "foo"->"bar"), ElementType("Paragraph")),
      styleDecl(Map("selector"->"class"), StyleName("class")).increaseOrderBy(1),
      styleDecl(Map("selector"->"id"), StylePredicate.Id("id")).increaseOrderBy(2),
      styleDecl(Map("selector"->"child"), selector(selector(StyleName("child")), selector(ElementType("Paragraph")), false)).increaseOrderBy(3),
      styleDecl(Map("selector"->"immediateChild"), selector(selector(StyleName("immediateChild")), selector(ElementType("Paragraph")), true)).increaseOrderBy(4),
      styleDecl(Map("selector"->"twoChildren"), {
        val top = selector(ElementType("Paragraph"))
        val middle = selector(selector(StyleName("middle")), top, false)
        selector(selector(StylePredicate.Id("bottom")), middle, true)
      }).increaseOrderBy(5),
      styleDecl(Map("selector"->"twoSelectors", "bar"->"foo"), selector(ElementType("Paragraph"), StyleName("twoSelectors"))).increaseOrderBy(6),
      styleDecl(Map("selector"->"pos7", "foo"->"bar"), StyleName("pos")).increaseOrderBy(7),
      styleDecl(Map("selector"->"pos8", "bar"->"foo"), StyleName("pos")).increaseOrderBy(8)
  ))
  
  
  "The style matching logic" should "not collect any styles when no selector matches" in {
    styles.collectStyles(Text(""), Nil) should be (Map())
  }
  
  it should "collect styles with matching type selectors" in {
    styles.collectStyles(Paragraph(Nil), Nil) should be (Map("selector"->"type","foo"->"bar"))
  }
  
  it should "collect styles with matching class selectors" in {
    styles.collectStyles(Text("", laika.ast.Styles("class")), Nil) should be (Map("selector"->"class"))
  }
  
  it should "collect styles with matching id selectors" in {
    styles.collectStyles(Text("", laika.ast.Id("id")), Nil) should be (Map("selector"->"id"))
  }
  
  it should "collect styles with matching type and class selectors" in {
    styles.collectStyles(Paragraph(Nil, laika.ast.Styles("twoSelectors")), Nil) should be (Map("selector"->"twoSelectors","foo"->"bar","bar"->"foo"))
  }
  
  it should "collect styles with matching parent and child selectors" in {
    styles.collectStyles(Text("", laika.ast.Styles("child")), List(Paragraph(Nil))) should be (Map("selector"->"child"))
  }
  
  it should "not collect any styles when the parent selector does not match" in {
    styles.collectStyles(Text("", laika.ast.Styles("child")), List(Emphasized(Nil))) should be (Map())
  }
  
  it should "collect styles with matching parent and immediate child selectors" in {
    styles.collectStyles(Text("", laika.ast.Styles("immediateChild")), List(Paragraph(Nil))) should be (Map("selector"->"immediateChild"))
  }
  
  it should "not collect any styles when the matching parent selector is not an immediate parent" in {
    styles.collectStyles(Text("", laika.ast.Styles("immediateChild")), List(Emphasized(Nil), Paragraph(Nil))) should be (Map())
  }
  
  it should "collect styles with two matching parent selectors" in {
    styles.collectStyles(Text("", laika.ast.Id("bottom")), List(Emphasized(Nil, laika.ast.Styles("middle")), Paragraph(Nil))) should be (Map("selector"->"twoChildren"))
  }
  
  it should "not collect any styles when one of the parent selectors does not match" in {
    styles.collectStyles(Text("", laika.ast.Id("bottom")), List(Emphasized(Nil, laika.ast.Styles("middle")), Header(1, Nil))) should be (Map())
  }
  
  it should "should apply styles based on order if two declarations have the same specificity" in {
    styles.collectStyles(Text("", laika.ast.Styles("pos")), Nil) should be (Map("selector"->"pos8","foo"->"bar","bar"->"foo"))
  }
  
  
}
