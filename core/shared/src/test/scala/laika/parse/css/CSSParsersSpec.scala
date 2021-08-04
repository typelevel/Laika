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

package laika.parse.css

import laika.ast.StylePredicate._
import laika.ast._
import laika.ast.sample.StyleBuilders
import laika.parse.Parser
import laika.parse.css.CSSParsers._
import laika.parse.helper.MigrationFlatSpec

class CSSParsersSpec extends MigrationFlatSpec
                        with StyleBuilders {

  
  val defaultParser: Parser[Set[StyleDeclaration]] = styleDeclarationSet
  
  def parse (input: String): Either[String, Set[StyleDeclaration]] = defaultParser.parse(input).toEither

  def result (decls: StyleDeclaration*): Either[String, Set[StyleDeclaration]] = Right(decls.toSet)
  
  
  "The CSS parser" should "parse a style with a type selector" in {
    
    val css = """Type {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(parse(css), result(styleDecl(ElementType("Type"))))
  }
  
  it should "parse a style with a class selector" in {
    
    val css = """.class {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(parse(css), result(styleDecl(StyleName("class"))))
  }
  
  it should "parse a style with an id selector" in {
    
    val css = """#id {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(parse(css), result(styleDecl(StylePredicate.Id("id"))))
  }
  
  it should "parse a style with a type and class selector" in {
    
    val css = """Type.class {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(parse(css), result(styleDecl(ElementType("Type"), StyleName("class"))))
  }
  
  it should "parse a style with a type and id selector" in {
    
    val css = """Type#id {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(parse(css), result(styleDecl(ElementType("Type"), StylePredicate.Id("id"))))
  }
  
  it should "parse a style with a child selector" in {
    
    val css = """Type .class {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(
      parse(css), 
      result(styleDecl(selector(selector(StyleName("class")), selector(ElementType("Type")), immediate = false)))
    )
  }
  
  it should "parse a style with an immediate child selector" in {
    
    val css = """Type > .class {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(
      parse(css), 
      result(styleDecl(selector(selector(StyleName("class")), selector(ElementType("Type")), immediate = true)))
    )
  }
  
  it should "parse a style with multiple child selectors" in {
    
    val css = """Type .class > #id {
      | foo: bar;
      |}""".stripMargin
    
    val top = selector(ElementType("Type"))
    val middle = selector(selector(StyleName("class")), top, immediate = false)
    val finalSelector = selector(selector(StylePredicate.Id("id")), middle, immediate = true)
    
    assertEquals(parse(css), result(styleDecl(finalSelector)))
  }
  
  it should "parse a style with multiple selectors" in {
    
    val css = """Type, .class {
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(
      parse(css), 
      result(styleDecl(StyleName("class")).increaseOrderBy(1), styleDecl(ElementType("Type")))
    )
  }
  
  it should "parse multiple style declarations" in {
    
    val css = """Type {
      |  foo: bar;
      |}
      |
      |.class {
      |  bar: foo;
      |}""".stripMargin
    
    assertEquals(
      parse(css), 
      result(styleDecl(Map("bar"->"foo"), StyleName("class")).increaseOrderBy(1), styleDecl(ElementType("Type")))
    )
  }
  
  it should "parse multiple style declarations with comments" in {
    
    val css = """/* comment 1 */
      |Type {
      |  foo: bar;
      |}
      |
      |/* comment 2 */
      |
      |.class {
      |  bar: foo;
      |}
      |
      |/* comment 3 */""".stripMargin
    
    assertEquals(
      parse(css),
      result(styleDecl(Map("bar"->"foo"), StyleName("class")).increaseOrderBy(1), styleDecl(ElementType("Type")))
    )
  }
  
  it should "parse a style with multiple comments" in {
    
    val css = """Type {
      | /* comment 1 */
      | /* comment 2 */ foo: bar;
      | /* comment 3 */
      | bar: foo; /* comment 4 */
      | /* comment 5 */
      | foo: bar;
      |}""".stripMargin
    
    assertEquals(parse(css), result(styleDecl(Map("foo"->"bar", "bar"->"foo"), ElementType("Type"))))
  }
  
  
}
