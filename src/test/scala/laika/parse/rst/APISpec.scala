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
  
package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import laika.api._
import laika.tree.Elements._
import laika.parse.rst.Elements._
import laika.parse.rst.Directives._
import laika.parse.rst.TextRoles._
import laika.parse.rst.Directives.Parts._
import laika.tree.helper.ModelBuilder

class APISpec extends FlatSpec 
                 with ShouldMatchers
                 with ModelBuilder {
  
  
  "The API" should "support registration of block directives" in {
    val directives = List(
      BlockDirective("oneArg")(argument() map p),
      BlockDirective("twoArgs")((argument() ~ argument()) { (arg1,arg2) => p(arg1+arg2) })
    )
    val input = """.. oneArg:: arg
      |
      |.. twoArgs:: arg arg""".stripMargin
    Parse as (ReStructuredText withBlockDirectives (directives:_*)) fromString input should be (doc (p("arg"),p("argarg")))
  }
  
  it should "support registration of span directives" in {
    val directives = List(
      SpanDirective("oneArg")(argument() map Text),
      SpanDirective("twoArgs")((argument() ~ argument()) { (arg1,arg2) => Text(arg1+arg2) })
    )
    val input = """foo |one| foo |two|
      |
      |.. |one| oneArg:: arg
      |
      |.. |two| twoArgs:: arg arg""".stripMargin
    Parse as (ReStructuredText withSpanDirectives (directives:_*)) fromString input should be (doc 
        (p(txt("foo "), txt("arg"), txt(" foo "), txt("argarg"))))
  }
  
  it should "support registration of text roles" in {
    import laika.parse.rst.TextRoles.{Parts=>P}
    val roles = List(
      TextRole("oneArg", "foo1")(P.field("name")) { (res,text) =>
       txt(res+text)
      },
      TextRole("twoArgs", "foo2")
        ((P.field("name1") ~ P.field("name2")) { (arg1,arg2) => arg1+arg2 }) 
        { (res,text) => txt(res+text) }
    )
    val input = """foo `one`:one: foo :two:`two`
      |
      |.. role::one(oneArg)
      | :name: val
      |
      |.. role::two(twoArgs)
      | :name1: val1
      | :name2: val2""".stripMargin
    Parse as (ReStructuredText withTextRoles (roles:_*)) fromString input should be (doc 
        (p(txt("foo "), txt("valone"), txt(" foo "), txt("val1val2two"))))
  }
  

}