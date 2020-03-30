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
  
package laika.api

import laika.ast._
import laika.format._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TransformAPISpec extends AnyFlatSpec 
                       with Matchers {

   
  val input = """# Title äöü
    |
    |text zzz *foo*""".stripMargin 
  
  val output = """RootElement - Blocks: 2
    |. Title(Id(title) + Styles(title)) - Spans: 1
    |. . Text - 'Title äöü'
    |. Paragraph - Spans: 2
    |. . Text - 'text zzz '
    |. . Emphasized - Spans: 1
    |. . . Text - 'foo'""".stripMargin
    
  val builder = Transformer.from(Markdown).to(AST)
  
  
  "The Transform API" should "transform from string to string" in {
    builder.build.transform(input).toOption.get should be (output)
  }
  
  it should "allow to override the default renderer for specific element types" in {
    val modifiedOutput = output.replaceAllLiterally(". Text", ". String")
    val transformCustom = builder rendering { case (_, Text(content,_)) => s"String - '$content'" }
    transformCustom.build.transform(input).toOption.get should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule" in {
    val modifiedOutput = output.replaceAllLiterally("zzz", "yyy")
    val transformCustom = builder usingSpanRule { case Text("text zzz ",_) => Replace(Text("text yyy ")) }
    transformCustom.build.transform(input).toOption.get should be (modifiedOutput)
  }
  
  it should "allow to specify multiple rewrite rules" in {
    val modifiedOutput = output.replaceAllLiterally("zzz", "new").replaceAllLiterally("foo", "bar")
    val transformCustom = builder usingSpanRule { case Text("foo" ,_) => Replace(Text("bar")) } usingSpanRule
                                                { case Text("text zzz ",_)    => Replace(Text("text new ")) }
    transformCustom.build.transform(input).toOption.get should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule that depends on the document" in {
    val modifiedOutput = output.replaceAllLiterally("zzz", "2")
    val transformCustom = builder creatingRule { cursor => RewriteRules.forSpans { 
      case Text("text zzz ",_) => Replace(Text("text " + cursor.target.content.content.length + " ")) 
    }}
    transformCustom.build.transform(input).toOption.get should be (modifiedOutput)
  }

}
  