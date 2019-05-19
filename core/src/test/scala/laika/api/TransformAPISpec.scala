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
  
package laika.api

import java.io._

import laika.api.Transform.TransformMappedOutput
import laika.ast.DocumentType.Static
import laika.ast.Path.Root
import laika.ast._
import laika.bundle.{BundleProvider, ExtensionBundle}
import laika.directive.Templates
import laika.format._
import laika.parse.Parser
import laika.parse.text.TextParsers
import laika.render.TextFormatter
import org.scalatest.{FlatSpec, Matchers}


class TransformAPISpec extends FlatSpec 
                       with Matchers {

   
  val input = """# Title äöü
    |
    |text""".stripMargin 
  
  val output = """RootElement - Blocks: 2
    |. Title(Id(title) + Styles(title)) - Spans: 1
    |. . Text - 'Title äöü'
    |. Paragraph - Spans: 1
    |. . Text - 'text'""".stripMargin
    
  val transform = Transform from Markdown to AST
  
  
  "The Transform API" should "transform from string to string" in {
    (transform transform input) should be (output)
  }
  
  it should "allow to override the default renderer for specific element types" in {
    val modifiedOutput = output.replaceAllLiterally(". Text", ". String")
    val transformCustom = transform rendering { case (_, Text(content,_)) => s"String - '$content'" }
    (transformCustom transform input) should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz")
    val transformCustom = transform usingSpanRule { case Text("Title äöü",_) => Replace(Text("Title zzz")) }
    (transformCustom transform input) should be (modifiedOutput)
  }
  
  it should "allow to specify multiple rewrite rules" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz").replaceAllLiterally("text", "new")
    val transformCustom = transform usingSpanRule { case Text("Title äöü",_) => Replace(Text("Title zzz")) } usingSpanRule
                                                  { case Text("text",_)      => Replace(Text("new")) }
    (transformCustom transform input) should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule that depends on the document" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "2")
    val transformCustom = transform creatingRule { cursor => RewriteRules.forSpans { 
      case Text("Title äöü",_) => Replace(Text("Title " + cursor.target.content.content.length)) 
    }}
    (transformCustom transform input) should be (modifiedOutput)
  }


}
  