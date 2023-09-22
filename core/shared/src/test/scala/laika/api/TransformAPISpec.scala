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
import laika.ast.RewriteAction.Replace
import laika.config.LaikaKeys
import laika.format._
import munit.FunSuite

class TransformAPISpec extends FunSuite {

  private val input = """# Title äöü
                        |
                        |text zzz *foo*""".stripMargin

  private val output = """RootElement - Blocks: 2
                         |. Title(Id(title-äöü) + Styles(title)) - Spans: 1
                         |. . Text - 'Title äöü'
                         |. Paragraph - Spans: 2
                         |. . Text - 'text zzz '
                         |. . Emphasized - Spans: 1
                         |. . . Text - 'foo'""".stripMargin

  private val builder =
    Transformer.from(Markdown).to(AST).withConfigValue(LaikaKeys.firstHeaderAsTitle, true)

  test("string to string") {
    assertEquals(builder.build.transform(input), Right(output))
  }

  test("empty string") {
    assertEquals(builder.build.transform(""), Right("RootElement - Blocks: 0"))
  }

  test("render override for a specific element type") {
    val modifiedOutput  = output.replace(". Text", ". String")
    val transformCustom = builder.rendering { case (_, Text(content, _)) => s"String - '$content'" }
    assertEquals(transformCustom.build.transform(input), Right(modifiedOutput))
  }

  test("custom rewrite rule") {
    val modifiedOutput  = output.replace("zzz", "yyy")
    val transformCustom = builder.usingSpanRule { case Text("text zzz ", _) =>
      Replace(Text("text yyy "))
    }
    assertEquals(transformCustom.build.transform(input), Right(modifiedOutput))
  }

  test("multiple rewrite rules") {
    val modifiedOutput  = output.replace("zzz", "new").replace("foo", "bar")
    val transformCustom = builder
      .usingSpanRule { case Text("foo", _) => Replace(Text("bar")) }
      .usingSpanRule { case Text("text zzz ", _) => Replace(Text("text new ")) }
    assertEquals(transformCustom.build.transform(input), Right(modifiedOutput))
  }

  test("process a Replace followed by a Retain") {
    val modifiedOutput  = output.replace("foo", "bar")
    val transformCustom = builder
      .usingSpanRule { case Text("foo", _) => Replace(Text("bar")) }
      .usingSpanRule { case Text(_, _) => RewriteAction.Retain }
    assertEquals(transformCustom.build.transform(input), Right(modifiedOutput))
  }

  test("custom rewrite rule that depends on the document") {
    val modifiedOutput  = output.replace("zzz", "2")
    val transformCustom = builder.buildingRules { cursor =>
      Right(RewriteRules.forSpans { case Text("text zzz ", _) =>
        Replace(Text("text " + cursor.target.content.content.length + " "))
      })
    }
    assertEquals(transformCustom.build.transform(input), Right(modifiedOutput))
  }

  test("process rules with access to a document cursor in a later phase") {
    val modifiedOutput  = output.replace("foo", "baz")
    val transformCustom = builder
      .buildingRules { _ =>
        Right(RewriteRules.forSpans { case Text("bar", _) =>
          Replace(Text("baz"))
        })
      }
      .usingSpanRule { case Text("foo", _) => Replace(Text("bar")) }
    assertEquals(transformCustom.build.transform(input), Right(modifiedOutput))
  }

  test("do not validate or translate URLs when transforming a single string - Markdown") {
    val input    = "An inline ![image](image.png)."
    val expected = """<p>An inline <img src="image.png" alt="image">.</p>"""
    val result   = Transformer.from(Markdown).to(HTML).build.transform(input)
    assertEquals(result, Right(expected))
  }

  test("do not validate or translate URLs when transforming a single string - reStructuredText") {
    val input       = "An image:\n\n.. image:: image.png"
    val expected    =
      """<p>An image:</p>
        |<p><img src="image.png"></p>""".stripMargin
    val transformer = Transformer
      .from(ReStructuredText)
      .to(HTML)
      .build
    assertEquals(transformer.transform(input), Right(expected))
  }

}
