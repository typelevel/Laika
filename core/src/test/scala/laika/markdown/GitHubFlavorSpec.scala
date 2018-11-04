/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.markdown

import laika.ast.RootElement
import laika.ast.helper.ModelBuilder
import laika.config.OperationConfig
import laika.format.Markdown
import laika.markdown.github.GitHubFlavor
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParser
import org.scalatest.{Matchers, WordSpec}

class GitHubFlavorSpec extends WordSpec
  with Matchers
  with ParseResultHelpers
  with DefaultParserHelpers[RootElement]
  with ModelBuilder {

  val rootParser = new RootParser(Markdown, OperationConfig(Markdown.extensions)
    .withBundles(Seq(GitHubFlavor)).markupExtensions)

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  "The Markdown parser with GitHubFlavor extension" should {

    "parse standard Markdown" in {
      val input = """aaa
                    |bbb
                    |
                    |# CCC""".stripMargin
      Parsing (input) should produce (root( p("aaa\nbbb"), h(1, "CCC", "ccc")))
    }

  }

}
