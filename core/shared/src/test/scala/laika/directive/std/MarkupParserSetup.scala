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

package laika.directive.std

import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast.{Document, Element, MessageFilter, Path, RootElement}
import laika.format.Markdown

/**
  * @author Jens Halm
  */
trait MarkupParserSetup {

  lazy val markupParser = MarkupParser.of(Markdown).failOnMessages(MessageFilter.None).build

  def parse (input: String, path: Path = Root / "doc"): Document = markupParser.parse(input, path).toOption.get
  def parseUnresolved (input: String, path: Path = Root / "doc"): Document = markupParser.parseUnresolved(input, path).toOption.get.document

  def parseWithFragments (input: String, path: Path = Root / "doc"): (Map[String,Element], RootElement) = {
    val doc = parse(input, path)
    (doc.fragments, doc.content)
  }
  
}
