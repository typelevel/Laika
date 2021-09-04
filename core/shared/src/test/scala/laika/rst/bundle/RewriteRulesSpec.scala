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

package laika.rst.bundle

import laika.api.builder.OperationConfig
import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.config.Config.ConfigResult
import laika.format.ReStructuredText
import laika.parse.GeneratedSource
import laika.rst.ast.{CustomizedTextRole, InterpretedText, SubstitutionDefinition, SubstitutionReference}
import munit.FunSuite
 
class RewriteRulesSpec extends FunSuite with ParagraphCompanionShortcuts {

  
  def rewritten (root: RootElement): ConfigResult[RootElement] = {
    val doc = Document(Path.Root, root)
    OperationConfig.default
      .withBundlesFor(ReStructuredText)
      .rewriteRulesFor(doc)
      .flatMap(doc.rewrite)
      .map(_.content)
  }
  
  def invalidSpan (message: String): InvalidSpan = InvalidSpan(message, GeneratedSource)
  
  def run (input: RootElement, expected: RootElement): Unit =
    assertEquals(rewritten(input), Right(expected))
      
      
  test("substitutions - replace a single reference with the target span") {
    val input = RootElement(p(SubstitutionReference("id", GeneratedSource)), SubstitutionDefinition("id", Text("subst")))
    val expected = RootElement(p("subst"))
    run(input, expected)
  }
  
  test("substitutions - replace multiple occurrences of the same reference with the same target span") {
    val input = RootElement(
      p(
        SubstitutionReference("id", GeneratedSource), 
        Text(" foo "), 
        SubstitutionReference("id", GeneratedSource)
      ), 
      SubstitutionDefinition("id", Text("subst"))
    )
    val expected = RootElement(p(Text("subst"),Text(" foo "),Text("subst")))
    run(input, expected)
  }
  
  test("substitutions - replace a reference with an unknown substitution id with an invalid span") {
    val input = RootElement(
      p(SubstitutionReference("id1", GeneratedSource)), 
      SubstitutionDefinition("id2", Text("subst"))
    )
    val expected = RootElement(p(invalidSpan("unknown substitution id: id1")))
    run(input, expected)
  }
  
  
  test("interpreted text roles - replace a single reference with the result of applying the role function") {
    val input = RootElement(p(InterpretedText("id", "foo", GeneratedSource)), CustomizedTextRole("id", s => Text(s":$s:")))
    val expected = RootElement(p(":foo:"))
    run(input, expected)
  }
  
  test("interpreted text roles - replace multiple references with the result of applying corresponding role functions") {
    val input = RootElement(
      p(
        InterpretedText("id1", "foo", GeneratedSource),
        InterpretedText("id2", "bar", GeneratedSource),
        InterpretedText("id1", "baz", GeneratedSource)
      ), 
      CustomizedTextRole("id1", s => Text(":"+s+":")),
      CustomizedTextRole("id2", s => Text(s".$s."))
    )
    val expected = RootElement(p(Text(":foo:"),Text(".bar."),Text(":baz:")))
    run(input, expected)
  }
  
  test("interpreted text roles - replace an unknown text role with an invalid span") {
    val input = RootElement(
      p(InterpretedText("id1", "foo", GeneratedSource)), 
      CustomizedTextRole("id2", s => Text(s".$s."))
    )
    val expected = RootElement(p(invalidSpan("unknown text role: id1")))
    run(input, expected)
  }
  
  
}
