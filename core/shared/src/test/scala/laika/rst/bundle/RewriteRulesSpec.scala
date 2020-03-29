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
import laika.ast.helper.ModelBuilder
import laika.format.ReStructuredText
import laika.rst.ast.{CustomizedTextRole, InterpretedText, SubstitutionDefinition, SubstitutionReference}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
 
class RewriteRulesSpec extends AnyFlatSpec 
                  with Matchers
                  with ModelBuilder {

  
  def rewritten (root: RootElement): RootElement = {
    val doc = Document(Path.Root, root)
    val rules = OperationConfig.default.withBundlesFor(ReStructuredText).rewriteRulesFor(doc)
    doc.rewrite(rules).content
  }
  
  def invalidSpan (message: String, fallback: String): InvalidSpan = InvalidElement(message, fallback).asSpan
      
      
  "The rewrite rules for substitutions" should "replace a single reference with the target span" in {
    val rootElem = root(p(SubstitutionReference("id")), SubstitutionDefinition("id", Text("subst")))
    rewritten (rootElem) should be (root(p("subst")))
  }
  
  it should "replace multiple occurrences of the same reference with the same target span" in {
    val rootElem = root(p(SubstitutionReference("id"), Text(" foo "), SubstitutionReference("id")), SubstitutionDefinition("id", Text("subst")))
    rewritten (rootElem) should be (root(p(Text("subst"),Text(" foo "),Text("subst"))))
  }
  
  it should "replace a reference with an unknown substitution id with an invalid span" in {
    val rootElem = root(p(SubstitutionReference("id1")), SubstitutionDefinition("id2", Text("subst")))
    rewritten (rootElem) should be (root(p(invalidSpan("unknown substitution id: id1", "|id1|"))))
  }
  
  
  "The rewrite rules for interpreted text roles" should "replace a single reference with the result of applying the role function" in {
    val rootElem = root(p(InterpretedText("id", "foo","")), CustomizedTextRole("id", s => Text(s":$s:")))
    rewritten (rootElem) should be (root(p(":foo:")))
  }
  
  it should "replace multiple references with the result of applying corresponding role functions" in {
    val rootElem = root(p(InterpretedText("id1", "foo",""),InterpretedText("id2", "bar",""),InterpretedText("id1", "baz","")), 
        CustomizedTextRole("id1", s => Text(":"+s+":")), CustomizedTextRole("id2", s => Text(s".$s.")))
    rewritten (rootElem) should be (root(p(Text(":foo:"),Text(".bar."),Text(":baz:"))))
  }
  
  it should "replace an unknown text role with an invalid span" in {
    val rootElem = root(p(InterpretedText("id1", "foo", "")), CustomizedTextRole("id2", s => Text(s".$s.")))
    rewritten (rootElem) should be (root(p(invalidSpan("unknown text role: id1", "`foo`"))))
  }
  
  
}
