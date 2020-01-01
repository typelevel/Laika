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
import org.scalatest.{FlatSpec, Matchers}
 
class RewriteRulesSpec extends FlatSpec 
                  with Matchers
                  with ModelBuilder {

  
  def rewritten (root: RootElement): RootElement = {
    val doc = Document(Path.Root, root)
    val rules = OperationConfig.default.withBundlesFor(ReStructuredText).rewriteRules(DocumentCursor(doc))
    doc.rewrite(rules).content
  }
  
  def invalidSpan (message: String, fallback: String): InvalidSpan = InvalidElement(message, fallback).asSpan
      
      
  "The rewrite rules for substitutions" should "replace a single reference with the target span" in {
    val rootElem = root(p(SubstitutionReference("id")), SubstitutionDefinition("id", txt("subst")))
    rewritten (rootElem) should be (root(p("subst")))
  }
  
  it should "replace multiple occurrences of the same reference with the same target span" in {
    val rootElem = root(p(SubstitutionReference("id"), txt(" foo "), SubstitutionReference("id")), SubstitutionDefinition("id", txt("subst")))
    rewritten (rootElem) should be (root(p(txt("subst"),txt(" foo "),txt("subst"))))
  }
  
  it should "replace a reference with an unknown substitution id with an invalid span" in {
    val rootElem = root(p(SubstitutionReference("id1")), SubstitutionDefinition("id2", txt("subst")))
    rewritten (rootElem) should be (root(p(invalidSpan("unknown substitution id: id1", "|id1|"))))
  }
  
  
  "The rewrite rules for interpreted text roles" should "replace a single reference with the result of applying the role function" in {
    val rootElem = root(p(InterpretedText("id", "foo","")), CustomizedTextRole("id", s => txt(s":$s:")))
    rewritten (rootElem) should be (root(p(":foo:")))
  }
  
  it should "replace multiple references with the result of applying corresponding role functions" in {
    val rootElem = root(p(InterpretedText("id1", "foo",""),InterpretedText("id2", "bar",""),InterpretedText("id1", "baz","")), 
        CustomizedTextRole("id1", s => txt(":"+s+":")), CustomizedTextRole("id2", s => txt(s".$s.")))
    rewritten (rootElem) should be (root(p(txt(":foo:"),txt(".bar."),txt(":baz:"))))
  }
  
  it should "replace an unknown text role with an invalid span" in {
    val rootElem = root(p(InterpretedText("id1", "foo", "")), CustomizedTextRole("id2", s => txt(s".$s.")))
    rewritten (rootElem) should be (root(p(invalidSpan("unknown text role: id1", "`foo`"))))
  }
  
  
}
