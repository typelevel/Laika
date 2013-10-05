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
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import laika.tree.Elements._
import laika.tree.Documents._
import laika.parse.rst.Elements._
import laika.tree.helper.ModelBuilder
 
class RewriteRulesSpec extends FlatSpec 
                  with ShouldMatchers
                  with ModelBuilder {

  
  def rewritten (root: RootElement) = {
    val doc = new Document(Root, Nil, DocumentInfo(), root, rewriteRules = List(RewriteRules))
    doc.rewrite.content
  }
  
  def invalidSpan (message: String, fallback: String) =
      InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), Text(fallback))
      
      
  "The rewrite rules for substitutions" should "replace a single reference with the target span" in {
    val document = doc(p(SubstitutionReference("id")), SubstitutionDefinition("id", txt("subst")))
    rewritten (document) should be (doc(p("subst")))
  }
  
  it should "replace multiple occurrences of the same reference with the same target span" in {
    val document = doc(p(SubstitutionReference("id"), txt(" foo "), SubstitutionReference("id")), SubstitutionDefinition("id", txt("subst")))
    rewritten (document) should be (doc(p(txt("subst"),txt(" foo "),txt("subst"))))
  }
  
  it should "replace a reference with an unknown substitution id with an invalid span" in {
    val document = doc(p(SubstitutionReference("id1")), SubstitutionDefinition("id2", txt("subst")))
    rewritten (document) should be (doc(p(invalidSpan("unknown substitution id: id1", "|id1|"))))
  }
  
  
  "The rewrite rules for interpreted text roles" should "replace a single reference with the result of applying the role function" in {
    val document = doc(p(InterpretedText("id", "foo","")), CustomizedTextRole("id", s => txt(":"+s+":")))
    rewritten (document) should be (doc(p(":foo:")))
  }
  
  it should "replace multiple references with the result of applying corresponding role functions" in {
    val document = doc(p(InterpretedText("id1", "foo",""),InterpretedText("id2", "bar",""),InterpretedText("id1", "baz","")), 
        CustomizedTextRole("id1", s => txt(":"+s+":")), CustomizedTextRole("id2", s => txt("."+s+".")))
    rewritten (document) should be (doc(p(txt(":foo:"),txt(".bar."),txt(":baz:"))))
  }
  
  it should "replace an unknown text role with an invalid span" in {
    val document = doc(p(InterpretedText("id1", "foo", "")), CustomizedTextRole("id2", s => txt("."+s+".")))
    rewritten (document) should be (doc(p(invalidSpan("unknown text role: id1", "`foo`"))))
  }
  
  
}