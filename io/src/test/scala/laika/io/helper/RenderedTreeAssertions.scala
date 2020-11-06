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

package laika.io.helper

import cats.effect.IO
import laika.io.model.{RenderedDocument, RenderedTree, RenderedTreeRoot}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

trait RenderedTreeAssertions extends Matchers {

  implicit class DocumentTreeRootOps (val root: RenderedTreeRoot[IO]) {

    def assertEquals (expected: RenderedTreeRoot[IO]): Unit = {

      root.tree.assertEquals(expected.tree)

      withClue("tree structure differs") {
        root.allDocuments.map(_.path) shouldBe expected.allDocuments.map(_.path)
      }
    
      withClue(s"difference in cover documents") {
        (root.coverDocument, expected.coverDocument) match {
          case (Some(actual), Some(exp)) => actual.assertEquals(exp)
          case _ => ()
        }
      }

      withClue(s"difference in static documents") {
        root.staticDocuments.map(_.path) shouldBe expected.staticDocuments.map(_.path)
      }
      
    }
  }

  implicit class DocumentTreeOps (val tree: RenderedTree) {
    
    def assertEquals (expected: RenderedTree): Unit = {
      
      def validateStructure(): Assertion = {
        withClue("tree structure differs") {
          tree.allDocuments.map(_.path) shouldBe expected.allDocuments.map(_.path)
        }
      }
      
      def validateContent(): Unit = {
        tree.allDocuments.zip(expected.allDocuments).foreach { case (actual, expected) =>
          actual.assertEquals(expected)
        }
      }
      
      validateStructure()
      validateContent()
    }
    
  }

  implicit class DocumentOps (val actual: RenderedDocument) {
    
    def assertEquals (expected: RenderedDocument): Unit = {
      val doc = s"of document '${actual.path.toString}'"
      withClue(s"difference in content $doc") {
        actual.content shouldBe expected.content
      }
      withClue(s"difference in title $doc") {
        actual.title shouldBe expected.title
      }
      withClue(s"difference in sections $doc") {
        actual.sections shouldBe expected.sections
      }
    }
    
  }
  
}
