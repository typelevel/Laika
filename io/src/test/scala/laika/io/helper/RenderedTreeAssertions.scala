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
import munit.Assertions

trait RenderedTreeAssertions extends Assertions { self =>

  implicit class DocumentTreeRootOps (val root: RenderedTreeRoot[IO]) {

    def assertEquals (expected: RenderedTreeRoot[IO]): Unit = {

      root.tree.assertEquals(expected.tree)

      (root.coverDocument, expected.coverDocument) match {
        case (Some(actual), Some(exp)) => actual.assertEquals(exp)
        case _ => ()
      }

      self.assertEquals(
        root.staticDocuments.map(_.path),
        expected.staticDocuments.map(_.path),
        s"difference in static documents"
      )
    }
  }

  implicit class DocumentTreeOps (val tree: RenderedTree) {
    
    def assertEquals (expected: RenderedTree): Unit = {
      
      self.assertEquals(
        tree.allDocuments.map(_.path), 
        expected.allDocuments.map(_.path), 
        "tree structure differs"
      )
    
      tree.allDocuments.zip(expected.allDocuments).foreach { case (actual, expected) =>
        actual.assertEquals(expected)
      }
      
    }
    
  }

  implicit class DocumentOps (val actual: RenderedDocument) {
    
    def assertEquals (expected: RenderedDocument): Unit = {
      val doc = s"of document '${actual.path.toString}'"
      self.assertEquals(actual.content, expected.content, s"difference in content $doc")
      self.assertEquals(actual.title, expected.title, s"difference in title $doc")
      self.assertEquals(actual.sections, expected.sections, s"difference in sections $doc")
    }
    
  }
  
}
