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

package laika.ast.sample

import laika.ast.{ Document, DocumentTree, DocumentTreeRoot, TemplateDocument }
import laika.api.config.Config.ConfigResult

trait MunitDocumentTreeAssertions extends munit.Assertions { self =>

  implicit class DocumentTreeRootEitherOps(val root: ConfigResult[DocumentTreeRoot]) {

    def assertEquals(expected: DocumentTreeRoot): Unit = {
      root.fold(
        err => fail(s"rewriting failed: $err"),
        root => root.assertEquals(expected)
      )
    }

  }

  implicit class DocumentTreeEitherOps(val root: ConfigResult[DocumentTree]) {

    def assertEquals(expected: DocumentTree): Unit = {
      root.fold(
        err => fail(s"rewriting failed: $err"),
        root => root.assertEquals(expected)
      )
    }

  }

  implicit class DocumentTreeRootOps(val root: DocumentTreeRoot) {

    def assertEquals(expected: DocumentTreeRoot): Unit = {

      root.tree.assertEquals(expected.tree)

      self.assertEquals(
        root.allDocuments.map(_.path),
        expected.allDocuments.map(_.path),
        "tree structure differs"
      )

      (root.coverDocument, expected.coverDocument) match {
        case (Some(actual), Some(exp)) => actual.assertEquals(exp)
        case _                         => ()
      }

      self.assertEquals(
        root.staticDocuments,
        expected.staticDocuments,
        s"difference in static documents"
      )

    }

  }

  implicit class DocumentTreeOps(val tree: DocumentTree) {

    def assertEquals(expected: DocumentTree): Unit = {

      def validateStructure(): Unit = {
        self.assertEquals(
          tree.allDocuments.map(_.path),
          expected.allDocuments.map(_.path),
          "tree structure differs"
        )
      }

      def validateContent(): Unit = {
        tree.allDocuments.zip(expected.allDocuments).foreach { case (actual, expected) =>
          actual.assertEquals(expected)
        }
      }

      def collectTemplates(tree: DocumentTree): Seq[TemplateDocument] =
        tree.templates ++ (tree.content.collect { case child: DocumentTree =>
          collectTemplates(child)
        }.flatten)

      def validateTemplates(): Unit = {
        val actualTmp   = collectTemplates(tree)
        val expectedTmp = collectTemplates(expected)
        self.assertEquals(
          actualTmp.map(_.path),
          expectedTmp.map(_.path),
          "number or names of templates differs"
        )
        actualTmp.zip(expectedTmp).foreach { case (actual, expected) =>
          self.assertEquals(
            actual.content,
            expected.content,
            s"difference in content of template '${actual.path.toString}'"
          )
        }
      }

      validateStructure()
      validateContent()
      validateTemplates()
    }

  }

  implicit class DocumentOps(val actual: Document) {

    def assertEquals(expected: Document): Unit = {
      val doc = s"of document '${actual.path.toString}'"
      self.assertEquals(actual.content, expected.content, s"difference in content $doc")
      self.assertEquals(actual.title, expected.title, s"difference in title $doc")
      self.assertEquals(actual.fragments, expected.fragments, s"difference in fragments $doc")
    }

  }

}
