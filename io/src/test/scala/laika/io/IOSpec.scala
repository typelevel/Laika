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

package laika.io

import cats.implicits._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import laika.ast.{Document, DocumentTree, DocumentTreeRoot, TemplateDocument}
import laika.config.Config.ConfigResult
import laika.io.helper.RenderedTreeAssertions
import laika.io.model.RenderedTreeRoot
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait DocumentTreeAssertions extends Matchers {

  implicit class DocumentTreeRootEitherOps (val root: ConfigResult[DocumentTreeRoot]) {

    def assertEquals (expected: DocumentTreeRoot): Unit = {
      root.fold(
        err  => fail(s"rewriting failed: $err"),
        root => root.assertEquals(expected)
      )
    }

  }

  implicit class DocumentTreeEitherOps (val root: ConfigResult[DocumentTree]) {

    def assertEquals (expected: DocumentTree): Unit = {
      root.fold(
        err  => fail(s"rewriting failed: $err"),
        root => root.assertEquals(expected)
      )
    }

  }

  implicit class DocumentTreeRootOps (val root: DocumentTreeRoot) {

    def assertEquals (expected: DocumentTreeRoot): Unit = {

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
        root.staticDocuments shouldBe expected.staticDocuments
      }

    }
  }

  implicit class DocumentTreeOps (val tree: DocumentTree) {

    def assertEquals (expected: DocumentTree): Unit = {

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

      def collectTemplates (tree: DocumentTree): Seq[TemplateDocument] = tree.templates ++ tree.content.collect {
        case child: DocumentTree => collectTemplates(child)
      }.flatten

      def validateTemplates(): Unit = {
        val actualTmp = collectTemplates(tree)
        val expectedTmp = collectTemplates(expected)
        withClue("number or names of templates differs") {
          actualTmp.map(_.path) shouldBe expectedTmp.map(_.path)
        }
        actualTmp.zip(expectedTmp).foreach { case (actual, expected) =>
          withClue(s"difference in content of template '${actual.path.toString}'") {
            actual.content shouldBe expected.content
          }
        }
      }

      validateStructure()
      validateContent()
      validateTemplates()
    }

  }

  implicit class DocumentOps (val actual: Document) {

    def assertEquals (expected: Document): Unit = {
      val doc = s"of document '${actual.path.toString}'"
      withClue(s"difference in content $doc") {
        actual.content shouldBe expected.content
      }
      withClue(s"difference in title $doc") {
        actual.title shouldBe expected.title
      }
      withClue(s"difference in fragments $doc") {
        actual.fragments shouldBe expected.fragments
      }
    }

  }

}

trait IOSpec extends Matchers {
  
  /* not using AsyncWordSpec/unsafeToFuture due to poor support in sbt and IntelliJ 
     (wrong grouping of tests in reports) */
  
  implicit class AssertingTree(private val self: IO[DocumentTreeRoot]) extends DocumentTreeAssertions {

    def assertEquals(tree: DocumentTreeRoot): Unit = 
      self.map(_.assertEquals(tree)).unsafeRunSync()
    
  }

  implicit class AssertingRenderedTree(private val self: IO[RenderedTreeRoot[IO]]) extends RenderedTreeAssertions {

    def assertEquals(tree: RenderedTreeRoot[IO]): Unit =
      self.map(_.assertEquals(tree)).unsafeRunSync()

  }

  implicit class Asserting[A](private val self: IO[A]) {

    def assertEquals(a: A): Assertion = self.map(_ shouldBe a).unsafeRunSync()

    def asserting(f: A => Assertion): Assertion = self.map(f).unsafeRunSync()
    
    def assertingIO(f: A => IO[Assertion]): Assertion = self.flatMap(f).unsafeRunSync()

    def assertFailsWith (t: Throwable): Assertion = self.attempt.map(_ shouldBe Left(t)).unsafeRunSync()
    
    def assertFailsWithMessage (msg: String): Assertion = 
      self.attempt.map(_.leftMap(_.getMessage) shouldBe Left(msg)).unsafeRunSync()
  }
  
}

trait IOWordSpec extends AnyWordSpec with IOSpec 
trait IOFunSuite extends AnyFunSuite with IOSpec 


