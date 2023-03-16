/*
 * Copyright 2012-2021 the original author or authors.
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

import cats.effect.IO
import laika.ast.DocumentTreeRoot
import laika.ast.sample.DocumentTreeAssertions
import laika.io.helper.RenderedTreeAssertions
import laika.io.model.RenderedTreeRoot
import munit.{ Assertions, CatsEffectAssertions }

trait IOTreeAssertions extends CatsEffectAssertions with DocumentTreeAssertions {
  self: Assertions =>

  implicit class AssertingTree(private val self: IO[DocumentTreeRoot])
      extends DocumentTreeAssertions {

    def assertEquals(tree: DocumentTreeRoot): IO[Unit] =
      self.map(_.assertEquals(tree))

  }

  implicit class AssertingRenderedTree(private val self: IO[RenderedTreeRoot[IO]])
      extends RenderedTreeAssertions {

    def assertEquals(tree: RenderedTreeRoot[IO]): IO[Unit] =
      self.map(_.assertEquals(tree))

  }

  implicit class Asserting[A](private val self: IO[A]) {

    def assertFailsWith(t: Throwable): IO[Unit] = self.attempt.assertEquals(Left(t))

  }

}
