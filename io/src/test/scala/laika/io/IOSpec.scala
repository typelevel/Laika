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

import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait IOSpec extends Matchers {
  
  /* not using AsyncWordSpec/unsafeToFuture due to poor support in sbt and IntelliJ 
     (wrong grouping of tests in reports) */
  
  val executionContext: ExecutionContext = ExecutionContext.global
  
  implicit val ioContextShift: ContextShift[IO] = IO.contextShift(executionContext)
  
  implicit val ioTimer: Timer[IO] = IO.timer(executionContext)

  implicit class Asserting[A](private val self: IO[A]) {

    def assertEquals(a: A): Assertion = self.map(_ shouldBe a).unsafeRunSync()

    def asserting(f: A => Assertion): Assertion = self.map(f).unsafeRunSync()

    def assertFailsWith (t: Throwable): Assertion = self.attempt.map(_ shouldBe Left(t)).unsafeRunSync()
  }
  
}

trait IOWordSpec extends AnyWordSpec with IOSpec 
trait IOFunSuite extends AnyFunSuite with IOSpec 


