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

package laika.helium.generate

import cats.effect.IO
import munit.FunSuite

class BalancedGroupsSpec extends FunSuite {

  val dummyOp: IO[Unit] = IO.unit

  def groupCount (numOps: Int, size: Int): Seq[Int] = {
    val ops = Vector.fill(numOps)(dummyOp)
    BalancedGroups
      .create(ops, size)
      .map(_.size)
  }

  test("create a single group when size is 1") {
    assertEquals(groupCount(5, 1), Seq(5))
  }

  test("create groups of size 1 when the number of items is lower than the specified size") {
    assertEquals(groupCount(3, 5), Seq(1,1,1))
  }

  test("create groups of size 1 when the number of items is equal to the specified size") {
    assertEquals(groupCount(4, 4), Seq(1,1,1,1))
  }

  test("create groups of variable size when the number of items is not a multiple of the specified size") {
    assertEquals(groupCount(9, 4), Seq(3,2,2,2))
  }

  test("create groups of equal size when the number of items is a multiple of the specified size") {
    assertEquals(groupCount(9, 3), Seq(3,3,3))
  }

}
