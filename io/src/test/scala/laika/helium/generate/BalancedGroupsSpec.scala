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
import cats.implicits._
import laika.io.IOWordSpec

class BalancedGroupsSpec extends IOWordSpec {

  trait Setup {

    val dummyOp: IO[Unit] = IO.unit

    def groupCount (numOps: Int, size: Int): IO[Seq[Int]] = {
      val ops = Vector.fill(numOps)(dummyOp)
      BalancedGroups
        .create[IO, Unit](ops, size)
        .sequence[IO, Vector[Unit]]
        .map(_.map(_.size))
    }

  }

  "The BalancedGroups utility" should {

    "create a single group when size is 1" in new Setup {
      groupCount(5, 1).assertEquals(Seq(5))
    }

    "create groups of size 1 when the number of items is lower than the specified size" in new Setup {
      groupCount(3, 5).assertEquals(Seq(1,1,1))
    }

    "create groups of size 1 when the number of items is equal to the specified size" in new Setup {
      groupCount(4, 4).assertEquals(Seq(1,1,1,1))
    }

    "create groups of variable size when the number of items is not a multiple of the specified size" in new Setup {
      groupCount(9, 4).assertEquals(Seq(3,2,2,2))
    }

    "create groups of equal size when the number of items is a multiple of the specified size" in new Setup {
      groupCount(9, 3).assertEquals(Seq(3,3,3))
    }

  }

}
