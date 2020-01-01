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

package laika.io.runtime

import cats.effect.IO
import cats.implicits._
import laika.io.IOSpec

class BatchRuntimeSpec extends IOSpec {

  trait Setup {

    val dummyOp: IO[Unit] = IO.unit

    def batchCount(numOps: Int, parallelism: Int): IO[Seq[Int]] = {
      val ops = Vector.fill(numOps)(dummyOp)
      BatchRuntime
        .createBatches[IO, Unit](ops, parallelism)
        .sequence[IO, Vector[Unit]]
        .map(_.map(_.size))
    }

  }

  "The Executor" should {

    "create a single batch when parallelism is 1" in new Setup {
      batchCount(5, 1).assertEquals(Seq(5))
    }

    "create batches of size 1 when the number of operations is lower than the parallelism" in new Setup {
      batchCount(3, 5).assertEquals(Seq(1,1,1))
    }

    "create batches of size 1 when the number of operations is equal to the parallelism" in new Setup {
      batchCount(4, 4).assertEquals(Seq(1,1,1,1))
    }

    "create batches of variable size when the number of operations is not a multiple of the parallelism" in new Setup {
      batchCount(9, 4).assertEquals(Seq(3,2,2,2))
    }

    "create batches of equal size when the number of operations is a multiple of the parallelism" in new Setup {
      batchCount(9, 3).assertEquals(Seq(3,3,3))
    }

  }

}
