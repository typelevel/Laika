/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.api

import laika.execute.BatchExecutor
import org.scalatest.{Matchers, WordSpec}

class ExecutorSpec extends WordSpec with Matchers {

  trait Setup {

    val dummyOp: () => Unit = () => ()

    def batchCount(numOps: Int, parallelism: Int): Seq[Int] = {
      val ops = List.fill(numOps)(dummyOp)
      BatchExecutor.createBatches(ops, parallelism).map(_.size)
    }

  }


  "The Executor" should {

    "create a single batch when parallelism is 1" in new Setup {
      batchCount(5, 1) shouldBe Seq(5)
    }

    "create batches of size 1 when the number of operations is lower than the parallelism" in new Setup {
      batchCount(3, 5) shouldBe Seq(1,1,1)
    }

    "create batches of size 1 when the number of operations is equal to the parallelism" in new Setup {
      batchCount(4, 4) shouldBe Seq(1,1,1,1)
    }

    "create batches of variable size when the number of operations is not a multiple of the parallelism" in new Setup {
      batchCount(9, 4) shouldBe Seq(3,2,2,2)
    }

    "create batches of equal size when the number of operations is a multiple of the parallelism" in new Setup {
      batchCount(9, 3) shouldBe Seq(3,3,3)
    }

  }

}
