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

package laika.rst

import laika.rst.BaseParsers.simpleRefName
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class BaseParsersSpec extends AnyWordSpec with Matchers {

  "The reference name parser" should {

    "parse a name consisting of characters" in {
       simpleRefName.parse("name").toEither shouldBe Right("name")
    }
      
    "parse a name consisting of characters and digits" in {
      simpleRefName.parse("7name9").toEither shouldBe Right("7name9")
    }

    "parse a name consisting of characters and any of the supported symbols" in {
      simpleRefName.parse("a-a_a.a:a+a").toEither shouldBe Right("a-a_a.a:a+a")
    }

    "fail if the name starts with a symbol" in {
      simpleRefName.parse("-a_a.a:a+a").toEither.isLeft shouldBe true
    }

    "ignore a trailing symbol" in {
      simpleRefName.parse("a-a_a.a:a+").toEither shouldBe Right("a-a_a.a:a")
    }

    "stop parsing at two consecutive symbols" in {
      simpleRefName.parse("a-a.+a").toEither shouldBe Right("a-a")
    }

    "stop parsing at unsupported symbols" in {
      simpleRefName.parse("a-a(a").toEither shouldBe Right("a-a")
    }

    "parse a name containing non-ASCII characters" in {
      simpleRefName.parse("näme").toEither shouldBe Right("näme")
    }

  }
  
}
