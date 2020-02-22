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

import laika.parse.Failure
import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import laika.rst.BaseParsers.simpleRefName
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class BaseParsersSpec extends AnyWordSpec with Matchers with ParseResultHelpers with StringParserHelpers  {

  "The reference name parser" should {

    "parse a name consisting of characters" in {
      Parsing ("name") using simpleRefName should produce ("name")
    }

    "parse a name consisting of characters and digits" in {
      Parsing ("7name9") using simpleRefName should produce ("7name9")
    }

    "parse a name consisting of characters and any of the supported symbols" in {
      Parsing ("a-a_a.a:a+a") using simpleRefName should produce ("a-a_a.a:a+a")
    }

    "fail if the name starts with a symbol" in {
      Parsing ("-a_a.a:a+a") using simpleRefName should cause [Failure]
    }

    "ignore a trailing symbol" in {
      Parsing ("a-a_a.a:a+") using simpleRefName should produce ("a-a_a.a:a")
    }

    "stop parsing at two consecutive symbols" in {
      Parsing ("a-a.+a") using simpleRefName should produce ("a-a")
    }

    "stop parsing at unsupported symbols" in {
      Parsing ("a-a(a") using simpleRefName should produce ("a-a")
    }

    "parse a name containing non-ASCII characters" in {
      Parsing ("näme") using simpleRefName should produce ("näme")
    }

  }
  
}
