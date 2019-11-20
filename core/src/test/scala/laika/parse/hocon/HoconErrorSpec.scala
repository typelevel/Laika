/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.parse.hocon

import laika.config.{ConfigParser, ConfigParserErrors}
import org.scalatest.{Assertion, Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class HoconErrorSpec extends WordSpec with Matchers {
  
  def parseAndValidate(input: String, expectedMessage: String): Assertion = {

    ConfigParser.parse(input).resolve match {
      case Right(result) => fail(s"Unexpected parser success: $result")
      case Left(ConfigParserErrors(errors)) =>
        errors.size shouldBe 1
        errors.head.toString shouldBe expectedMessage
      case Left(other) => fail(s"Unexpected parser error: $other")
    }

  }

  "The error messages of the HOCON parser" should {
    
    "report an invalid escape sequence" in {
      val input =
        """
          |a = "foo \x bar"
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.11] failure: Invalid escape sequence: \x
          |
          |a = "foo \x bar"
          |          ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "report a missing closing quote" in {
      val input =
        """
          |a = "foo bar
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.13] failure: Expected closing quote
          |
          |a = "foo bar
          |            ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

  }
  
}
