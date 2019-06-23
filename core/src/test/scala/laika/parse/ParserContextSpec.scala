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

package laika.parse

import java.io.StringReader

import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class ParserContextSpec extends WordSpec with Matchers {


  val context = ParserContext("abc\ndef")


  "The ParserContext" should {

    "not be at the end of input after creation" in {
      context.atEnd shouldBe false
    }

    "be at the end of input after consuming all characters" in {
      context.consume(7).atEnd shouldBe true
    }

    "should indicate the full input length as remaining after creation" in {
      context.remaining shouldBe 7
    }

    "should indicate zero remaining after consuming all characters" in {
      context.consume(7).remaining shouldBe 0
    }

    "should return the first character after creation" in {
      context.char shouldBe 'a'
    }

    "should return the last character after consuming all but one characters" in {
      context.consume(6).char shouldBe 'f'
    }

    "should return a character relative to the current offset" in {
      context.consume(1).charAt(1) shouldBe 'c'
    }

    "should capture the input from the beginning" in {
      context.capture(3) shouldBe "abc"
    }

    "should capture the input from the current position" in {
      context.consume(4).capture(3) shouldBe "def"
    }

    "should capture all remaining characters when trying to capture more characters than are available" in {
      context.consume(5).capture(7) shouldBe "ef" 
    }

    "should provide the input reversed" in {
      context.consume(3).reverse.capture(3) shouldBe "cba"
    }

    "indicate the position is on the first line" in {
      context.position.line shouldBe 1
    }

    "indicate the position is on the second line" in {
      context.consume(4).position.line shouldBe 2
    }

    "provide the content of the first line" in {
      context.position.lineContent shouldBe "abc"
    }

    "provide the content of the second line" in {
      context.consume(4).position.lineContent shouldBe "def"
    }

    "provide the content of the second line with a positional caret" in {
      context.consume(5).position.lineContentWithCaret shouldBe "def\n ^"
    }

    "indicate the correct column" in {
      context.consume(5).position.column shouldBe 2
    }

  }


}
