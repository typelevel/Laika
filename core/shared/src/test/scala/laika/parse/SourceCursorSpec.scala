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

package laika.parse

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class SourceCursorSpec extends AnyWordSpec with Matchers {


  "The RootCursor" should {
    
    val cursor = SourceCursor("abc\ndef")

    "not be at the end of input after creation" in {
      cursor.atEnd shouldBe false
    }

    "be at the end of input after consuming all characters" in {
      cursor.consume(7).atEnd shouldBe true
    }

    "should indicate the full input length as remaining after creation" in {
      cursor.remaining shouldBe 7
    }

    "should indicate zero remaining after consuming all characters" in {
      cursor.consume(7).remaining shouldBe 0
    }

    "should return the first character after creation" in {
      cursor.char shouldBe 'a'
    }

    "should return the last character after consuming all but one characters" in {
      cursor.consume(6).char shouldBe 'f'
    }

    "should return a character relative to the current offset" in {
      cursor.consume(1).charAt(1) shouldBe 'c'
    }

    "should capture the input from the beginning" in {
      cursor.capture(3) shouldBe "abc"
    }

    "should capture the input from the current position" in {
      cursor.consume(4).capture(3) shouldBe "def"
    }

    "should capture all remaining characters when trying to capture more characters than are available" in {
      cursor.consume(5).capture(7) shouldBe "ef" 
    }

    "should provide the input reversed" in {
      cursor.consume(3).reverse.capture(3) shouldBe "cba"
    }

    "indicate the position is on the first line" in {
      cursor.position.line shouldBe 1
    }

    "indicate the position is on the second line" in {
      cursor.consume(4).position.line shouldBe 2
    }

    "provide the content of the first line" in {
      cursor.position.lineContent shouldBe "abc"
    }

    "provide the content of the second line" in {
      cursor.consume(4).position.lineContent shouldBe "def"
    }

    "provide the content of the second line with a positional caret" in {
      cursor.consume(5).position.lineContentWithCaret shouldBe "def\n ^"
    }

    "indicate the correct column" in {
      cursor.consume(5).position.column shouldBe 2
    }

  }

  "The BlockCursor" should {
    
    import cats.data.NonEmptyChain

    val root = new RootSource(new InputString("000\nabc\ndef"), 4, 0)
    val lines = NonEmptyChain(
      LineSource("abc", root),
      LineSource("def", root)
    )
    val cursor = BlockSource(lines)

    "not be at the end of input after creation" in {
      cursor.atEnd shouldBe false
    }

    "be at the end of input after consuming all characters" in {
      cursor.consume(7).atEnd shouldBe true
    }

    "should indicate the full input length as remaining after creation" in {
      cursor.remaining shouldBe 7
    }

    "should indicate zero remaining after consuming all characters" in {
      cursor.consume(7).remaining shouldBe 0
    }

    "should return the first character after creation" in {
      cursor.char shouldBe 'a'
    }

    "should return the last character after consuming all but one characters" in {
      cursor.consume(6).char shouldBe 'f'
    }

    "should return a character relative to the current offset" in {
      cursor.consume(1).charAt(1) shouldBe 'c'
    }

    "should capture the input from the beginning" in {
      cursor.capture(3) shouldBe "abc"
    }

    "should capture the input from the current position" in {
      cursor.consume(4).capture(3) shouldBe "def"
    }

    "should capture all remaining characters when trying to capture more characters than are available" in {
      cursor.consume(5).capture(7) shouldBe "ef"
    }

    "should provide the input reversed" in {
      cursor.consume(3).reverse.capture(3) shouldBe "cba"
    }

    "indicate the position is on the first line (second in root source)" in {
      cursor.position.line shouldBe 2
    }

    "indicate the position is on the second line (third in root source)" in {
      cursor.consume(4).position.line shouldBe 3
    }

    "provide the content of the first line" in {
      cursor.position.lineContent shouldBe "abc"
    }

    "provide the content of the second line" in {
      cursor.consume(4).position.lineContent shouldBe "def"
    }

    "provide the content of the second line with a positional caret" in {
      cursor.consume(5).position.lineContentWithCaret shouldBe "def\n ^"
    }

    "indicate the correct column" in {
      cursor.consume(5).position.column shouldBe 2
    }

  }



}
