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

import laika.ast.Path.Root
import munit.FunSuite

class RootCursorSpec extends FunSuite {

  private val cursor = SourceCursor("abc\ndef")

  test("not at the end of input after creation") {
    assertEquals(cursor.atEnd, false)
  }

  test("at the end of input after consuming all characters") {
    assertEquals(cursor.consume(7).atEnd, true)
  }

  test("indicate the full input length as remaining after creation") {
    assertEquals(cursor.remaining, 7)
  }

  test("indicate zero remaining after consuming all characters") {
    assertEquals(cursor.consume(7).remaining, 0)
  }

  test("return the first character after creation") {
    assertEquals(cursor.char, 'a')
  }

  test("return the last character after consuming all but one characters") {
    assertEquals(cursor.consume(6).char, 'f')
  }

  test("return a character relative to the current offset") {
    assertEquals(cursor.consume(1).charAt(1), 'c')
  }

  test("capture the input from the beginning") {
    assertEquals(cursor.capture(3), "abc")
  }

  test("capture the input from the current position") {
    assertEquals(cursor.consume(4).capture(3), "def")
  }

  test("capture all remaining characters when trying to capture more characters than are available") {
    assertEquals(cursor.consume(5).capture(7), "ef")
  }

  test("provide the input reversed") {
    assertEquals(cursor.consume(3).reverse.capture(3), "cba")
  }

  test("indicate the position is on the first line") {
    assertEquals(cursor.position.line, 1)
  }

  test("indicate the position is on the second line") {
    assertEquals(cursor.consume(4).position.line, 2)
  }

  test("provide the content of the first line") {
    assertEquals(cursor.position.lineContent, "abc")
  }

  test("provide the content of the second line") {
    assertEquals(cursor.consume(4).position.lineContent, "def")
  }

  test("provide the content of the second line with a positional caret") {
    assertEquals(cursor.consume(5).position.lineContentWithCaret, "def\n ^")
  }

  test("indicate the correct column") {
    assertEquals(cursor.consume(5).position.column, 2)
  }

  test("keep the path information") {
    assertEquals(SourceCursor("foo", Root / "bar").path, Some(Root / "bar"))
  }

  test("convert Windows line feeds") {
    assertEquals(SourceCursor("abc\r\ndef\r\nghi").input, "abc\ndef\nghi")
  }
  
}

class BlockCursorSpec extends FunSuite {

  import cats.data.NonEmptyChain

  private val cursor = {
    val root = new RootSource(InputString("000\nabc\ndef", Some(Root / "doc")), 4, 0)
    val lines = NonEmptyChain(
      LineSource("abc", root),
      LineSource("def", root.consume(4))
    )
    BlockSource(lines)
  }

  test("not be at the end of input after creation") {
    assertEquals(cursor.atEnd, false)
  }

  test("be at the end of input after consuming all characters") {
    assertEquals(cursor.consume(7).atEnd, true)
  }

  test("indicate the full input length as remaining after creation") {
    assertEquals(cursor.remaining, 7)
  }

  test("indicate zero remaining after consuming all characters") {
    assertEquals(cursor.consume(7).remaining, 0)
  }

  test("return the first character after creation") {
    assertEquals(cursor.char, 'a')
  }

  test("return the last character after consuming all but one characters") {
    assertEquals(cursor.consume(6).char, 'f')
  }

  test("return a character relative to the current offset") {
    assertEquals(cursor.consume(1).charAt(1), 'c')
  }

  test("capture the input from the beginning") {
    assertEquals(cursor.capture(3), "abc")
  }

  test("capture the input from the current position") {
    assertEquals(cursor.consume(4).capture(3), "def")
  }

  test("capture all remaining characters when trying to capture more characters than are available") {
    assertEquals(cursor.consume(5).capture(7), "ef")
  }

  test("provide the input reversed") {
    assertEquals(cursor.consume(3).reverse.capture(3), "cba")
  }

  test("indicate the position is on the first line (second in root source)") {
    assertEquals(cursor.position.line, 2)
  }

  test("indicate the position is on the second line (third in root source)") {
    assertEquals(cursor.consume(4).position.line, 3)
  }

  test("provide the content of the first line") {
    assertEquals(cursor.position.lineContent, "abc")
  }

  test("provide the content of the second line") {
    assertEquals(cursor.consume(4).position.lineContent, "def")
  }

  test("provide the content of the second line with a positional caret") {
    assertEquals(cursor.consume(5).position.lineContentWithCaret, "def\n ^")
  }

  test("indicate the correct column") {
    assertEquals(cursor.consume(5).position.column, 2)
  }

  test("keep the path information") {
    assertEquals(cursor.path, Some(Root / "doc"))
  }

}
