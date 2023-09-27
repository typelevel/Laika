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

package laika.parse.hocon

import laika.internal.parse.hocon.*
import laika.internal.parse.hocon.HoconParsers.*
import munit.FunSuite

/** @author Jens Halm
  */
class HoconJsonSpec extends FunSuite with ResultBuilders {

  private def f(key: String, value: String): BuilderField = BuilderField(key, stringValue(value))

  private def result(fields: BuilderField*): Either[String, ObjectBuilderValue] = Right(
    ObjectBuilderValue(fields)
  )

  test("empty object in") {
    assertEquals(objectValue.parse("{ }").toEither, result())
  }

  test("object with one property in") {
    assertEquals(
      objectValue.parse("""{ "a": "foo" }""").toEither,
      result(BuilderField("a", stringValue("foo")))
    )
  }

  test("object with two properties in") {
    assertEquals(
      objectValue.parse("""{ "a": "foo", "b": "bar" }""").toEither,
      result(f("a", "foo"), f("b", "bar"))
    )
  }

  test("object with all property types") {
    val input =
      """{
        |  "str": "foo",
        |  "int": 27,
        |  "null": null,
        |  "bool": true,
        |  "arr": [ 1, 2, "bar" ],
        |  "obj": { "inner": "xx", "num": 9.5 }
        |}""".stripMargin
    assertEquals(
      objectValue.parse(input).toEither,
      result(
        BuilderField("str", stringValue("foo")),
        BuilderField("int", longValue(27)),
        BuilderField("null", nullValue),
        BuilderField("bool", trueValue),
        BuilderField(
          "arr",
          ArrayBuilderValue(
            Seq(
              longValue(1),
              longValue(2),
              stringValue("bar")
            )
          )
        ),
        BuilderField(
          "obj",
          ObjectBuilderValue(
            Seq(
              BuilderField("inner", stringValue("xx")),
              BuilderField("num", doubleValue(9.5))
            )
          )
        )
      )
    )
  }

  test("empty string") {
    assertEquals(quotedString.parse("\"\"").toEither, Right(ValidStringValue("")))
  }

  test("string containing only whitespace") {
    assertEquals(quotedString.parse("\"  \"").toEither, Right(ValidStringValue("  ")))
  }

  test("plain string") {
    assertEquals(quotedString.parse("\"fooz\"").toEither, Right(ValidStringValue("fooz")))
  }

  test("new line character") {
    assertEquals(quotedString.parse("\"foo\\nbar\"").toEither, Right(ValidStringValue("foo\nbar")))
  }

  test("unicode character reference") {
    assertEquals(
      quotedString.parse("\"foo \\u007B bar\"").toEither,
      Right(ValidStringValue("foo { bar"))
    )
  }

  test("long") {
    assertEquals(numberValue.parse("123").toEither, Right(longValue(123)))
  }

  test("signed long") {
    assertEquals(numberValue.parse("-123").toEither, Right(longValue(-123)))
  }

  test("double") {
    assertEquals(numberValue.parse("123.5").toEither, Right(doubleValue(123.5)))
  }

  test("double with an exponent") {
    assertEquals(numberValue.parse("123.5E10").toEither, Right(doubleValue(1.235e12)))
  }

  test("double with a negative exponent") {
    assertEquals(numberValue.parse("123.5E-2").toEither, Right(doubleValue(1.235)))
  }

}
