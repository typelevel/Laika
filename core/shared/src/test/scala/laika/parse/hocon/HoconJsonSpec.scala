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

import laika.api.config.ConfigValue.DoubleValue
import laika.internal.parse.hocon.*
import laika.internal.parse.hocon.HoconParsers.*
import laika.parse.{ LineSource, SourceCursor }
import munit.FunSuite

/** @author Jens Halm
  */
class HoconJsonSpec extends FunSuite with ResultBuilders {

  private def f(key: String, value: String, input: String): BuilderField =
    BuilderField(key, stringValue(value, input))

  private def result(fields: BuilderField*): Either[String, ObjectBuilderValue] = Right(
    ObjectBuilderValue(fields)
  )

  test("empty object in") {
    assertEquals(objectValue.parse("{ }").toEither, result())
  }

  test("object with one property in") {
    val input = """{ "a": "foo" }"""
    assertEquals(
      objectValue.parse(input).toEither,
      result(BuilderField("a", stringValue("foo", input)))
    )
  }

  test("object with two properties in") {
    val input = """{ "a": "foo", "b": "bar" }"""
    assertEquals(
      objectValue.parse(input).toEither,
      result(f("a", "foo", input), f("b", "bar", input))
    )
  }

  test("object with all property types") {
    val input =
      """{
        |  "str": "foo",
        |  "int": 27,
        |  "none": null,
        |  "bool": true,
        |  "arr": [ 12, 13, "bar" ],
        |  "obj": { "inner": "xx", "num": 9.5 }
        |}""".stripMargin
    assertEquals(
      objectValue.parse(input).toEither,
      result(
        BuilderField("str", stringValue("foo", input)),
        BuilderField("int", longValue(27, input)),
        BuilderField("none", nullValue(input)),
        BuilderField("bool", trueValue(input)),
        BuilderField(
          "arr",
          ArrayBuilderValue(
            Seq(
              longValue(12, input),
              longValue(13, input),
              stringValue("bar", input)
            )
          )
        ),
        BuilderField(
          "obj",
          ObjectBuilderValue(
            Seq(
              BuilderField("inner", stringValue("xx", input)),
              BuilderField("num", doubleValue(9.5, input))
            )
          )
        )
      )
    )
  }

  private def validQuoted(value: String): ValidString =
    ValidString(value, LineSource(value, SourceCursor("\"" + value + "\"").consume(1)))

  test("empty string") {
    assertEquals(quotedString.parse("\"\"").toEither, Right(validQuoted("")))
  }

  test("string containing only whitespace") {
    assertEquals(quotedString.parse("\"  \"").toEither, Right(validQuoted("  ")))
  }

  test("plain string") {
    assertEquals(quotedString.parse("\"fooz\"").toEither, Right(validQuoted("fooz")))
  }

  test("new line character") {
    val input    = "\"foo\\nbar\""
    val expected =
      ValidString("foo\nbar", LineSource("foo\\nbar", SourceCursor(input).consume(1)))
    assertEquals(quotedString.parse(input).toEither, Right(expected))
  }

  test("unicode character reference") {
    val input = "\"foo \\u007B bar\""
    assertEquals(
      quotedString.parse(input).toEither,
      Right(
        ValidString(
          "foo { bar",
          LineSource(input.drop(1).dropRight(1), SourceCursor(input).consume(1))
        )
      )
    )
  }

  test("long") {
    assertEquals(numberValue.parse("123").toEither, Right(longValue(123, "123")))
  }

  test("signed long") {
    assertEquals(numberValue.parse("-123").toEither, Right(longValue(-123, "-123")))
  }

  test("double") {
    assertEquals(numberValue.parse("123.5").toEither, Right(doubleValue(123.5, "123.5")))
  }

  private def doubleValueWithExp(value: Double, input: String): ConfigBuilderValue =
    ResolvedBuilderValue(DoubleValue(value), cursor(input, input))

  test("double with an exponent") {
    assertEquals(
      numberValue.parse("123.5E10").toEither,
      Right(doubleValueWithExp(1.235e12, "123.5E10"))
    )
  }

  test("double with a negative exponent") {
    assertEquals(
      numberValue.parse("123.5E-2").toEither,
      Right(doubleValueWithExp(1.235, "123.5E-2"))
    )
  }

}
