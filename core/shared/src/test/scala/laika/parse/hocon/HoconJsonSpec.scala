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

import laika.parse.helper.MigrationSpec
import laika.parse.hocon.HoconParsers._

/**
  * @author Jens Halm
  */
class HoconJsonSpec extends MigrationSpec with ResultBuilders {

  def f (key: String, value: String): BuilderField = BuilderField(key, stringValue(value))

  def result (fields: BuilderField*): Either[String, ObjectBuilderValue] = Right(ObjectBuilderValue(fields))
  
  "The object parser" should {

    "parse an empty object in" in {
      assertEquals(objectValue.parse("{ }").toEither, result())
    }

    "parse an object with one property in" in {
      assertEquals(objectValue.parse("""{ "a": "foo" }""").toEither, result(BuilderField("a", stringValue("foo"))))
    }

    "parse an object with two properties in" in {
      assertEquals(objectValue.parse("""{ "a": "foo", "b": "bar" }""").toEither, result(f("a","foo"),f("b","bar")))
    }

    "parse an object with all property types" in {
      val input =
        """{
          |  "str": "foo",
          |  "int": 27,
          |  "null": null,
          |  "bool": true,
          |  "arr": [ 1, 2, "bar" ],
          |  "obj": { "inner": "xx", "num": 9.5 }
          |}""".stripMargin
      assertEquals(objectValue.parse(input).toEither, result(
        BuilderField("str", stringValue("foo")),
        BuilderField("int", longValue(27)),
        BuilderField("null", nullValue),
        BuilderField("bool", trueValue),
        BuilderField("arr", ArrayBuilderValue(Seq(
          longValue(1), longValue(2), stringValue("bar")
        ))),
        BuilderField("obj", ObjectBuilderValue(Seq(
          BuilderField("inner", stringValue("xx")),
          BuilderField("num", doubleValue(9.5))
        )))
      ))
    }

  }

  "The string parser" should {

    "parse an empty string" in {
      assertEquals(quotedString.parse("\"\"").toEither, Right(ValidStringValue("")))
    }

    "parse an string containing only whitespace" in {
      assertEquals(quotedString.parse("\"  \"").toEither, Right(ValidStringValue("  ")))
    }
    
    "parse a plain string" in {
      assertEquals(quotedString.parse("\"fooz\"").toEither, Right(ValidStringValue("fooz")))
    }

    "parse a new line character" in {
      assertEquals(quotedString.parse("\"foo\\nbar\"").toEither, Right(ValidStringValue("foo\nbar")))
    }

    "parse a unicode character reference" in {
      assertEquals(quotedString.parse("\"foo \\u007B bar\"").toEither, Right(ValidStringValue("foo { bar")))
    }
    
  }
  
  "The number parser" should {
    
    "parse a long" in {
      assertEquals(numberValue.parse("123").toEither, Right(longValue(123)))
    }

    "parse a signed long" in {
      assertEquals(numberValue.parse("-123").toEither, Right(longValue(-123)))
    }

    "parse a double" in {
      assertEquals(numberValue.parse("123.5").toEither, Right(doubleValue(123.5)))
    }

    "parse a double with an exponent" in {
      assertEquals(numberValue.parse("123.5E10").toEither, Right(doubleValue(1.235E12)))
    }

    "parse a double with a negative exponent" in {
      assertEquals(numberValue.parse("123.5E-2").toEither, Right(doubleValue(1.235)))
    }
  }
  
}
