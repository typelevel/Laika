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

import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import laika.parse.hocon.HoconParsers._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class HoconJsonSpec extends AnyWordSpec with Matchers with ParseResultHelpers with StringParserHelpers with ResultBuilders {

  def f (key: String, value: String): BuilderField = BuilderField(key, stringValue(value))
  
  "The object parser" should {

    "parse an empty object in" in {
      Parsing ("{ }") using objectValue should produce (ObjectBuilderValue(Nil): ConfigBuilderValue)
    }

    "parse an object with one property in" in {
      Parsing ("""{ "a": "foo" }""") using objectValue should produce (ObjectBuilderValue(Seq(BuilderField("a", stringValue("foo")))): ConfigBuilderValue)
    }

    "parse an object with two properties in" in {
      Parsing ("""{ "a": "foo", "b": "bar" }""") using objectValue should produce (ObjectBuilderValue(Seq(f("a","foo"),f("b","bar"))): ConfigBuilderValue)
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
      Parsing (input) using objectValue should produce (ObjectBuilderValue(Seq(
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
      )): ConfigBuilderValue)
    }

  }

  "The string parser" should {

    "parse an empty string" in {
      Parsing ("\"\"") using quotedString should produce (ValidStringValue(""): StringBuilderValue)
    }

    "parse an string containing only whitespace" in {
      Parsing ("\"  \"") using quotedString should produce (ValidStringValue("  "): StringBuilderValue)
    }
    
    "parse a plain string" in {
      Parsing ("\"fooz\"") using quotedString should produce (ValidStringValue("fooz"): StringBuilderValue)
    }

    "parse a new line character" in {
      Parsing ("\"foo\\nbar\"") using quotedString should produce (ValidStringValue("foo\nbar"): StringBuilderValue)
    }

    "parse a unicode character reference" in {
      Parsing ("\"foo \\u007B bar\"") using quotedString should produce (ValidStringValue("foo { bar"): StringBuilderValue)
    }
    
  }
  
  "The number parser" should {
    
    "parse a long" in {
      Parsing ("123") using numberValue should produce (longValue(123): ConfigBuilderValue)
    }

    "parse a signed long" in {
      Parsing ("-123") using numberValue should produce (longValue(-123): ConfigBuilderValue)
    }

    "parse a double" in {
      Parsing ("123.5") using numberValue should produce (doubleValue(123.5): ConfigBuilderValue)
    }

    "parse a double with an exponent" in {
      Parsing ("123.5E10") using numberValue should produce (doubleValue(1.235E12): ConfigBuilderValue)
    }

    "parse a double with a negative exponent" in {
      Parsing ("123.5E-2") using numberValue should produce (doubleValue(1.235): ConfigBuilderValue)
    }
  }
  
}
