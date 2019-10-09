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

import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import laika.parse.hocon.HoconParsers._
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class HoconParserSpec extends WordSpec with Matchers with ParseResultHelpers with StringParserHelpers {

  def f (key: String, value: String): Field = Field(key, StringValue(value))
  
  "The root parser for documents that are not enclosed in braces " should {

    "parse an empty object in" in {
      Parsing (" ") using rootObject should produce (ObjectValue(Nil))
    }

    "parse an object with two properties in" in {
      Parsing (""" "a": "foo", "b": "bar" """.stripMargin) using rootObject should produce (ObjectValue(Seq(f("a","foo"),f("b","bar"))))
    }

    "parse an object with all property types" in {
      val input =
        """"str": "foo",
          |"int": 27,
          |"null": null,
          |"bool": true,
          |"arr": [ 1, 2, "bar" ],
          |"obj": { "inner": "xx", "num": 9.5 }""".stripMargin
      Parsing (input) using rootObject should produce (ObjectValue(Seq(
        Field("str", StringValue("foo")),
        Field("int", LongValue(27)),
        Field("null", NullValue),
        Field("bool", BooleanValue(true)),
        Field("arr", ArrayValue(Seq(
          LongValue(1), LongValue(2), StringValue("bar")
        ))),
        Field("obj", ObjectValue(Seq(
          Field("inner", StringValue("xx")),
          Field("num", DoubleValue(9.5))
        )))
      )))
    }

  }

}
