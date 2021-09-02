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

import laika.config.{ConfigParser, Key}
import laika.parse.hocon.HoconParsers._
import munit.FunSuite

/**
  * @author Jens Halm
  */
class HoconParserSpec extends FunSuite with ResultBuilders {

  def f (key: String, value: String): BuilderField = BuilderField(key, stringValue(value))
  
  private val nestedObject = BuilderField("obj", ObjectBuilderValue(Seq(
    BuilderField("inner", stringValue("xx")),
    BuilderField("num", doubleValue(9.5))
  )))
  
  private val arrayProperty = BuilderField("arr", ArrayBuilderValue(Seq(
    longValue(1), longValue(2), stringValue("bar")
  )))
  
  def parse (input: String): Either[String, ObjectBuilderValue] = rootObject.parse(input).toEither
  
  def result (fields: BuilderField*): Either[String, ObjectBuilderValue] = Right(ObjectBuilderValue(fields))
  
  def run (input: String, expectedFields: BuilderField*): Unit =
    assertEquals(parse(input), Right(ObjectBuilderValue(expectedFields)))

  test("empty root object that is not enclosed in braces") {
    run(" ")
  }

  test("root object with two properties that is not enclosed in braces") {
    run(
      """ "a": "foo", "b": "bar" """.stripMargin,
      f("a","foo"),
      f("b","bar")
    )
  }

  test("root object with all property types that is not enclosed in braces") {
    val input =
      """obj { 
        |  inner = xx 
        |  num = 9.5 
        |}
        |str1 = "foo"
        |str2 = foo
        |
        |int = 27
        |"null": null
        |bool = true
        |arr = [ 1, 2, "bar" ]
        |"obj": { "inner": "xx", "num": 9.5 }""".stripMargin
    run(input, 
      nestedObject,
      BuilderField("str1", stringValue("foo")),
      BuilderField("str2", stringValue("foo")),
      BuilderField("int", longValue(27)),
      BuilderField("null", nullValue),
      BuilderField("bool", trueValue),
      arrayProperty,
      nestedObject
    )
  }

  test("root object with two properties that use '=' instead of ':'") {
    run(
      """ "a" = "foo", "b" = "bar" """.stripMargin, 
      f("a","foo"),
      f("b","bar")
    )
  }

  test("object property without separator") {
    val input =
      """"a": "foo", 
        |"obj" { 
        |  "inner": "xx", 
        |  "num": 9.5 
        |} """.stripMargin
    run(input, f("a","foo"), nestedObject)
  }

  test("object property with a trailing comma") {
    val input =
      """"a": "foo", 
        |"obj" = { 
        |  "inner": "xx", 
        |  "num": 9.5,
        |} """.stripMargin
    run(input, f("a","foo"), nestedObject)
  }

  test("array property with a trailing comma") {
    val input =
      """"a": "foo", 
        |"arr": [ 1, 2, "bar", ]""".stripMargin
    run(input, f("a","foo"), arrayProperty)
  }

  test("array property with elements separated by newline characters") {
    val input =
      """"a": "foo", 
        |"arr": [ 
        |  1 
        |  2 
        |  "bar"
        |]""".stripMargin
    run(input, f("a","foo"), arrayProperty)
  }

  test("root object with members separated by newline characters") {
    run(
      """"a": "foo"
        |"b": "bar" 
        |"c": "baz" """.stripMargin, 
      f("a","foo"),
      f("b","bar"),
      f("c","baz")
    )
  }

  test("root object with members separated by two newline characters") {
    run(
      """"a": "foo"
        |
        |"b": "bar"
        | 
        |"c": "baz" """.stripMargin, 
      f("a","foo"),
      f("b","bar"),
      f("c","baz")
    )
  }

  test("multiline string property") {
    val input =
      """"a": "foo", 
        |"s": +++Line 1
        | Line 2
        | Line 3+++""".stripMargin.replace("+++", "\"\"\"")
    run(input, f("a","foo"), f("s", "Line 1\n Line 2\n Line 3"))
  }

  test("multiline string property with more than 3 closing quotes") {
    val input =
      """"a": "foo", 
        |"s": +++Line 1
        | Line 2
        | Line 3+++++""".stripMargin.replace("+", "\"")
    run(input, f("a","foo"), f("s", "Line 1\n Line 2\n Line 3\"\""))
  }

  test("multiline string property - ignore escapes") {
    val input =
      """"a": "foo", 
        |"s": +++Word 1 \n Word 2+++""".stripMargin.replace("+++", "\"\"\"")
    run(input, f("a","foo"), f("s", "Word 1 \\n Word 2"))
  }

  test("object with unquoted keys") {
    val input =
      """a: "foo", 
        |arr: [ 1, 2, "bar" ]""".stripMargin
    run(input, f("a","foo"), arrayProperty)
  }

  test("object with unquoted string values") {
    val input =
      """"a": foo, 
        |"arr": [ 1, 2, bar ]""".stripMargin
    run(input, f("a","foo"), arrayProperty)
  }

  test("object with the += field separator") {
    val input =
      """a = [ foo ], 
        |a += bar""".stripMargin
    run(input, 
      BuilderField("a", ArrayBuilderValue(Seq(stringValue("foo")))),
      BuilderField("a", ConcatValue(SelfReference, Seq(ConcatPart("", ArrayBuilderValue(Seq(stringValue("bar")))))))
    )
  }
  
  
  test("simple values containing booleans") {
    val input = "a = true is false"
    run(input, 
      BuilderField("a", ConcatValue(trueValue, Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", falseValue))))
    )
  }

  test("simple values containing numbers") {
    val input = "a = 9 is 7"
    run(input, 
      BuilderField("a", ConcatValue(longValue(9), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", longValue(7)))))
    )
  }

  test("object values on a single line") {
    val input = """a = { "inner": "xx", "num": 9.5 } { "inner": "xx", "num": 9.5 }"""
    run(input, 
      BuilderField("a", ConcatValue(nestedObject.value, Seq(ConcatPart(" ", nestedObject.value))))
    )
  }

  test("object values spanning multiple lines") {
    val input = """a = { 
                  |  "inner": "xx", 
                  |  "num": 9.5
                  |} { 
                  |  "inner": "xx", 
                  |  "num": 9.5
                  |}""".stripMargin
    run(input, 
      BuilderField("a", ConcatValue(nestedObject.value, Seq(ConcatPart(" ", nestedObject.value))))
    )
  }

  test("array values on a single line") {
    val input = """a = [ 1, 2, "bar", ] [ 1, 2, "bar", ]"""
    run(input, 
      BuilderField("a", ConcatValue(arrayProperty.value, Seq(ConcatPart(" ", arrayProperty.value))))
    )
  }

  test("array values spanning multiple lines") {
    val input = """a = [ 
                  | 1
                  | 2 
                  | "bar"
                  |] [ 
                  | 1 
                  | 2
                  | "bar"
                  |]""".stripMargin
    run(input, 
      BuilderField("a", ConcatValue(arrayProperty.value, Seq(ConcatPart(" ", arrayProperty.value))))
    )
  }

  
  test("concatenated key consisting of unquoted strings") {
    val input = """a b c = foo"""
    run(input, f("a b c","foo"))
  }

  test("concatenated key consisting of unquoted and quoted strings") {
    val input = """a "b" c = foo"""
    run(input, f("a b c","foo"))
  }
  
  
  test("substitution as a simple value") {
    val input = "a = ${foo.bar}"
    run(input, 
      BuilderField("a", SubstitutionValue(Key("foo", "bar"), optional = false))
    )
  }

  test("substitution for an optional value") {
    val input = "a = ${?foo.bar}"
    run(input, 
      BuilderField("a", SubstitutionValue(Key("foo", "bar"), optional = true))
    )
  }

  test("substitution as the first part in a concatenated value") {
    val input = "a = ${foo.bar} is null"
    run(input, 
      BuilderField("a", ConcatValue(SubstitutionValue(Key("foo", "bar"), optional = false), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", nullValue))))
    )
  }

  test("substitution as the last part in a concatenated value") {
    val input = "a = Blue is ${foo.bar}"
    run(input, 
      BuilderField("a", ConcatValue(stringValue("Blue"), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", SubstitutionValue(Key("foo", "bar"), optional = false)))))
    )
  }
  
  
  test("comment at the beginning of the input") {
    val input = """
      | // comment
      | 
      | a = 7
    """.stripMargin
    run(input, BuilderField("a", longValue(7)))
  }

  test("comment at the end of the input") {
    val input = """
                  | a = 7
                  |
                  | // comment
                """.stripMargin
    run(input, BuilderField("a", longValue(7)))
  }

  test("comment in the middle of the input") {
    val input = """
                  | a = 7
                  |
                  | // comment
                  | 
                  | b = 9
                """.stripMargin
    run(input, BuilderField("a", longValue(7)), BuilderField("b", longValue(9)))
  }

  test("multiple comments in the middle of the input") {
    val input = """
                  | a = 7
                  |
                  | // comment
                  | 
                  | # some other comment
                  |
                  | b = 9
                """.stripMargin
    run(input, BuilderField("a", longValue(7)), BuilderField("b", longValue(9)))
  }

  test("comment next to an object member") {
    val input = """
                  | a = 7   // comment
                  | 
                  | b = 9
                """.stripMargin
    run(input, BuilderField("a", longValue(7)), BuilderField("b", longValue(9)))
  }

  test("comment next to an array property") {
    val input =
      """"a": "foo", 
        |"arr": [ 
        |  1  // hello
        |  2  # there
        |  "bar"
        |]""".stripMargin
    run(input, f("a","foo"), arrayProperty)
  }
  
  
  test("unquoted path") {
    run("foo.bar = 7", BuilderField(Key("foo", "bar"), longValue(7)))
  }

  test("unquoted path with whitespace") {
    run("foo.bar bar.baz = 7", BuilderField(Key("foo", "bar bar", "baz"), longValue(7)))
  }

  test("quoted path") {
    run("\"foo.bar\" = 7", BuilderField(Key("foo.bar"), longValue(7)))
  }
  
  test("quoted and unquoted path combined") {
    run("foo.\"bar.bar\".baz = 7", BuilderField(Key("foo", "bar.bar", "baz"), longValue(7)))
  }

  test("quoted empty string as a path element") {
    run("foo.\"\".baz = 7", BuilderField(Key("foo", "", "baz"), longValue(7)))
  }
  
  
  test("optional file include") {
    val input = """include file("foo.conf")"""
    assertEquals(ConfigParser.parse(input).includes, Seq(IncludeFile(ValidStringValue("foo.conf"))))
  }

  test("optional classpath include") {
    val input = """include classpath("foo.conf")"""
    assertEquals(ConfigParser.parse(input).includes, Seq(IncludeClassPath(ValidStringValue("foo.conf"))))
  }

  test("optional URL include") {
    val input = """include url("http://config.com/foo.conf")"""
    assertEquals(ConfigParser.parse(input).includes, Seq(IncludeUrl(ValidStringValue("http://config.com/foo.conf"))))
  }

  test("optional heuristic include") {
    val input = """include "http://config.com/foo.conf""""
    assertEquals(ConfigParser.parse(input).includes, Seq(IncludeAny(ValidStringValue("http://config.com/foo.conf"))))
  }

  test("required heuristic include") {
    val input = """include required("http://config.com/foo.conf")"""
    assertEquals(ConfigParser.parse(input).includes, Seq(IncludeAny(ValidStringValue("http://config.com/foo.conf"), isRequired = true)))
  }

  test("required file include") {
    val input = """include required(file("foo.conf"))"""
    assertEquals(ConfigParser.parse(input).includes, Seq(IncludeFile(ValidStringValue("foo.conf"), isRequired = true)))
  }
  
  test("multiple include statements on different nesting levels") {
    val input =
      """
        |include file("foo.conf")
        |a {
        |  include classpath("foo.conf")
        |  
        |  x = 5
        |  
        |  b {
        |    include url("http://config.com/foo.conf")
        |  }
        |}
        |""".stripMargin
    assertEquals(ConfigParser.parse(input).includes, Seq(
      IncludeFile(ValidStringValue("foo.conf")),
      IncludeClassPath(ValidStringValue("foo.conf")),
      IncludeUrl(ValidStringValue("http://config.com/foo.conf"))
    ))
  }
  
  
}
