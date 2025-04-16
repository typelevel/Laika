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

import laika.api.config.{ ConfigParser, Key }
import laika.internal.parse.hocon.*
import laika.internal.parse.hocon.HoconParsers.*
import munit.FunSuite

/** @author Jens Halm
  */
class HoconParserSpec extends FunSuite with ResultBuilders {

  private def f(key: String, value: String, input: String): BuilderField =
    BuilderField(key, stringValue(value, input))

  private def nestedObject(stringVal: String, doubleVal: Double, input: String): BuilderField =
    BuilderField(
      "obj",
      ObjectBuilderValue(
        Seq(
          BuilderField("inner", stringValue(stringVal, input)),
          BuilderField("num", doubleValue(doubleVal, input))
        )
      )
    )

  private def nestedObject(input: String): BuilderField = nestedObject("xx", 9.5, input)

  private def arrayProperty(num1: Long, num2: Long, str: String, input: String): BuilderField =
    BuilderField(
      "arr",
      ArrayBuilderValue(
        Seq(
          longValue(num1, input),
          longValue(num2, input),
          stringValue(str, input)
        )
      )
    )

  private def arrayProperty(input: String): BuilderField = arrayProperty(17, 18, "bar", input)

  def parse(input: String): Either[String, ObjectBuilderValue] = rootObject.parse(input).toEither

  private def run(input: String, expectedFields: BuilderField*)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(parse(input), Right(ObjectBuilderValue(expectedFields)))

  test("empty root object that is not enclosed in braces") {
    run(" ")
  }

  test("root object with two properties that is not enclosed in braces") {
    val input = """ "a": "foo", "b": "bar" """
    run(
      input,
      f("a", "foo", input),
      f("b", "bar", input)
    )
  }

  test("root object with all property types that is not enclosed in braces") {
    val input =
      """obj { 
        |  inner = xx 
        |  num = 9.5 
        |}
        |str1 = "fop"
        |str2 = foo
        |
        |int = 27
        |"none": null
        |bool = true
        |arr = [ 17, 18, "bar" ]
        |"obj": { "inner": "yy", "num": 8.5 }""".stripMargin
    run(
      input,
      nestedObject(input),
      BuilderField("str1", stringValue("fop", input)),
      BuilderField("str2", stringValue("foo", input)),
      BuilderField("int", longValue(27, input)),
      BuilderField("none", nullValue(input)),
      BuilderField("bool", trueValue(input)),
      arrayProperty(input),
      nestedObject("yy", 8.5, input)
    )
  }

  test("root object with two properties that use '=' instead of ':'") {
    val input = """ "a" = "foo", "b" = "bar" """
    run(
      input,
      f("a", "foo", input),
      f("b", "bar", input)
    )
  }

  test("object property without separator") {
    val input =
      """"a": "foo", 
        |"obj" { 
        |  "inner": "xx", 
        |  "num": 9.5 
        |} """.stripMargin
    run(input, f("a", "foo", input), nestedObject(input))
  }

  test("object property with a trailing comma") {
    val input =
      """"a": "foo", 
        |"obj" = { 
        |  "inner": "xx", 
        |  "num": 9.5,
        |} """.stripMargin
    run(input, f("a", "foo", input), nestedObject(input))
  }

  test("array property with a trailing comma") {
    val input =
      """"a": "foo", 
        |"arr": [ 17, 18, "bar", ]""".stripMargin
    run(input, f("a", "foo", input), arrayProperty(input))
  }

  test("array property with elements separated by newline characters") {
    val input =
      """"a": "foo", 
        |"arr": [ 
        |  17 
        |  18 
        |  "bar"
        |]""".stripMargin
    run(input, f("a", "foo", input), arrayProperty(input))
  }

  test("root object with members separated by newline characters") {
    val input = """"a": "foo"
                  |"b": "bar" 
                  |"c": "baz" """.stripMargin
    run(
      input,
      f("a", "foo", input),
      f("b", "bar", input),
      f("c", "baz", input)
    )
  }

  test("root object with members separated by two newline characters") {
    val input = """"a": "foo"
                  |
                  |"b": "bar"
                  | 
                  |"c": "baz" """.stripMargin
    run(
      input,
      f("a", "foo", input),
      f("b", "bar", input),
      f("c", "baz", input)
    )
  }

  test("multiline string property") {
    val input =
      """"a": "foo", 
        |"s": +++Line 1
        | Line 2
        | Line 3+++""".stripMargin.replace("+++", "\"\"\"")
    run(input, f("a", "foo", input), f("s", "Line 1\n Line 2\n Line 3", input))
  }

  test("multiline string property with more than 3 closing quotes") {
    val input =
      """"a": "foo", 
        |"s": +++Line 1
        | Line 2
        | Line 3+++++""".stripMargin.replace("+", "\"")
    run(input, f("a", "foo", input), f("s", "Line 1\n Line 2\n Line 3\"\"", input))
  }

  test("multiline string property - ignore escapes") {
    val input =
      """"a": "foo", 
        |"s": +++Word 1 \n Word 2+++""".stripMargin.replace("+++", "\"\"\"")
    run(input, f("a", "foo", input), f("s", "Word 1 \\n Word 2", input))
  }

  test("object with unquoted keys") {
    val input =
      """a: "foo", 
        |arr: [ 17, 18, "bar" ]""".stripMargin
    run(input, f("a", "foo", input), arrayProperty(input))
  }

  test("object with unquoted string values") {
    val input =
      """"a": foo, 
        |"arr": [ 17, 18, bar ]""".stripMargin
    run(input, f("a", "foo", input), arrayProperty(input))
  }

  test("object with the += field separator") {
    val input =
      """a = [ foo ], 
        |a += bar""".stripMargin
    run(
      input,
      BuilderField("a", ArrayBuilderValue(Seq(stringValue("foo", input)))),
      BuilderField(
        "a",
        ConcatValue(
          SelfReference,
          Seq(ConcatPart("", ArrayBuilderValue(Seq(stringValue("bar", input)))))
        )
      )
    )
  }

  test("simple values containing booleans") {
    val input = "a = true is false"
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          trueValue(input),
          Seq(ConcatPart(" ", stringValue("is", input)), ConcatPart(" ", falseValue(input)))
        )
      )
    )
  }

  test("simple values containing numbers") {
    val input = "a = 9 is 7"
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          longValue(9, input),
          Seq(ConcatPart(" ", stringValue("is", input)), ConcatPart(" ", longValue(7, input)))
        )
      )
    )
  }

  test("object values on a single line") {
    val input = """a = { "inner": "xx", "num": 9.5 } { "inner": "yy", "num": 8.5 }"""
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          nestedObject(input).value,
          Seq(ConcatPart(" ", nestedObject("yy", 8.5, input).value))
        )
      )
    )
  }

  test("object values spanning multiple lines") {
    val input = """a = { 
                  |  "inner": "xx", 
                  |  "num": 9.5
                  |} { 
                  |  "inner": "yy", 
                  |  "num": 8.5
                  |}""".stripMargin
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          nestedObject(input).value,
          Seq(ConcatPart(" ", nestedObject("yy", 8.5, input).value))
        )
      )
    )
  }

  test("array values on a single line") {
    val input = """a = [ 17, 18, "bar", ] [ 27, 28, "baz", ]"""
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          arrayProperty(input).value,
          Seq(ConcatPart(" ", arrayProperty(27, 28, "baz", input).value))
        )
      )
    )
  }

  test("array values spanning multiple lines") {
    val input = """a = [ 
                  | 17
                  | 18 
                  | "bar"
                  |] [ 
                  | 27 
                  | 28
                  | "baz"
                  |]""".stripMargin
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          arrayProperty(input).value,
          Seq(ConcatPart(" ", arrayProperty(27, 28, "baz", input).value))
        )
      )
    )
  }

  test("concatenated key consisting of unquoted strings") {
    val input = """a b c = foo"""
    run(input, f("a b c", "foo", input))
  }

  test("concatenated key consisting of unquoted and quoted strings") {
    val input = """a "b" c = foo"""
    run(input, f("a b c", "foo", input))
  }

  test("substitution as a simple value") {
    val input = "a = ${foo.bar}"
    run(input, BuilderField("a", SubstitutionValue(Key("foo", "bar"), optional = false)))
  }

  test("substitution for an optional value") {
    val input = "a = ${?foo.bar}"
    run(input, BuilderField("a", SubstitutionValue(Key("foo", "bar"), optional = true)))
  }

  test("substitution as the first part in a concatenated value") {
    val input = "a = ${foo.bar} is null"
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          SubstitutionValue(Key("foo", "bar"), optional = false),
          Seq(ConcatPart(" ", stringValue("is", input)), ConcatPart(" ", nullValue(input)))
        )
      )
    )
  }

  test("substitution as the last part in a concatenated value") {
    val input = "a = Blue is ${foo.bar}"
    run(
      input,
      BuilderField(
        "a",
        ConcatValue(
          stringValue("Blue", input),
          Seq(
            ConcatPart(" ", stringValue("is", input)),
            ConcatPart(" ", SubstitutionValue(Key("foo", "bar"), optional = false))
          )
        )
      )
    )
  }

  test("comment at the beginning of the input") {
    val input = """
                  | // comment
                  | 
                  | a = 7
    """.stripMargin
    run(input, BuilderField("a", longValue(7, input)))
  }

  test("comment at the end of the input") {
    val input = """
                  | a = 7
                  |
                  | // comment
                """.stripMargin
    run(input, BuilderField("a", longValue(7, input)))
  }

  test("comment in the middle of the input") {
    val input = """
                  | a = 7
                  |
                  | // comment
                  | 
                  | b = 9
                """.stripMargin
    run(input, BuilderField("a", longValue(7, input)), BuilderField("b", longValue(9, input)))
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
    run(input, BuilderField("a", longValue(7, input)), BuilderField("b", longValue(9, input)))
  }

  test("comment next to an object member") {
    val input = """
                  | a = 7   // comment
                  | 
                  | b = 9
                """.stripMargin
    run(input, BuilderField("a", longValue(7, input)), BuilderField("b", longValue(9, input)))
  }

  test("comment next to an array property") {
    val input =
      """"a": "foo", 
        |"arr": [ 
        |  17  // hello
        |  18  # there
        |  "bar"
        |]""".stripMargin
    run(input, f("a", "foo", input), arrayProperty(input))
  }

  test("unquoted path") {
    val input = "foo.bar = 7"
    run(input, BuilderField(Key("foo", "bar"), longValue(7, input)))
  }

  test("unquoted path with whitespace") {
    val input = "foo.bar bar.baz = 7"
    run(input, BuilderField(Key("foo", "bar bar", "baz"), longValue(7, input)))
  }

  test("quoted path") {
    val input = "\"foo.bar\" = 7"
    run(input, BuilderField(Key("foo.bar"), longValue(7, input)))
  }

  test("quoted and unquoted path combined") {
    val input = "foo.\"bar.bar\".baz = 7"
    run(input, BuilderField(Key("foo", "bar.bar", "baz"), longValue(7, input)))
  }

  test("quoted empty string as a path element") {
    val input = " foo.\"\".baz = 7"
    run(input, BuilderField(Key("foo", "", "baz"), longValue(7, input)))
  }

  test("optional file include") {
    val file  = "foo.conf"
    val input = s"""include file("$file")"""
    assertEquals(
      ConfigParser.parse(input).includes,
      Seq(IncludeFile(ValidString("foo.conf", cursor(file, input))))
    )
  }

  test("optional classpath include") {
    val cp    = "foo.conf"
    val input = s"""include classpath("$cp")"""
    assertEquals(
      ConfigParser.parse(input).includes,
      Seq(IncludeClassPath(ValidString("foo.conf", cursor(cp, input))))
    )
  }

  test("optional URL include") {
    val url   = "http://config.com/foo.conf"
    val input = s"""include url("$url")"""
    assertEquals(
      ConfigParser.parse(input).includes,
      Seq(IncludeUrl(ValidString("http://config.com/foo.conf", cursor(url, input))))
    )
  }

  test("optional heuristic include") {
    val url   = "http://config.com/foo.conf"
    val input = s"""include "$url""""
    assertEquals(
      ConfigParser.parse(input).includes,
      Seq(IncludeAny(ValidString("http://config.com/foo.conf", cursor(url, input))))
    )
  }

  test("required heuristic include") {
    val url   = "http://config.com/foo.conf"
    val input = s"""include required("$url")"""
    assertEquals(
      ConfigParser.parse(input).includes,
      Seq(
        IncludeAny(ValidString("http://config.com/foo.conf", cursor(url, input)), isRequired = true)
      )
    )
  }

  test("required file include") {
    val file  = "foo.conf"
    val input = s"""include required(file("$file"))"""
    assertEquals(
      ConfigParser.parse(input).includes,
      Seq(IncludeFile(ValidString("foo.conf", cursor(file, input)), isRequired = true))
    )
  }

  test("multiple include statements on different nesting levels") {
    val url   = "http://config.com/foo.conf"
    val input =
      s"""
         |include file("foo.conf")
         |a {
         |  include classpath("bar.conf")
         |  
         |  x = 5
         |  
         |  b {
         |    include url("$url")
         |  }
         |}
         |""".stripMargin
    assertEquals(
      ConfigParser.parse(input).includes,
      Seq(
        IncludeFile(ValidString("foo.conf", cursor("foo.conf", input))),
        IncludeClassPath(ValidString("bar.conf", cursor("bar.conf", input))),
        IncludeUrl(ValidString(url, cursor(url, input)))
      )
    )
  }

}
