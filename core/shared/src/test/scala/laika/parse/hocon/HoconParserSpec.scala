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
import laika.parse.helper.MigrationSpec
import laika.parse.hocon.HoconParsers._

/**
  * @author Jens Halm
  */
class HoconParserSpec extends MigrationSpec with ResultBuilders {

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
  
  "The object parser" should {

    "parse an empty root object that is not enclosed in braces" in {
      assertEquals(parse(" "), result())
    }

    "parse a root object with two properties that is not enclosed in braces" in {
      assertEquals(
        parse(""" "a": "foo", "b": "bar" """.stripMargin),
        result(f("a","foo"),f("b","bar"))
      )
    }

    "parse a root object with all property types that is not enclosed in braces" in {
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
      assertEquals(parse(input), result(
        nestedObject,
        BuilderField("str1", stringValue("foo")),
        BuilderField("str2", stringValue("foo")),
        BuilderField("int", longValue(27)),
        BuilderField("null", nullValue),
        BuilderField("bool", trueValue),
        arrayProperty,
        nestedObject
      ))
    }

    "parse a root object with two properties that use '=' instead of ':'" in {
      assertEquals(
        parse(""" "a" = "foo", "b" = "bar" """.stripMargin), 
        result(f("a","foo"),f("b","bar"))
      )
    }

    "parse an object property without separator" in {
      val input =
        """"a": "foo", 
          |"obj" { 
          |  "inner": "xx", 
          |  "num": 9.5 
          |} """.stripMargin
      assertEquals(parse(input), result(f("a","foo"), nestedObject))
    }

    "parse an object property with a trailing comma" in {
      val input =
        """"a": "foo", 
          |"obj" = { 
          |  "inner": "xx", 
          |  "num": 9.5,
          |} """.stripMargin
      assertEquals(parse(input), result(f("a","foo"), nestedObject))
    }

    "parse an array property with a trailing comma" in {
      val input =
        """"a": "foo", 
          |"arr": [ 1, 2, "bar", ]""".stripMargin
      assertEquals(parse(input), result(f("a","foo"), arrayProperty))
    }

    "parse an array property with elements separated by newline characters" in {
      val input =
        """"a": "foo", 
          |"arr": [ 
          |  1 
          |  2 
          |  "bar"
          |]""".stripMargin
      assertEquals(parse(input), result(f("a","foo"), arrayProperty))
    }

    "parse a root object with members separated by newline characters" in {
      assertEquals(parse(
        """"a": "foo"
          |"b": "bar" 
          |"c": "baz" """.stripMargin), result(f("a","foo"),f("b","bar"),f("c","baz")))
    }

    "parse a root object with members separated by two newline characters" in {
      assertEquals(parse(
        """"a": "foo"
          |
          |"b": "bar"
          | 
          |"c": "baz" """.stripMargin), result(f("a","foo"),f("b","bar"),f("c","baz")))
    }

    "parse a multiline string property" in {
      val input =
        """"a": "foo", 
          |"s": +++Line 1
          | Line 2
          | Line 3+++""".stripMargin.replace("+++", "\"\"\"")
      assertEquals(parse(input), result(f("a","foo"), f("s", "Line 1\n Line 2\n Line 3")))
    }

    "parse a multiline string property with more than 3 closing quotes" in {
      val input =
        """"a": "foo", 
          |"s": +++Line 1
          | Line 2
          | Line 3+++++""".stripMargin.replace("+", "\"")
      assertEquals(parse(input), result(f("a","foo"), f("s", "Line 1\n Line 2\n Line 3\"\"")))
    }

    "ignore escapes in a multiline string property" in {
      val input =
        """"a": "foo", 
          |"s": +++Word 1 \n Word 2+++""".stripMargin.replace("+++", "\"\"\"")
      assertEquals(parse(input), result(f("a","foo"), f("s", "Word 1 \\n Word 2")))
    }

    "parse an object with unquoted keys" in {
      val input =
        """a: "foo", 
          |arr: [ 1, 2, "bar" ]""".stripMargin
      assertEquals(parse(input), result(f("a","foo"), arrayProperty))
    }

    "parse an object with unquoted string values" in {
      val input =
        """"a": foo, 
          |"arr": [ 1, 2, bar ]""".stripMargin
      assertEquals(parse(input), result(f("a","foo"), arrayProperty))
    }

    "parse an object with the += field separator" in {
      val input =
        """a = [ foo ], 
          |a += bar""".stripMargin
      assertEquals(parse(input), result(
        BuilderField("a", ArrayBuilderValue(Seq(stringValue("foo")))),
        BuilderField("a", ConcatValue(SelfReference, Seq(ConcatPart("", ArrayBuilderValue(Seq(stringValue("bar")))))))
      ))
    }
    
  }
  
  "The concatenated value parser" should {
    
    "parse simple values containing booleans" in {
      val input = "a = true is false"
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(trueValue, Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", falseValue))))
      ))
    }

    "parse simple values containing numbers" in {
      val input = "a = 9 is 7"
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(longValue(9), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", longValue(7)))))
      ))
    }

    "parse object values on a single line" in {
      val input = """a = { "inner": "xx", "num": 9.5 } { "inner": "xx", "num": 9.5 }"""
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(nestedObject.value, Seq(ConcatPart(" ", nestedObject.value))))
      ))
    }

    "parse object values spanning multiple lines" in {
      val input = """a = { 
                    |  "inner": "xx", 
                    |  "num": 9.5
                    |} { 
                    |  "inner": "xx", 
                    |  "num": 9.5
                    |}""".stripMargin
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(nestedObject.value, Seq(ConcatPart(" ", nestedObject.value))))
      ))
    }

    "parse array values on a single line" in {
      val input = """a = [ 1, 2, "bar", ] [ 1, 2, "bar", ]"""
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(arrayProperty.value, Seq(ConcatPart(" ", arrayProperty.value))))
      ))
    }

    "parse array values spanning multiple lines" in {
      val input = """a = [ 
                    | 1
                    | 2 
                    | "bar"
                    |] [ 
                    | 1 
                    | 2
                    | "bar"
                    |]""".stripMargin
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(arrayProperty.value, Seq(ConcatPart(" ", arrayProperty.value))))
      ))
    }

  }
  
  "The concatenated key parser" should {
    
    "parse a concatenated key consisting of unquoted strings" in {
      val input = """a b c = foo"""
      assertEquals(parse(input), result(f("a b c","foo")))
    }

    "parse a concatenated key consisting of unquoted and quoted strings" in {
      val input = """a "b" c = foo"""
      assertEquals(parse(input), result(f("a b c","foo")))
    }
    
  }
  
  "The substitution parser" should {
    
    "parse a substitution as a simple value" in {
      val input = "a = ${foo.bar}"
      assertEquals(parse(input), result(
        BuilderField("a", SubstitutionValue(Key("foo", "bar"), optional = false))
      ))
    }

    "parse a substitution for an optional value" in {
      val input = "a = ${?foo.bar}"
      assertEquals(parse(input), result(
        BuilderField("a", SubstitutionValue(Key("foo", "bar"), optional = true))
      ))
    }

    "parse a substitution as the first part in a concatenated value" in {
      val input = "a = ${foo.bar} is null"
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(SubstitutionValue(Key("foo", "bar"), optional = false), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", nullValue))))
      ))
    }

    "parse a substitution as the last part in a concatenated value" in {
      val input = "a = Blue is ${foo.bar}"
      assertEquals(parse(input), result(
        BuilderField("a", ConcatValue(stringValue("Blue"), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", SubstitutionValue(Key("foo", "bar"), optional = false)))))
      ))
    }
    
  }
  
  "The comment parser" should {
    
    "parse a comment at the beginning of the input" in {
      val input = """
        | // comment
        | 
        | a = 7
      """.stripMargin
      assertEquals(parse(input), result(BuilderField("a", longValue(7))))
    }

    "parse a comment at the end of the input" in {
      val input = """
                    | a = 7
                    |
                    | // comment
                  """.stripMargin
      assertEquals(parse(input), result(BuilderField("a", longValue(7))))
    }

    "parse a comment in the middle of the input" in {
      val input = """
                    | a = 7
                    |
                    | // comment
                    | 
                    | b = 9
                  """.stripMargin
      assertEquals(parse(input), result(BuilderField("a", longValue(7)), BuilderField("b", longValue(9))))
    }

    "parse multiple comments in the middle of the input" in {
      val input = """
                    | a = 7
                    |
                    | // comment
                    | 
                    | # some other comment
                    |
                    | b = 9
                  """.stripMargin
      assertEquals(parse(input), result(BuilderField("a", longValue(7)), BuilderField("b", longValue(9))))
    }

    "parse a comment next to an object member" in {
      val input = """
                    | a = 7   // comment
                    | 
                    | b = 9
                  """.stripMargin
      assertEquals(parse(input), result(BuilderField("a", longValue(7)), BuilderField("b", longValue(9))))
    }

    "parse a comment next to an array property" in {
      val input =
        """"a": "foo", 
          |"arr": [ 
          |  1  // hello
          |  2  # there
          |  "bar"
          |]""".stripMargin
      assertEquals(parse(input), result(f("a","foo"), arrayProperty))
    }
    
  }
  
  "The path expression parser" should {
    
    "parse an unquoted path" in {
      assertEquals(parse("foo.bar = 7"), result(BuilderField(Key("foo", "bar"), longValue(7))))
    }

    "parse an unquoted path with whitespace" in {
      assertEquals(parse("foo.bar bar.baz = 7"), result(BuilderField(Key("foo", "bar bar", "baz"), longValue(7))))
    }

    "parse a quoted path" in {
      assertEquals(parse("\"foo.bar\" = 7"), result(BuilderField(Key("foo.bar"), longValue(7))))
    }
    
    "parse a quoted and unquoted path combined" in {
      assertEquals(parse("foo.\"bar.bar\".baz = 7"), result(BuilderField(Key("foo", "bar.bar", "baz"), longValue(7))))
    }

    "parse a quoted empty string as a path element" in {
      assertEquals(parse("foo.\"\".baz = 7"), result(BuilderField(Key("foo", "", "baz"), longValue(7))))
    }
    
  }
  
  "The include parser" should {
    
    "parse an optional file include" in {
      val input = """include file("foo.conf")"""
      ConfigParser.parse(input).includes shouldBe Seq(IncludeFile(ValidStringValue("foo.conf")))
    }

    "parse an optional classpath include" in {
      val input = """include classpath("foo.conf")"""
      ConfigParser.parse(input).includes shouldBe Seq(IncludeClassPath(ValidStringValue("foo.conf")))
    }

    "parse an optional URL include" in {
      val input = """include url("http://config.com/foo.conf")"""
      ConfigParser.parse(input).includes shouldBe Seq(IncludeUrl(ValidStringValue("http://config.com/foo.conf")))
    }

    "parse an optional heuristic include" in {
      val input = """include "http://config.com/foo.conf""""
      ConfigParser.parse(input).includes shouldBe Seq(IncludeAny(ValidStringValue("http://config.com/foo.conf")))
    }

    "parse a required heuristic include" in {
      val input = """include required("http://config.com/foo.conf")"""
      ConfigParser.parse(input).includes shouldBe Seq(IncludeAny(ValidStringValue("http://config.com/foo.conf"), isRequired = true))
    }

    "parse a required file include" in {
      val input = """include required(file("foo.conf"))"""
      ConfigParser.parse(input).includes shouldBe Seq(IncludeFile(ValidStringValue("foo.conf"), isRequired = true))
    }
    
    "parse multiple include statements on different nesting levels" in {
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
      ConfigParser.parse(input).includes shouldBe Seq(
        IncludeFile(ValidStringValue("foo.conf")),
        IncludeClassPath(ValidStringValue("foo.conf")),
        IncludeUrl(ValidStringValue("http://config.com/foo.conf"))
      )
    }
    
  }
  
}
