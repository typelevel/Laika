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

import laika.ast.Path.Root
import laika.config.ConfigParser
import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import laika.parse.hocon.HoconParsers._
import laika.transform.helper.FileTransformerUtil
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class HoconParserSpec extends WordSpec with Matchers with ParseResultHelpers with StringParserHelpers with ResultBuilders {

  def f (key: String, value: String): BuilderField = BuilderField(key, stringValue(value))
  
  private val nestedObject = BuilderField("obj", ObjectBuilderValue(Seq(
    BuilderField("inner", stringValue("xx")),
    BuilderField("num", doubleValue(9.5))
  )))
  
  private val arrayProperty = BuilderField("arr", ArrayBuilderValue(Seq(
    longValue(1), longValue(2), stringValue("bar")
  )))
  
  "The object parser" should {

    "parse an empty root object that is not enclosed in braces" in {
      Parsing (" ") using rootObject should produce (ObjectBuilderValue(Nil))
    }

    "parse a root object with two properties that is not enclosed in braces" in {
      Parsing (""" "a": "foo", "b": "bar" """.stripMargin) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"),f("b","bar"))))
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
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        nestedObject,
        BuilderField("str1", stringValue("foo")),
        BuilderField("str2", stringValue("foo")),
        BuilderField("int", longValue(27)),
        BuilderField("null", nullValue),
        BuilderField("bool", trueValue),
        arrayProperty,
        nestedObject
      )))
    }

    "parse a root object with two properties that use '=' instead of ':'" in {
      Parsing (""" "a" = "foo", "b" = "bar" """.stripMargin) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"),f("b","bar"))))
    }

    "parse an object property without separator" in {
      val input =
        """"a": "foo", 
          |"obj" { 
          |  "inner": "xx", 
          |  "num": 9.5 
          |} """.stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), nestedObject)))
    }

    "parse an object property with a trailing comma" in {
      val input =
        """"a": "foo", 
          |"obj" = { 
          |  "inner": "xx", 
          |  "num": 9.5,
          |} """.stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), nestedObject)))
    }

    "parse an array property with a trailing comma" in {
      val input =
        """"a": "foo", 
          |"arr": [ 1, 2, "bar", ]""".stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), arrayProperty)))
    }

    "parse an array property with elements separated by newline characters" in {
      val input =
        """"a": "foo", 
          |"arr": [ 
          |  1 
          |  2 
          |  "bar"
          |]""".stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), arrayProperty)))
    }

    "parse a root object with members separated by newline characters" in {
      Parsing (
        """"a": "foo"
          |"b": "bar" 
          |"c": "baz" """.stripMargin) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"),f("b","bar"),f("c","baz"))))
    }

    "parse a root object with members separated by two newline characters" in {
      Parsing (
        """"a": "foo"
          |
          |"b": "bar"
          | 
          |"c": "baz" """.stripMargin) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"),f("b","bar"),f("c","baz"))))
    }

    "parse a multiline string property" in {
      val input =
        """"a": "foo", 
          |"s": +++Line 1
          | Line 2
          | Line 3+++""".stripMargin.replaceAllLiterally("+++", "\"\"\"")
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), f("s", "Line 1\n Line 2\n Line 3"))))
    }

    "ignore escapes in a multiline string property" in {
      val input =
        """"a": "foo", 
          |"s": +++Word 1 \n Word 2+++""".stripMargin.replaceAllLiterally("+++", "\"\"\"")
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), f("s", "Word 1 \\n Word 2"))))
    }

    "parse an object with unquoted keys" in {
      val input =
        """a: "foo", 
          |arr: [ 1, 2, "bar" ]""".stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), arrayProperty)))
    }

    "parse an object with unquoted string values" in {
      val input =
        """"a": foo, 
          |"arr": [ 1, 2, bar ]""".stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), arrayProperty)))
    }

    "parse an object with the += field separator" in {
      val input =
        """a = [ foo ], 
          |a += bar""".stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ArrayBuilderValue(Seq(stringValue("foo")))),
        BuilderField("a", ConcatValue(SelfReference, Seq(ConcatPart("", ArrayBuilderValue(Seq(stringValue("bar")))))))
      )))
    }
    
  }
  
  "The concatenated value parser" should {
    
    "parse simple values containing booleans" in {
      val input = "a = true is false"
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(trueValue, Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", falseValue))))
      )))
    }

    "parse simple values containing numbers" in {
      val input = "a = 9 is 7"
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(longValue(9), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", longValue(7)))))
      )))
    }

    "parse object values on a single line" in {
      val input = """a = { "inner": "xx", "num": 9.5 } { "inner": "xx", "num": 9.5 }"""
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(nestedObject.value, Seq(ConcatPart(" ", nestedObject.value))))
      )))
    }

    "parse object values spanning multiple lines" in {
      val input = """a = { 
                    |  "inner": "xx", 
                    |  "num": 9.5
                    |} { 
                    |  "inner": "xx", 
                    |  "num": 9.5
                    |}""".stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(nestedObject.value, Seq(ConcatPart(" ", nestedObject.value))))
      )))
    }

    "parse array values on a single line" in {
      val input = """a = [ 1, 2, "bar", ] [ 1, 2, "bar", ]"""
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(arrayProperty.value, Seq(ConcatPart(" ", arrayProperty.value))))
      )))
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
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(arrayProperty.value, Seq(ConcatPart(" ", arrayProperty.value))))
      )))
    }

  }
  
  "The concatenated key parser" should {
    
    "parse a concatenated key consisting of unquoted strings" in {
      val input = """a b c = foo"""
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a b c","foo"))))
    }

    "parse a concatenated key consisting of unquoted and quoted strings" in {
      val input = """a "b" c = foo"""
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a b c","foo"))))
    }
    
  }
  
  "The substitution parser" should {
    
    "parse a substitution as a simple value" in {
      val input = "a = ${foo.bar}"
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", SubstitutionValue(Root / "foo" / "bar", optional = false))
      )))
    }

    "parse a substitution for an optional value" in {
      val input = "a = ${?foo.bar}"
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", SubstitutionValue(Root / "foo" / "bar", optional = true))
      )))
    }

    "parse a substitution as the first part in a concatenated value" in {
      val input = "a = ${foo.bar} is null"
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(SubstitutionValue(Root / "foo" / "bar", optional = false), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", nullValue))))
      )))
    }

    "parse a substitution as the last part in a concatenated value" in {
      val input = "a = Blue is ${foo.bar}"
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(
        BuilderField("a", ConcatValue(stringValue("Blue"), Seq(ConcatPart(" ", stringValue("is")), ConcatPart(" ", SubstitutionValue(Root / "foo" / "bar", optional = false)))))
      )))
    }
    
  }
  
  "The comment parser" should {
    
    "parse a comment at the beginning of the input" in {
      val input = """
        | // comment
        | 
        | a = 7
      """.stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(BuilderField("a", longValue(7)))))
    }

    "parse a comment at the end of the input" in {
      val input = """
                    | a = 7
                    |
                    | // comment
                  """.stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(BuilderField("a", longValue(7)))))
    }

    "parse a comment in the middle of the input" in {
      val input = """
                    | a = 7
                    |
                    | // comment
                    | 
                    | b = 9
                  """.stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(BuilderField("a", longValue(7)), BuilderField("b", longValue(9)))))
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
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(BuilderField("a", longValue(7)), BuilderField("b", longValue(9)))))
    }

    "parse a comment next to an object member" in {
      val input = """
                    | a = 7   // comment
                    | 
                    | b = 9
                  """.stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(BuilderField("a", longValue(7)), BuilderField("b", longValue(9)))))
    }

    "parse a comment next to an array property" in {
      val input =
        """"a": "foo", 
          |"arr": [ 
          |  1  // hello
          |  2  # there
          |  "bar"
          |]""".stripMargin
      Parsing (input) using rootObject should produce (ObjectBuilderValue(Seq(f("a","foo"), arrayProperty)))
    }
    
  }
  
  "The path expression parser" should {
    
    "parse an unquoted path" in {
      Parsing ("foo.bar = 7") using rootObject should produce (ObjectBuilderValue(Seq(BuilderField(Root / "foo" / "bar", longValue(7)))))
    }

    "parse an unquoted path with whitespace" in {
      Parsing ("foo.bar bar.baz = 7") using rootObject should produce (ObjectBuilderValue(Seq(BuilderField(Root / "foo" / "bar bar" / "baz", longValue(7)))))
    }

    "parse a quoted path" in {
      Parsing ("\"foo.bar\" = 7") using rootObject should produce (ObjectBuilderValue(Seq(BuilderField(Root / "foo.bar", longValue(7)))))
    }
    
    "parse a quoted and unquoted path combined" in {
      Parsing ("foo.\"bar.bar\".baz = 7") using rootObject should produce (ObjectBuilderValue(Seq(BuilderField(Root / "foo" / "bar.bar" / "baz", longValue(7)))))
    }

    "parse a quoted empty string as a path element" in {
      Parsing ("foo.\"\".baz = 7") using rootObject should produce (ObjectBuilderValue(Seq(BuilderField(Root / "foo" / "" / "baz", longValue(7)))))
    }
    
  }
  
  "The root parser" should {
    
    "successfully parse the full Akka default configuration" in new FileTransformerUtil {
      val input = readFile(classPathResourcePath("/akka.conf"))
      println(ConfigParser.parse(input).resolve)
      ConfigParser.parse(input).resolve.isRight shouldBe true
    }
    
  }

}
