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

import laika.config.Config.IncludeMap
import laika.config._
import munit.FunSuite

/** @author Jens Halm
  */
class ConfigResolverSpec extends FunSuite with ResultBuilders {

  def parseAndResolve(
      input: String,
      fallback: Config = EmptyConfig,
      includes: IncludeMap = Map.empty
  ): Either[ConfigError, ObjectValue] = for {
    builder <- ConfigParser.parse(input).unresolved
    result  <- ConfigResolver.resolve(builder, Origin.root, fallback, includes)
  } yield result

  def run(input: String, expectedFields: Field*)(implicit loc: munit.Location): Unit =
    assertEquals(parseAndResolve(input), Right(ObjectValue(expectedFields)))

  def runWithFallback(input: String, fallback: Config, expectedFields: Field*)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(parseAndResolve(input, fallback), Right(ObjectValue(expectedFields)))

  def runWithIncludes(input: String, includes: IncludeMap, expectedFields: Field*)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(parseAndResolve(input, includes = includes), Right(ObjectValue(expectedFields)))

  def runFailure(
      input: String,
      expectedMessage: String,
      includes: IncludeMap = Map.empty,
      adjustMsg: String => String = identity
  )(implicit loc: munit.Location): Unit = {
    val res = parseAndResolve(input, includes = includes).left.map(e =>
      ConfigResolverError(adjustMsg(e.message))
    )
    assertEquals(res, Left(ConfigResolverError(adjustMsg(expectedMessage))))
  }

  test("resolve a simple object") {
    val input =
      """
        |a = 5
        |b = 7
      """.stripMargin
    run(input, Field("a", LongValue(5)), Field("b", LongValue(7)))
  }

  test("resolve an object with expanded paths") {
    val input =
      """
        |a.b = 5
        |a.c = 7
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("b", LongValue(5)),
            Field("c", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve an object with expanded paths 2 levels deep") {
    val input =
      """
        |a.b.c = 5
        |a.b.d = 7
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field(
              "b",
              ObjectValue(
                Seq(
                  Field("c", LongValue(5)),
                  Field("d", LongValue(7))
                )
              )
            )
          )
        )
      )
    )
  }

  test("merge two object definitions with the same path") {
    val input =
      """
        |a = { c = 5 }
        |a = { d = 7 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("c", LongValue(5)),
            Field("d", LongValue(7))
          )
        )
      )
    )
  }

  test("don't merge an object if there is a simple overriding value between them") {
    val input =
      """
        |a = { c = 5 }
        |a = 7
        |a = { d = 7 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("d", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve a nested object") {
    val input =
      """
        |a {
        |  b = 5
        |  c = 7
        |}  
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("b", LongValue(5)),
            Field("c", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve an array of simple values") {
    val input =
      """
        |a = [1,2,3]
      """.stripMargin
    run(input, Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3)))))
  }

  test("resolve an array of objects") {
    val input =
      """
        |a = [
        |  { name = foo }
        |  { name = bar }
        |  { name = baz }
        |]
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ArrayValue(
          Seq(
            ObjectValue(Seq(Field("name", StringValue("foo")))),
            ObjectValue(Seq(Field("name", StringValue("bar")))),
            ObjectValue(Seq(Field("name", StringValue("baz"))))
          )
        )
      )
    )
  }

  test("resolve an object with an overridden field") {
    val input =
      """
        |a = 5
        |a = 7
      """.stripMargin
    run(input, Field("a", LongValue(7)))
  }

  test("resolve a concatenated array") {
    val input =
      """
        |a = [1,2] [3,4]
      """.stripMargin
    run(input, Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4)))))
  }

  test("resolve a merged object") {
    val input =
      """
        |a = { b = 5 } { c = 7 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("b", LongValue(5)),
            Field("c", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve a concatenated string") {
    val input =
      """
        |a = nothing is null
      """.stripMargin
    run(input, Field("a", StringValue("nothing is null")))
  }

  test("resolve a backward looking reference to a simple value") {
    val input =
      """
        |a = 5
        |b = ${a}
      """.stripMargin
    run(input, Field("a", LongValue(5)), Field("b", LongValue(5)))
  }

  test("resolve a forward looking reference to a simple value") {
    val input =
      """
        |a = ${b}
        |b = 5
      """.stripMargin
    run(input, Field("a", LongValue(5)), Field("b", LongValue(5)))
  }

  test("resolve a reference to a value declared in the fallback config") {
    val input    =
      """
        |b = ${a}
      """.stripMargin
    val fallback = ConfigBuilder.empty.withValue("a", 5).build
    runWithFallback(input, fallback, Field("b", LongValue(5)))
  }

  test("ignore an optional, missing reference") {
    val input =
      """
        |a = ${?x}
        |b = 5
      """.stripMargin
    run(input, Field("b", LongValue(5)))
  }

  test("ignore an optional, missing self reference") {
    val input =
      """
        |a = ${?a} [5,7]
      """.stripMargin
    run(input, Field("a", ArrayValue(Seq(LongValue(5), LongValue(7)))))
  }

  test("fail with a missing required reference") {
    val input =
      """
        |a = ${x}
        |b = 5
      """.stripMargin
    runFailure(
      input,
      "One or more errors resolving configuration: 'a': Missing required reference: 'x'"
    )
  }

  test("fail with a circular reference") {
    def adjustMsg(msg: String): String = msg.replaceAll("'.'", "'x'")
    val input                          =
      """
        |a = ${c}
        |b = ${a}
        |c = ${b}
      """.stripMargin
    val msg                            =
      "One or more errors resolving configuration: 'c': Circular Reference involving path 'c'"
    runFailure(input, msg, adjustMsg = adjustMsg)
  }

  test("fail with a circular reference to a parent node") {
    val input =
      """
        |a = { x = 5, y = ${a} }
      """.stripMargin
    runFailure(
      input,
      "One or more errors resolving configuration: 'a': Circular Reference involving path 'a'"
    )
  }

  test("resolve a backward looking reference to a simple value with a common path segment") {
    val input =
      """
        |o = { a = 5, b = ${o.a} }
      """.stripMargin
    run(
      input,
      Field(
        "o",
        ObjectValue(
          Seq(
            Field("a", LongValue(5)),
            Field("b", LongValue(5))
          )
        )
      )
    )
  }

  test("resolve a forward looking reference to a simple value with a common path segment") {
    val input =
      """
        |o = { a = ${o.b}, b = 5 }
      """.stripMargin
    run(
      input,
      Field(
        "o",
        ObjectValue(
          Seq(
            Field("a", LongValue(5)),
            Field("b", LongValue(5))
          )
        )
      )
    )
  }

  test("resolve a backward looking reference to another object") {
    val input =
      """
        |a = { a1 = 5, a2 = { foo = bar } }
        |b = { b1 = 9, b2 = ${a.a2} }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("a1", LongValue(5)),
            Field("a2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
          )
        )
      ),
      Field(
        "b",
        ObjectValue(
          Seq(
            Field("b1", LongValue(9)),
            Field("b2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
          )
        )
      )
    )
  }

  test("resolve a forward looking reference to another object") {
    val input =
      """
        |a = { a1 = 5, a2 = ${b.b2} }
        |b = { b1 = 9, b2 = { foo = bar } }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("a1", LongValue(5)),
            Field("a2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
          )
        )
      ),
      Field(
        "b",
        ObjectValue(
          Seq(
            Field("b1", LongValue(9)),
            Field("b2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
          )
        )
      )
    )
  }

  test("resolve a backward looking reference in a concatenated string") {
    val input =
      """
        |a = yes
        |b = ${a} or no
      """.stripMargin
    run(input, Field("a", StringValue("yes")), Field("b", StringValue("yes or no")))
  }

  test("resolve a forward looking reference in a concatenated string") {
    val input =
      """
        |a = ${b} or no
        |b = yes
      """.stripMargin
    run(input, Field("a", StringValue("yes or no")), Field("b", StringValue("yes")))
  }

  test("ignore a missing, optional reference in a concatenated string") {
    val input =
      """
        |a = ${?x} or no
      """.stripMargin
    run(input, Field("a", StringValue(" or no")))
  }

  test("resolve a backward looking reference in a concatenated array") {
    val input =
      """
        |a = [1,2]
        |b = ${a} [3,4]
      """.stripMargin
    run(
      input,
      Field("a", ArrayValue(Seq(LongValue(1), LongValue(2)))),
      Field("b", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4))))
    )
  }

  test("resolve a forward looking reference in a concatenated array") {
    val input =
      """
        |a = ${b} [3,4]
        |b = [1,2]
      """.stripMargin
    run(
      input,
      Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4)))),
      Field("b", ArrayValue(Seq(LongValue(1), LongValue(2))))
    )
  }

  test("ignore a missing, optional reference in a concatenated array") {
    val input =
      """
        |a = [1,2] ${?x} [3,4]
      """.stripMargin
    run(input, Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4)))))
  }

  test("fail with a missing required reference in a concatenated array") {
    val input =
      """
        |a = [1,2] ${x} [3,4]
      """.stripMargin
    runFailure(
      input,
      "One or more errors resolving configuration: 'a': Missing required reference: 'x'"
    )
  }

  test("fail when the combination of types is invalid in concatenated field") {
    val input =
      """
        |a = { x = 9 }
        |b = [1,2] ${a} [3,4]
      """.stripMargin
    val msg   =
      "One or more errors resolving configuration: 'b': Invalid concatenation of values. It must contain either only objects, only arrays or only simple values"
    runFailure(input, msg)
  }

  test("resolve a self reference in a concatenated array") {
    val input =
      """
        |a = [1,2]
        |a = ${a} [3,4]
      """.stripMargin
    run(input, Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4)))))
  }

  test("resolve a self reference via += in a concatenated array") {
    val input =
      """
        |a = [1,2]
        |a += 3
      """.stripMargin
    run(input, Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3)))))
  }

  test("resolve a self reference via += as the first occurrence in the input") {
    val input =
      """
        |a += 1
        |a += 2
      """.stripMargin
    run(input, Field("a", ArrayValue(Seq(LongValue(1), LongValue(2)))))
  }

  test("resolve a backward looking reference in a concatenated object") {
    val input =
      """
        |a = { a = 5 }
        |b = ${a} { b = 7 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("a", LongValue(5))
          )
        )
      ),
      Field(
        "b",
        ObjectValue(
          Seq(
            Field("a", LongValue(5)),
            Field("b", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve a forward looking reference in a concatenated object") {
    val input =
      """
        |a = ${b} { b = 7 }
        |b = { a = 5 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("a", LongValue(5)),
            Field("b", LongValue(7))
          )
        )
      ),
      Field(
        "b",
        ObjectValue(
          Seq(
            Field("a", LongValue(5))
          )
        )
      )
    )
  }

  test("ignore an optional, missing reference in a concatenated object") {
    val input =
      """
        |a = ${?x} { a = 5, b = 7 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("a", LongValue(5)),
            Field("b", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve a backward looking reference in a merged object") {
    val input =
      """
        |a = { c = 5 }
        |b = { d = 7 }
        |b = ${a}
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("c", LongValue(5))
          )
        )
      ),
      Field(
        "b",
        ObjectValue(
          Seq(
            Field("c", LongValue(5)),
            Field("d", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve a forward looking reference in a merged object") {
    val input =
      """
        |a = { c = 5 }
        |a = ${b}
        |b = { d = 7 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("c", LongValue(5)),
            Field("d", LongValue(7))
          )
        )
      ),
      Field(
        "b",
        ObjectValue(
          Seq(
            Field("d", LongValue(7))
          )
        )
      )
    )
  }

  test("ignore a missing, optional reference in a merged object") {
    val input =
      """
        |b = { c = 5, d = 7 }
        |b = ${?x}
      """.stripMargin
    run(
      input,
      Field(
        "b",
        ObjectValue(
          Seq(
            Field("c", LongValue(5)),
            Field("d", LongValue(7))
          )
        )
      )
    )
  }

  test("resolve a self reference in a merged object") {
    val input =
      """
        |a = { b = 5 }
        |a = ${a} { c = 7 }
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("b", LongValue(5)),
            Field("c", LongValue(7))
          )
        )
      )
    )
  }

  test("ignore a missing reference when it is later overridden") {
    val input =
      """
        |a = ${non-existing}
        |a = 5
      """.stripMargin
    run(input, Field("a", LongValue(5)))
  }

  test("resolve a self reference in a nested object") {
    val input =
      """
        |a { 
        |  b = { c = 5 }
        |  b = ${a.b} { d = 7 }
        |}
      """.stripMargin
    run(
      input,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field(
              "b",
              ObjectValue(
                Seq(
                  Field("c", LongValue(5)),
                  Field("d", LongValue(7))
                )
              )
            )
          )
        )
      )
    )
  }

  val includes: IncludeMap = Map(
    IncludeFile(ValidStringValue("foo.conf")) -> Right(
      ObjectBuilderValue(
        Seq(
          BuilderField(Right(Key("c")), ResolvedBuilderValue(LongValue(5)))
        )
      )
    )
  )

  test("include on the top level") {
    val input =
      """
        |b = 7
        |include file("foo.conf")
        |
      """.stripMargin
    runWithIncludes(input, includes, Field("b", LongValue(7)), Field("c", LongValue(5)))
  }

  test("resolve a nested include") {
    val input =
      """
        |a { 
        |  b = 7
        |  include file("foo.conf")
        |}
      """.stripMargin
    runWithIncludes(
      input,
      includes,
      Field(
        "a",
        ObjectValue(
          Seq(
            Field("b", LongValue(7)),
            Field("c", LongValue(5))
          )
        )
      )
    )
  }

  test("ignore a missing optional resource") {
    val input =
      """
        |b = 7
        |include file("foo.conf")
        |
      """.stripMargin
    run(input, Field("b", LongValue(7)))
  }

  test("fail with a missing required resource") {
    val input =
      """
        |b = 7
        |include required(file("foo.conf"))
        |
      """.stripMargin
    runFailure(
      input,
      "One or more errors resolving configuration: '<RootKey>': Missing required include 'foo.conf'"
    )
  }

  test("fail when an include failed to load") {
    val includes: IncludeMap = Map(
      IncludeFile(ValidStringValue("foo.conf")) -> Left(ValidationError("This include is faulty"))
    )
    val input                = {
      """
        |b = 7
        |include file("foo.conf")
        |
      """.stripMargin
    }
    val msg                  =
      "One or more errors resolving configuration: '<RootKey>': Error including 'foo.conf': This include is faulty"
    runFailure(input, msg, includes)
  }

  test("expand a single path") {
    val in       = ObjectBuilderValue(Seq(BuilderField(Key("foo", "bar", "baz"), longValue(7))))
    val expected = ObjectBuilderValue(
      Seq(
        BuilderField(
          Key("foo"),
          ObjectBuilderValue(
            Seq(
              BuilderField(
                Key("foo", "bar"),
                ObjectBuilderValue(
                  Seq(BuilderField(Key("foo", "bar", "baz"), longValue(7)))
                )
              )
            )
          )
        )
      )
    )
    assertEquals(ConfigResolver.expandPaths(in), expected)
  }

  test("expand a nested path") {
    val in       = ObjectBuilderValue(
      Seq(
        BuilderField(
          Key("foo"),
          ObjectBuilderValue(
            Seq(BuilderField(Key("bar", "baz"), longValue(7)))
          )
        )
      )
    )
    val expected = ObjectBuilderValue(
      Seq(
        BuilderField(
          Key("foo"),
          ObjectBuilderValue(
            Seq(
              BuilderField(
                Key("foo", "bar"),
                ObjectBuilderValue(
                  Seq(BuilderField(Key("foo", "bar", "baz"), longValue(7)))
                )
              )
            )
          )
        )
      )
    )
    assertEquals(ConfigResolver.expandPaths(in), expected)
  }

}
