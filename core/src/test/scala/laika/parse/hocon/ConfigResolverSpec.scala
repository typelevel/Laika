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
import laika.config.{ArrayValue, Config, ConfigBuilder, ConfigError, ConfigResolverError, EmptyConfig, Field, LongValue, ObjectValue, Origin, StringValue}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class ConfigResolverSpec extends WordSpec with Matchers with ResultBuilders {

  def parseAndResolve(input: String): ObjectValue = {
    val builder = HoconParsers.rootObject.parse(input).toOption.get
    ConfigResolver.resolve(builder, Origin.root, EmptyConfig).toOption.get
  }

  def parseAndResolve(input: String, fallback: Config): ObjectValue = {
    val builder = HoconParsers.rootObject.parse(input).toOption.get
    ConfigResolver.resolve(builder, Origin.root, fallback).toOption.get
  }

  def parseAndResolveForFailure(input: String): Either[ConfigError, ObjectValue] = {
    val builder = HoconParsers.rootObject.parse(input).toOption.get
    ConfigResolver.resolve(builder, Origin.root, EmptyConfig)
  }
   
  "The config resolver" should {

    "resolve a simple object" in {
      val input =
        """
          |a = 5
          |b = 7
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", LongValue(5)),
        Field("b", LongValue(7))
      ))
    }

    "resolve an object with expanded paths" in {
      val input =
        """
          |a.b = 5
          |a.c = 7
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("b", LongValue(5)),
          Field("c", LongValue(7))
        )))
      ))
    }

    "resolve an object with expanded paths 2 levels deep" in {
      val input =
        """
          |a.b.c = 5
          |a.b.d = 7
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("b", ObjectValue(Seq(
            Field("c", LongValue(5)),
            Field("d", LongValue(7))
          )))
        )))
      ))
    }

    "merge two object definitions with the same path" in {
      val input =
        """
          |a = { c = 5 }
          |a = { d = 7 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("c", LongValue(5)),
          Field("d", LongValue(7)),
        )))
      ))
    }

    "don't merge an object if there is a simple overriding value between them" in {
      val input =
        """
          |a = { c = 5 }
          |a = 7
          |a = { d = 7 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("d", LongValue(7)),
        )))
      ))
    }

    "resolve a nested object" in {
      val input =
        """
          |a {
          |  b = 5
          |  c = 7
          |}  
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("b", LongValue(5)),
          Field("c", LongValue(7))
        )))
      ))
    }

    "resolve an array of simple values" in {
      val input =
        """
          |a = [1,2,3]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3))))
      ))
    }

    "resolve an array of objects" in {
      val input =
        """
          |a = [
          |  { name = foo }
          |  { name = bar }
          |  { name = baz }
          |]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(
          ObjectValue(Seq(Field("name", StringValue("foo")))),
          ObjectValue(Seq(Field("name", StringValue("bar")))),
          ObjectValue(Seq(Field("name", StringValue("baz")))),
        )))
      ))
    }

    "resolve an object with an overridden field" in {
      val input =
        """
          |a = 5
          |a = 7
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", LongValue(7))
      ))
    }

    "resolve a concatenated array" in {
      val input =
        """
          |a = [1,2] [3,4]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4))))
      ))
    }

    "resolve a merged object" in {
      val input =
        """
          |a = { b = 5 } { c = 7 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("b", LongValue(5)),
          Field("c", LongValue(7))
        )))
      ))
    }

    "resolve a concatenated string" in {
      val input =
        """
          |a = nothing is null
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", StringValue("nothing is null"))
      ))
    }

  }

  "The reference resolver" should {
    
    "resolve a backward looking reference to a simple value" in {
      val input =
        """
          |a = 5
          |b = ${a}
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", LongValue(5)),
        Field("b", LongValue(5))
      ))
    }

    "resolve a forward looking reference to a simple value" in {
      val input =
        """
          |a = ${b}
          |b = 5
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", LongValue(5)),
        Field("b", LongValue(5))
      ))
    }

    "resolve a reference to a value declared in the fallback config" in {
      val input =
        """
          |b = ${a}
        """.stripMargin
      val fallback = ConfigBuilder.empty.withValue("a", 5).build
      parseAndResolve(input, fallback) shouldBe ObjectValue(Seq(
        Field("b", LongValue(5))
      ))
    }

    "ignore an optional, missing reference" in {
      val input =
        """
          |a = ${?x}
          |b = 5
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("b", LongValue(5))
      ))
    }

    "ignore an optional, missing self reference" in {
      val input =
        """
          |a = ${?a} [5,7]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(5),LongValue(7))))
      ))
    }

    "fail with a missing required reference" in {
      val input =
        """
          |a = ${x}
          |b = 5
        """.stripMargin
      parseAndResolveForFailure(input) shouldBe Left(ConfigResolverError("One or more errors resolving configuration: 'a': Missing required reference: 'x'"))
    }

    "fail with a circular reference" in {
      def adjustMsg(res: Either[ConfigError, ObjectValue]): Either[ConfigError, ObjectValue] =
        res.left.map(e => ConfigResolverError(e.message.replaceAll("'.'", "'x'")))
      val input =
        """
          |a = ${c}
          |b = ${a}
          |c = ${b}
        """.stripMargin
      adjustMsg(parseAndResolveForFailure(input)) shouldBe adjustMsg(Left(ConfigResolverError("One or more errors resolving configuration: 'c': Circular Reference involving path 'c'")))
    }

    "fail with a circular reference to a parent node" in {
      val input =
        """
          |a = { x = 5, y = ${a} }
        """.stripMargin
      parseAndResolveForFailure(input) shouldBe Left(ConfigResolverError("One or more errors resolving configuration: 'a': Circular Reference involving path 'a'"))
    }

    "resolve a backward looking reference to a simple value with a common path segment" in {
      val input =
        """
          |o = { a = 5, b = ${o.a} }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(Field("o",
        ObjectValue(Seq(
          Field("a", LongValue(5)),
          Field("b", LongValue(5))
        ))
      )))
    }

    "resolve a forward looking reference to a simple value with a common path segment" in {
      val input =
        """
          |o = { a = ${o.b}, b = 5 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(Field("o",
        ObjectValue(Seq(
          Field("a", LongValue(5)),
          Field("b", LongValue(5))
        ))
      ))) 
    }

    "resolve a backward looking reference to another object" in {
      val input =
        """
          |a = { a1 = 5, a2 = { foo = bar } }
          |b = { b1 = 9, b2 = ${a.a2} }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("a1", LongValue(5)),
          Field("a2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
        ))),
        Field("b", ObjectValue(Seq(
          Field("b1", LongValue(9)),
          Field("b2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
        )))
      ))
    }

    "resolve a forward looking reference to another object" in {
      val input =
        """
          |a = { a1 = 5, a2 = ${b.b2} }
          |b = { b1 = 9, b2 = { foo = bar } }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("a1", LongValue(5)),
          Field("a2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
        ))),
        Field("b", ObjectValue(Seq(
          Field("b1", LongValue(9)),
          Field("b2", ObjectValue(Seq(Field("foo", StringValue("bar")))))
        )))
      ))
    }

    "resolve a backward looking reference in a concatenated string" in {
      val input =
        """
          |a = yes
          |b = ${a} or no
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", StringValue("yes")),
        Field("b", StringValue("yes or no"))
      ))
    }

    "resolve a forward looking reference in a concatenated string" in {
      val input =
        """
          |a = ${b} or no
          |b = yes
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", StringValue("yes or no")),
        Field("b", StringValue("yes"))
      ))
    }

    "ignore a missing, optional reference in a concatenated string" in {
      val input =
        """
          |a = ${?x} or no
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", StringValue(" or no"))
      ))
    }

    "resolve a backward looking reference in a concatenated array" in {
      val input =
        """
          |a = [1,2]
          |b = ${a} [3,4]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2)))),
        Field("b", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4))))
      ))
    }

    "resolve a forward looking reference in a concatenated array" in {
      val input =
        """
          |a = ${b} [3,4]
          |b = [1,2]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4)))),
        Field("b", ArrayValue(Seq(LongValue(1), LongValue(2))))
      ))
    }

    "ignore a missing, optional reference in a concatenated array" in {
      val input =
        """
          |a = [1,2] ${?x} [3,4]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4))))
      ))
    }

    "fail with a missing required reference in a concatenated array" in {
      val input =
        """
          |a = [1,2] ${x} [3,4]
        """.stripMargin
      parseAndResolveForFailure(input) shouldBe Left(ConfigResolverError("One or more errors resolving configuration: 'a': Missing required reference: 'x'"))
    }

    "fail when the combination of types is invalid in concatenated field" in {
      val input =
        """
          |a = { x = 9 }
          |b = [1,2] ${a} [3,4]
        """.stripMargin
      val msg = "One or more errors resolving configuration: 'b': Invalid concatenation of values. It must contain either only objects, only arrays or only simple values"
      parseAndResolveForFailure(input) shouldBe Left(ConfigResolverError(msg))
    }

    "resolve a self reference in a concatenated array" in {
      val input =
        """
          |a = [1,2]
          |a = ${a} [3,4]
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3), LongValue(4))))
      ))
    }

    "resolve a self reference via += in a concatenated array" in {
      val input =
        """
          |a = [1,2]
          |a += 3
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2), LongValue(3))))
      ))
    }

    "resolve a self reference via += as the first occurrence in the input" in {
      val input =
        """
          |a += 1
          |a += 2
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ArrayValue(Seq(LongValue(1), LongValue(2))))
      ))
    }

    "resolve a backward looking reference in a concatenated object" in {
      val input =
        """
          |a = { a = 5 }
          |b = ${a} { b = 7 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("a", LongValue(5))
        ))),
        Field("b", ObjectValue(Seq(
          Field("a", LongValue(5)),
          Field("b", LongValue(7))
        )))
      ))
    }

    "resolve a forward looking reference in a concatenated object" in {
      val input =
        """
          |a = ${b} { b = 7 }
          |b = { a = 5 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("a", LongValue(5)),
          Field("b", LongValue(7))
        ))),
        Field("b", ObjectValue(Seq(
          Field("a", LongValue(5)),
        )))
      ))
    }

    "ignore an optional, missing reference in a concatenated object" in {
      val input =
        """
          |a = ${?x} { a = 5, b = 7 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("a", LongValue(5)),
          Field("b", LongValue(7))
        )))
      ))
    }

    "resolve a backward looking reference in a merged object" in {
      val input =
        """
          |a = { c = 5 }
          |b = { d = 7 }
          |b = ${a}
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("c", LongValue(5))
        ))),
        Field("b", ObjectValue(Seq(
          Field("c", LongValue(5)),
          Field("d", LongValue(7))
        )))
      ))
    }

    "resolve a forward looking reference in a merged object" in {
      val input =
        """
          |a = { c = 5 }
          |a = ${b}
          |b = { d = 7 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("c", LongValue(5)),
          Field("d", LongValue(7))
        ))),
        Field("b", ObjectValue(Seq(
          Field("d", LongValue(7)),
        )))
      ))
    }

    "ignore a missing, optional reference in a merged object" in {
      val input =
        """
          |b = { c = 5, d = 7 }
          |b = ${?x}
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("b", ObjectValue(Seq(
          Field("c", LongValue(5)),
          Field("d", LongValue(7))
        )))
      ))
    }

    "resolve a self reference in a merged object" in {
      val input =
        """
          |a = { b = 5 }
          |a = ${a} { c = 7 }
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", ObjectValue(Seq(
          Field("b", LongValue(5)),
          Field("c", LongValue(7))
        )))
      ))
    }

    "ignore a missing reference when it is later overridden" in {
      val input =
        """
          |a = ${non-existing}
          |a = 5
        """.stripMargin
      parseAndResolve(input) shouldBe ObjectValue(Seq(
        Field("a", LongValue(5))
      ))
    }
    
  }
  
  "The path expansion" should {
    
    "expand a single path" in {
      val in = ObjectBuilderValue(Seq(BuilderField(Root / "foo" / "bar" / "baz", longValue(7))))
      val expected = ObjectBuilderValue(
        Seq(BuilderField(Root / "foo", ObjectBuilderValue(
          Seq(BuilderField(Root / "foo" / "bar", ObjectBuilderValue(
            Seq(BuilderField(Root / "foo" / "bar" / "baz", longValue(7)))
          )))
        )))
      )
      ConfigResolver.expandPaths(in) shouldBe expected
    }

    "expand a nested path" in {
      val in = ObjectBuilderValue(
        Seq(BuilderField(Root / "foo", ObjectBuilderValue(
          Seq(BuilderField(Root / "bar" / "baz", longValue(7)))
        )))
      )
      val expected = ObjectBuilderValue(
        Seq(BuilderField(Root / "foo", ObjectBuilderValue(
          Seq(BuilderField(Root / "foo" / "bar", ObjectBuilderValue(
            Seq(BuilderField(Root / "foo" / "bar" / "baz", longValue(7)))
          )))
        )))
      )
      ConfigResolver.expandPaths(in) shouldBe expected
    }
     
  }
   
}
