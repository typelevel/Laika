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
import laika.parse.hocon.HoconParsers.{ArrayValue, BuilderField, Field, LongValue, ObjectBuilderValue, ObjectValue, StringValue}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class ConfigResolverSpec extends WordSpec with Matchers with ResultBuilders {

  def parseAndResolve(input: String): ObjectValue = {
    val builder = HoconParsers.rootObject.parse(input).toOption.get
    ConfigResolver.resolve(builder)
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
