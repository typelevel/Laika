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
import laika.parse.hocon.HoconParsers.{BuilderField, ObjectBuilderValue}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class ConfigResolverSpec extends WordSpec with Matchers with ResultBuilders {

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
