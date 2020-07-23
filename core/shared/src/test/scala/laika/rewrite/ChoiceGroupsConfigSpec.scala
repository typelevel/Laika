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

package laika.rewrite

import laika.ast.helper.ModelBuilder
import laika.config.{Config, ConfigBuilder}
import laika.rewrite.nav.{ChoiceConfig, ChoiceGroupConfig, ChoiceGroupsConfig}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class ChoiceGroupsConfigSpec extends AnyWordSpec
  with Matchers
  with ModelBuilder {

  val groupFoo = ChoiceGroupConfig("foo", Seq(
    ChoiceConfig("foo-a", "foo-label-a"),
    ChoiceConfig("foo-b", "foo-label-b")
  ))
  val groupBar = ChoiceGroupConfig("bar", Seq(
    ChoiceConfig("bar-a", "bar-label-a"),
    ChoiceConfig("bar-b", "bar-label-b")
  ))
  val groupBaz = ChoiceGroupConfig("baz", Seq(
    ChoiceConfig("baz-a", "baz-label-a"),
    ChoiceConfig("baz-b", "baz-label-b")
  ))
  val groupFooSeparate = groupFoo.copy(separateEbooks = true)
  val groupBarSeparate = groupBar.copy(separateEbooks = true)

  def select (group: ChoiceGroupConfig, pos: Int): ChoiceGroupConfig =
    group.copy(choices = group.choices.updated(pos, group.choices(pos).copy(selected = true)))

  "ChoiceGroupsConfig.createChoiceCombinations" should {

    "succeed with an empty config" in {
      ChoiceGroupsConfig.createChoiceCombinations(Config.empty) shouldBe Seq(Config.empty)
    }

    "succeed with no choice groups in the config" in {
      val config = ConfigBuilder.empty.withValue(ChoiceGroupsConfig(Nil)).build
      val result = ChoiceGroupsConfig.createChoiceCombinations(config)
      result.size shouldBe 1
      result.head.get[ChoiceGroupsConfig] shouldBe Right(ChoiceGroupsConfig(Nil))
    }

    "succeed with a single choice group without separation" in {
      val config = ConfigBuilder.empty.withValue(ChoiceGroupsConfig(Seq(groupFoo))).build
      val result = ChoiceGroupsConfig.createChoiceCombinations(config)
      result.size shouldBe 1
      result.head.get[ChoiceGroupsConfig] shouldBe Right(ChoiceGroupsConfig(Seq(groupFoo)))
    }

    "succeed with a single choice group with separation" in {
      val config = ConfigBuilder.empty.withValue(ChoiceGroupsConfig(Seq(groupFooSeparate))).build
      val result = ChoiceGroupsConfig.createChoiceCombinations(config)
      val expectedGroup1 = ChoiceGroupsConfig(Seq(
        ChoiceGroupConfig("foo", Seq(
          ChoiceConfig("foo-a", "foo-label-a", selected = true),
          ChoiceConfig("foo-b", "foo-label-b")
        ), separateEbooks = true)
      ))
      val expectedGroup2 = ChoiceGroupsConfig(Seq(
        ChoiceGroupConfig("foo", Seq(
          ChoiceConfig("foo-a", "foo-label-a"),
          ChoiceConfig("foo-b", "foo-label-b", selected = true)
        ), separateEbooks = true)
      ))
      result.size shouldBe 2
      result.head.get[ChoiceGroupsConfig] shouldBe Right(expectedGroup1)
      result.tail.head.get[ChoiceGroupsConfig] shouldBe Right(expectedGroup2)
    }

    "succeed with a two choice groups with separation and one without" in {
      val config = ConfigBuilder.empty.withValue(ChoiceGroupsConfig(Seq(
        groupFooSeparate,
        groupBarSeparate,
        groupBaz))).build
      val result = ChoiceGroupsConfig.createChoiceCombinations(config).toIndexedSeq

      def groups (selectIn1: Int, selectIn2: Int): ChoiceGroupsConfig = ChoiceGroupsConfig(Seq(
        select(groupFooSeparate, selectIn1), select(groupBarSeparate, selectIn2), groupBaz
      ))

      result.size shouldBe 4
      result(0).get[ChoiceGroupsConfig] shouldBe Right(groups(0,0))
      result(1).get[ChoiceGroupsConfig] shouldBe Right(groups(0,1))
      result(2).get[ChoiceGroupsConfig] shouldBe Right(groups(1,0))
      result(3).get[ChoiceGroupsConfig] shouldBe Right(groups(1,1))
    }
  }

}
