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

import cats.data.{NonEmptyChain, NonEmptyVector}
import laika.ast.Path
import laika.ast.Path.Root
import laika.ast.helper.ModelBuilder
import laika.config.Config.ConfigResult
import laika.config.{Config, ConfigBuilder, LaikaKeys}
import laika.rewrite.nav.{ChoiceConfig, SelectionConfig, Selections, Classifiers, CoverImage}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class SelectionConfigSpec extends AnyWordSpec
  with Matchers
  with ModelBuilder {

  val selectionFoo = SelectionConfig("foo", NonEmptyChain(
    ChoiceConfig("foo-a", "foo-label-a"),
    ChoiceConfig("foo-b", "foo-label-b")
  ))
  val selectionBar = SelectionConfig("bar", NonEmptyChain(
    ChoiceConfig("bar-a", "bar-label-a"),
    ChoiceConfig("bar-b", "bar-label-b")
  ))
  val selectionBaz = SelectionConfig("baz", NonEmptyChain(
    ChoiceConfig("baz-a", "baz-label-a"),
    ChoiceConfig("baz-b", "baz-label-b")
  ))
  val selectionFooSeparate = selectionFoo.copy(separateEbooks = true)
  val selectionBarSeparate = selectionBar.copy(separateEbooks = true)

  def run (config: Config): NonEmptyVector[ConfigResult[Selections]] =
    Selections.createCombinations(config).toNonEmptyVector.map(_._1.get[Selections])

  def runAndGetCoverImage (config: Config): NonEmptyVector[ConfigResult[Path]] =
    Selections.createCombinations(config).toNonEmptyVector.map(_._1.get[Path](LaikaKeys.root.child("epub").child(LaikaKeys.coverImage.local)))

  "Selections.createCombinations" should {

    "succeed with an empty config" in {
      Selections.createCombinations(Config.empty) shouldBe NonEmptyChain.one((Config.empty, Classifiers(Nil)))
    }

    "succeed with no choice groups in the config" in {
      val config = ConfigBuilder.empty.withValue(Selections.empty).build
      val result = Selections.createCombinations(config)
      result.length shouldBe 1
      result.head._1.get[Selections] shouldBe Right(Selections.empty)
    }

    "succeed with a single choice group without separation" in {
      val config = ConfigBuilder.empty.withValue(Selections(selectionFoo)).build
      val result = Selections.createCombinations(config)
      result.length shouldBe 1
      result.head._1.get[Selections] shouldBe Right(Selections(selectionFoo))
    }

    "succeed with a single choice group with separation" in {
      val config = ConfigBuilder.empty.withValue(Selections(selectionFooSeparate)).build
      val result = run(config)
      val expectedGroup1 = Selections(
        SelectionConfig("foo",
          ChoiceConfig("foo-a", "foo-label-a", selected = true),
          ChoiceConfig("foo-b", "foo-label-b")
        ).withSeparateEbooks
      )
      val expectedGroup2 = Selections(
        SelectionConfig("foo",
          ChoiceConfig("foo-a", "foo-label-a"),
          ChoiceConfig("foo-b", "foo-label-b", selected = true)
        ).withSeparateEbooks
      )
      result.length shouldBe 2
      result.get(0) shouldBe Some(Right(expectedGroup1))
      result.get(1) shouldBe Some(Right(expectedGroup2))
    }

    "succeed with a two choice groups with separation and one without" in {
      val config = ConfigBuilder.empty
        .withValue(Selections(selectionFooSeparate, selectionBarSeparate, selectionBaz))
        .build
      val result = run(config)

      def select (selection: SelectionConfig, pos: Int): SelectionConfig =
        selection.select(selection.choices.toNonEmptyVector.getUnsafe(pos))

      def groups (selectIn1: Int, selectIn2: Int): Selections = Selections(
        select(selectionFooSeparate, selectIn1), select(selectionBarSeparate, selectIn2), selectionBaz
      )

      result.length shouldBe 4
      result.get(0) shouldBe Some(Right(groups(0,0)))
      result.get(1) shouldBe Some(Right(groups(0,1)))
      result.get(2) shouldBe Some(Right(groups(1,0)))
      result.get(3) shouldBe Some(Right(groups(1,1)))
    }

    "should use per-classifier cover image configuration" in {
      val config = ConfigBuilder.empty
        .withValue(Selections(selectionFooSeparate))
        .withValue(LaikaKeys.coverImage, Root / "default.png")
        .withValue(LaikaKeys.coverImages, Seq(CoverImage(Root / "foo-a.png", "foo-a"), CoverImage(Root / "foo-b.png", "foo-b")))
        .build
      val result = runAndGetCoverImage(config)
      result.length shouldBe 2
      result.get(0) shouldBe Some(Right(Root / "foo-a.png"))
      result.get(1) shouldBe Some(Right(Root / "foo-b.png"))
    }

    "should use the default cover image if none has been specified for a classifier" in {
      val defaultPath = Root / "default.png"
      val config = ConfigBuilder.empty
        .withValue(Selections(selectionFooSeparate))
        .withValue(LaikaKeys.coverImage, defaultPath)
        .build
      val result = runAndGetCoverImage(config)
      result.length shouldBe 2
      result.get(0) shouldBe Some(Right(defaultPath))
      result.get(1) shouldBe Some(Right(defaultPath))
    }
  }

}
