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

import cats.data.{ NonEmptyChain, NonEmptyVector }
import cats.syntax.all._
import laika.ast.Path
import laika.ast.Path.Root
import laika.config.Config.ConfigResult
import laika.config.{ Config, ConfigBuilder, LaikaKeys }
import laika.rewrite.nav.{ ChoiceConfig, Classifiers, CoverImage, SelectionConfig, Selections }
import munit.FunSuite

/** @author Jens Halm
  */
class SelectionConfigSpec extends FunSuite {

  val selectionFoo = SelectionConfig(
    "foo",
    NonEmptyChain(
      ChoiceConfig("foo-a", "foo-label-a"),
      ChoiceConfig("foo-b", "foo-label-b")
    )
  )

  val selectionBar = SelectionConfig(
    "bar",
    NonEmptyChain(
      ChoiceConfig("bar-a", "bar-label-a"),
      ChoiceConfig("bar-b", "bar-label-b")
    )
  )

  val selectionBaz = SelectionConfig(
    "baz",
    NonEmptyChain(
      ChoiceConfig("baz-a", "baz-label-a"),
      ChoiceConfig("baz-b", "baz-label-b")
    )
  )

  val selectionFooSeparate = selectionFoo.copy(separateEbooks = true)
  val selectionBarSeparate = selectionBar.copy(separateEbooks = true)

  private val epubCoverImgKey = LaikaKeys.root.child("epub").child(LaikaKeys.coverImage.local)

  def run(config: Config): ConfigResult[NonEmptyVector[Selections]] = Selections
    .createCombinations(config)
    .flatMap(_.toNonEmptyVector.traverse(_._1.get[Selections]))

  def runAndGetCoverImage(config: Config): ConfigResult[NonEmptyVector[Path]] = Selections
    .createCombinations(config)
    .flatMap(_.toNonEmptyVector.traverse(_._1.get[Path](epubCoverImgKey)))

  test("succeed with an empty config") {
    val result   = Selections.createCombinations(Config.empty).map { result =>
      (result.length, result.head._1.get[Selections], result.head._2)
    }
    val expected = (1L, Right(Selections.empty), Classifiers(Nil))
    assertEquals(result, Right(expected))
  }

  test("succeed with no choice groups in the config") {
    val config   = ConfigBuilder.empty.withValue(Selections.empty).build
    val result   = Selections.createCombinations(config).map { result =>
      (result.length, result.head._1.get[Selections])
    }
    val expected = (1L, Right(Selections.empty))
    assertEquals(result, Right(expected))
  }

  test("succeed with a single choice group without separation") {
    val config   = ConfigBuilder.empty.withValue(Selections(selectionFoo)).build
    val result   = Selections.createCombinations(config).map { result =>
      (result.length, result.head._1.get[Selections])
    }
    val expected = (1L, Right(Selections(selectionFoo)))
    assertEquals(result, Right(expected))
  }

  test("succeed with a single choice group with separation") {
    val config         = ConfigBuilder.empty.withValue(Selections(selectionFooSeparate)).build
    val result         = run(config)
    val expectedGroup1 = Selections(
      SelectionConfig(
        "foo",
        ChoiceConfig("foo-a", "foo-label-a", selected = true),
        ChoiceConfig("foo-b", "foo-label-b")
      ).withSeparateEbooks
    )
    val expectedGroup2 = Selections(
      SelectionConfig(
        "foo",
        ChoiceConfig("foo-a", "foo-label-a"),
        ChoiceConfig("foo-b", "foo-label-b", selected = true)
      ).withSeparateEbooks
    )
    val expected       = NonEmptyVector.of(expectedGroup1, expectedGroup2)
    assertEquals(result, Right(expected))
  }

  test("succeed with a two choice groups with separation and one without") {
    val config = ConfigBuilder.empty
      .withValue(Selections(selectionFooSeparate, selectionBarSeparate, selectionBaz))
      .build
    val result = run(config)

    def select(selection: SelectionConfig, pos: Int): SelectionConfig =
      selection.select(selection.choices.toNonEmptyVector.getUnsafe(pos))

    def groups(selectIn1: Int, selectIn2: Int): Selections = Selections(
      select(selectionFooSeparate, selectIn1),
      select(selectionBarSeparate, selectIn2),
      selectionBaz
    )
    val expected = NonEmptyVector.of(groups(0, 0), groups(0, 1), groups(1, 0), groups(1, 1))
    assertEquals(result, Right(expected))
  }

  test("should use per-classifier cover image configuration") {
    val config   = ConfigBuilder.empty
      .withValue(Selections(selectionFooSeparate))
      .withValue(LaikaKeys.coverImage, Root / "default.png")
      .withValue(
        LaikaKeys.coverImages,
        Seq(CoverImage(Root / "foo-a.png", "foo-a"), CoverImage(Root / "foo-b.png", "foo-b"))
      )
      .build
    val result   = runAndGetCoverImage(config)
    val expected = NonEmptyVector.of(Root / "foo-a.png", Root / "foo-b.png")
    assertEquals(result, Right(expected))
  }

  test("should use the default cover image if none has been specified for a classifier") {
    val defaultPath = Root / "default.png"
    val config      = ConfigBuilder.empty
      .withValue(Selections(selectionFooSeparate))
      .withValue(LaikaKeys.coverImage, defaultPath)
      .build
    val result      = runAndGetCoverImage(config)
    val expected    = NonEmptyVector.of(defaultPath, defaultPath)
    assertEquals(result, Right(expected))
  }

}
