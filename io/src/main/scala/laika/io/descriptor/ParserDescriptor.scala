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

package laika.io.descriptor

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import laika.io.api.TreeParser

/** Provides a description of a parsing operation, including the parsers
  * and extension bundles used, as well as the input sources.
  * This functionality is mostly intended for tooling support.
  *
  * @author Jens Halm
  */
case class ParserDescriptor(
    parsers: NonEmptyList[String],
    bundles: Seq[ExtensionBundleDescriptor],
    inputs: TreeInputDescriptor,
    theme: ThemeDescriptor,
    strict: Boolean,
    acceptRawContent: Boolean
) {

  def formatted: String = {
    s"""Parser(s):
       |  ${parsers.toList.mkString("\n  ")}
       |Extension Bundles:
       |  ${bundles.mkString("\n  ")}
       |Theme:
       |  ${theme.formatted}
       |Settings:
       |  Strict Mode: $strict
       |  Accept Raw Content: $acceptRawContent
       |Sources:
       |  ${inputs.formatted}""".stripMargin
  }

}

object ParserDescriptor {

  def create[F[_]: Sync](op: TreeParser.Op[F]): F[ParserDescriptor] =
    op.input.describe(op.config.docTypeMatcher)
      .map { inputDesc =>
        apply(
          op.parsers.map(_.format.description),
          op.config.filteredBundles.map(ExtensionBundleDescriptor.apply),
          inputDesc,
          op.theme.descriptor,
          op.config.bundleFilter.strict,
          op.config.bundleFilter.acceptRawContent
        )
      }

}
