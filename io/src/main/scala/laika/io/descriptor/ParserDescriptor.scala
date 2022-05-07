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
import cats.effect.{Async, Sync}
import cats.implicits._
import fs2.io.file.Files
import laika.io.api.TreeParser
import laika.io.model.{FileFilter, FilePath}

/** Provides a description of a parsing operation, including the parsers
  * and extension bundles used, as well as the input sources.
  * This functionality is mostly intended for tooling support.
  * 
  * @author Jens Halm
  */
case class ParserDescriptor (parsers: NonEmptyList[String], 
                             bundles: Seq[ExtensionBundleDescriptor],
                             inputs: TreeInputDescriptor,
                             strict: Boolean,
                             acceptRawContent: Boolean) {

  def formatted: String = {
    s"""Parser(s):
       |  ${parsers.toList.mkString("\n  ")}
       |Extension Bundles:
       |  ${bundles.mkString("\n  ")}
       |Settings:
       |  Strict Mode: $strict
       |  Accept Raw Content: $acceptRawContent
       |Sources:
       |  ${inputs.formatted}""".stripMargin
  }
  
  
}

object ParserDescriptor {
  
  private val fileFilter = new FileFilter {
    def filter[F[_] : Async] (file: FilePath) = Files[F].exists(file.toFS2Path).map(!_)
  }
  
  def create[F[_]: Sync] (op: TreeParser.Op[F]): F[ParserDescriptor] = 
    TreeInputDescriptor
      .create(op.input.withFileFilter(fileFilter).build(op.config.docTypeMatcher))
      .map { inputDesc =>
        apply(
          op.parsers.map(_.format.description),
          op.config.bundles.filter(op.config.bundleFilter).map(ExtensionBundleDescriptor.apply),
          inputDesc,
          op.config.bundleFilter.strict,
          op.config.bundleFilter.acceptRawContent
        )
      }
  
}
