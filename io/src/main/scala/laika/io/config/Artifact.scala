/*
 * Copyright 2012-2024 the original author or authors.
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

package laika.io.config

import laika.ast.Path

/** Represents an artifact with optional classifiers such as those used by binary renderers.
  * The format is `<basePath><classifiers>.<suffix>` where `classifiers` is the empty string
  * if the `classifiers` property is empty. Otherwise `classifiers` will be concatenated
  * with `-` as separator and prefix.
  */
sealed abstract class Artifact private[config] {

  /** The basePath for the artifact within the virtual tree, including the file name without suffix.
    */
  def basePath: Path

  /** The classifiers to insert between basePath and suffix, with `-` as separator and prefix.
    */
  def classifiers: Seq[String]

  /** The file suffix of the artifact.
    */
  def suffix: String

  /** Returns a new Artifact with the specified classifiers applied.
    */
  def withClassifiers(cls: Seq[String]): Artifact

  /** The full virtual Path for this Artifact, including all applied classifiers.
    */
  def fullPath: Path = {
    val classifierString = if (classifiers.isEmpty) "" else classifiers.mkString("-", "-", "")
    basePath.parent / (basePath.name + classifierString + "." + suffix)
  }

}

object Artifact {

  private final case class Impl(
      basePath: Path,
      suffix: String,
      classifiers: Seq[String]
  ) extends Artifact {
    override def productPrefix = "Artifact"

    def withClassifiers(cls: Seq[String]): Artifact = Impl(basePath, suffix, cls)
  }

  def apply(
      basePath: Path,
      suffix: String
  ): Artifact = Impl(
    basePath,
    suffix,
    Nil
  )

}
