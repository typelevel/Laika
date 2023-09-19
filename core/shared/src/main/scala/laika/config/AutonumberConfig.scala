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

package laika.config

import laika.api.config
import laika.api.config.*

/** Configuration for autonumbering of documents and sections.
  */
sealed abstract class AutonumberConfig {

  /** Applies autonumbering to directories and documents and their titles. */
  def documents: Boolean

  /** Applies autonumber to section header within documents. */
  def sections: Boolean

  /** Specifies how many levels deep the autonumbering should be applied.
    * Any documents or section headers beyond this limit will be ignored.
    *
    * The default is unlimited depth.
    */
  def maxDepth: Int

  /** Specifies how many levels deep the autonumbering should be applied.
    * Any documents or section headers beyond this limit will be ignored.
    */
  def withMaxDepth(value: Int): AutonumberConfig
}

object AutonumberConfig {

  private final case class Impl(documents: Boolean, sections: Boolean, maxDepth: Int)
      extends AutonumberConfig {
    override def productPrefix: String             = "AutonumberConfig"
    def withMaxDepth(value: Int): AutonumberConfig = copy(maxDepth = value)
  }

  private val scopeKey = Key("scope")
  private val depthKey = Key("depth")

  implicit val defaultKey: DefaultKey[AutonumberConfig] = DefaultKey(LaikaKeys.autonumbering)

  implicit val decoder: ConfigDecoder[AutonumberConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      scope <- config.get[Scope](scopeKey, Scope.None)
      depth <- config.get[Int](depthKey, Int.MaxValue)
    } yield {
      val (documents, sections) = scope match {
        case Scope.Documents => (true, false)
        case Scope.Sections  => (false, true)
        case Scope.All       => (true, true)
        case Scope.None      => (false, false)
      }
      Impl(documents, sections, depth)
    }
  }

  implicit val encoder: ConfigEncoder[AutonumberConfig] = config.ConfigEncoder[AutonumberConfig] {
    config =>
      val scopeString = (config.documents, config.sections) match {
        case (true, false)  => "documents"
        case (false, true)  => "sections"
        case (true, true)   => "all"
        case (false, false) => "none"
      }
      ConfigEncoder.ObjectBuilder.empty
        .withValue(scopeKey, scopeString)
        .withValue(depthKey, config.maxDepth)
        .build
  }

  /** Disables section numbering for the specified config instance.
    * Retains the existing value for auto-numbering of documents.
    */
  private[laika] def withoutSectionNumbering(
      config: Config
  )(builder: ConfigBuilder): ConfigBuilder = {
    val key = LaikaKeys.autonumbering.child(scopeKey)
    config.get[Scope](key).toOption.fold(builder) {
      case Scope.Documents => builder
      case Scope.Sections  => builder.withValue(key, "none")
      case Scope.All       => builder.withValue(key, "documents")
      case Scope.None      => builder
    }
  }

  /** Section numbering within documents switched on, but document and directory numbering switched off.
    */
  def sectionsEnabled: AutonumberConfig =
    Impl(documents = false, sections = true, maxDepth = Int.MaxValue)

  /** Document and directory numbering switched on, but section numbering within documents switched off.
    */
  def documentsEnabled: AutonumberConfig =
    Impl(documents = true, sections = false, maxDepth = Int.MaxValue)

  /** Section and document numbering both switched on.
    */
  def allEnabled: AutonumberConfig =
    Impl(documents = true, sections = true, maxDepth = Int.MaxValue)

  /** Section and document numbering both switched off.
    */
  def disabled: AutonumberConfig =
    Impl(documents = false, sections = false, maxDepth = 0)

}

private[config] sealed trait Scope

private[config] object Scope {

  case object Documents extends Scope
  case object Sections  extends Scope
  case object All       extends Scope
  case object None      extends Scope

  implicit val decoder: ConfigDecoder[Scope] = ConfigDecoder.string.flatMap {
    case "documents" => Right(Documents)
    case "sections"  => Right(Sections)
    case "all"       => Right(All)
    case "none"      => Right(None)
    case other       =>
      Left(ConfigError.ValidationFailed(s"Invalid value for autonumbering.scope: $other"))
  }

}
