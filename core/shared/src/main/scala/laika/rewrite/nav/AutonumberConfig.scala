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

package laika.rewrite.nav

import laika.config.Config.ConfigResult
import laika.config.{Config, ConfigDecoder, ConfigValue, DefaultKey, InvalidType, ObjectValue, Traced, ValidationError}

/** Configuration for autonumbering of documents and sections.
 */
case class AutonumberConfig (documents: Boolean, sections: Boolean, maxDepth: Int)

case class ConfigurationException (msg: String) extends RuntimeException(msg)

sealed trait Scope

object Scope {
  case object Documents extends Scope
  case object Sections extends Scope
  case object All extends Scope
  case object None extends Scope
  implicit val decoder: ConfigDecoder[Scope] = ConfigDecoder.string.flatMap {
    case "documents" => Right(Documents)
    case "sections"  => Right(Sections)
    case "all"       => Right(All)
    case "none"      => Right(None)
    case other       => Left(ValidationError(s"Invalid value for autonumbering.scope: $other"))
  }
}

object AutonumberConfig {

  implicit val decoder: ConfigDecoder[AutonumberConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      scope <- config.get[Scope]("scope", Scope.None)
      depth <- config.get[Int]("depth", Int.MaxValue)
    } yield {
      val (documents, sections) = scope match {
        case Scope.Documents => (true,  false)
        case Scope.Sections  => (false, true)
        case Scope.All       => (true,  true)
        case Scope.None      => (false, false)
      }
      AutonumberConfig(documents, sections, depth)
    }
  }
  implicit val defaultKey: DefaultKey[AutonumberConfig] = DefaultKey("autonumbering")

  /** Disables section numbering for the specified config instance.
    * Retains the existing value for auto-numbering of documents.
    */
  def withoutSectionNumbering (config: Config): Config = {
    val key = "autonumbering.scope"
    config.get[Scope](key).toOption.fold(config) {
      case Scope.Documents => config
      case Scope.Sections  => config.withValue(key, "none").build
      case Scope.All       => config.withValue(key, "documents").build
      case Scope.None      => config
    }
  }

  /** Tries to obtain the autonumbering configuration
   *  from the specified configuration instance or returns
   *  the default configuration if not found.
   */
  def fromConfig (config: Config): ConfigResult[AutonumberConfig] = 
    config.getOpt[AutonumberConfig].map(_.getOrElse(defaults))
  
  /** The defaults for autonumbering with section
   *  and document numbering both switched off. 
   */
  def defaults: AutonumberConfig = AutonumberConfig(documents = false, sections = false, maxDepth = 0)
}
  