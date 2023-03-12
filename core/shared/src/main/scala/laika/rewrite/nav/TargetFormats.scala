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

import cats.data.NonEmptySet
import laika.config.{ ConfigDecoder, DefaultKey, LaikaKeys }

import scala.collection.immutable.TreeSet

/** Describes the supported target (output) formats of a resource.
  *
  * @author Jens Halm
  */
sealed trait TargetFormats {
  def contains(format: String): Boolean
}

object TargetFormats {

  /** Describes a target that is intended to be rendered for all output formats.
    */
  case object All extends TargetFormats {
    def contains(format: String): Boolean = true
  }

  /** Describes a target that should not be rendered in any output format.
    * Such a resource may still be used with a directive like `@:include` in which case the including
    * document's configuration will determine the supported output formats.
    */
  case object None extends TargetFormats {
    def contains(format: String): Boolean = false
  }

  /** Describes a target that is only intended to be rendered for the specified set of output formats.
    */
  case class Selected(formats: NonEmptySet[String]) extends TargetFormats {
    def contains(format: String): Boolean = formats.contains(format)
  }

  object Selected {

    def apply(format: String, formats: String*): Selected = apply(
      NonEmptySet.of(format, formats: _*)
    )

  }

  implicit val decoder: ConfigDecoder[TargetFormats] = ConfigDecoder.seq[String].map { formats =>
    NonEmptySet.fromSet(TreeSet(formats: _*)).fold[TargetFormats](TargetFormats.None)(
      TargetFormats.Selected(_)
    )
  }

  implicit val defaultKey: DefaultKey[TargetFormats] = DefaultKey(LaikaKeys.targetFormats)

}
