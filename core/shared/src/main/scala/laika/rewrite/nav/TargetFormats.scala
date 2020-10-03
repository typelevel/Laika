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
import laika.config.{ConfigDecoder, DefaultKey, LaikaKeys}

import scala.collection.immutable.TreeSet

/**
  * @author Jens Halm
  */
sealed trait TargetFormats {
  def includes (format: String): Boolean
}

object TargetFormats {

  case object All extends TargetFormats {
    def includes (format: String): Boolean = true
  }
  case object None extends TargetFormats {
    def includes (format: String): Boolean = false
  }
  case class Selected (formats: NonEmptySet[String]) extends TargetFormats {
    def includes (format: String): Boolean = formats.contains(format)
  }
  
  implicit val decoder: ConfigDecoder[TargetFormats] = ConfigDecoder.seq[String].map { formats =>
    NonEmptySet.fromSet(TreeSet(formats:_*)).fold[TargetFormats](TargetFormats.None)(TargetFormats.Selected)
  }

  implicit val defaultKey: DefaultKey[TargetFormats] = DefaultKey(LaikaKeys.targetFormats)
  
}