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

package laika.parse.text

import cats.implicits._
import cats.data.NonEmptySet

/**
  * @author Jens Halm
  */
object CharGroup {
  
  private def setForRange(range: Seq[Char]): NonEmptySet[Char] = NonEmptySet.of(range.head, range.tail:_*)
  
  val digit: NonEmptySet[Char] = setForRange('0' to '9')

  val hexDigit: NonEmptySet[Char] = digit ++ setForRange('a' to 'f') ++ setForRange('A' to 'F')
  
  val octalDigit: NonEmptySet[Char] = digit ++ setForRange('0' to '7')
  
  val lowerAlpha: NonEmptySet[Char] = setForRange('a' to 'z')
  
  val upperAlpha: NonEmptySet[Char] = setForRange('A' to 'Z')
  
  val alpha: NonEmptySet[Char] = lowerAlpha ++ upperAlpha
  
  val alphaNum: NonEmptySet[Char] = alpha ++ digit
  
}
