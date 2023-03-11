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

import cats.data.NonEmptySet

/** Common groups of characters as input for parser definitions.
  *
  * These groups are all in the ASCII range. For dealing with
  * the entire unicode range you can use the generic `anyWhile`
  * parser that accepts a `Char => Boolean` predicate.
  *
  * @author Jens Halm
  */
object CharGroup {

  private def setForRange(range: Seq[Char]): NonEmptySet[Char] =
    NonEmptySet.of(range.head, range.tail: _*)

  /** All decimal digits (0 to 9).
    */
  val digit: NonEmptySet[Char] = setForRange('0' to '9')

  /** All hexadecimal digits (0 to 9, a to f, A to F).
    */
  val hexDigit: NonEmptySet[Char] = digit ++ setForRange('a' to 'f') ++ setForRange('A' to 'F')

  /** All octal digits (0 to 7).
    */
  val octalDigit: NonEmptySet[Char] = digit ++ setForRange('0' to '7')

  /** All lower-case letters in the ASCII range (a to z).
    */
  val lowerAlpha: NonEmptySet[Char] = setForRange('a' to 'z')

  /** All upper-case letters in the ASCII range (A to Z).
    */
  val upperAlpha: NonEmptySet[Char] = setForRange('A' to 'Z')

  /** All letters in the ASCII range (a to z, A to Z).
    */
  val alpha: NonEmptySet[Char] = lowerAlpha ++ upperAlpha

  /** All letters and digits in the ASCII range (a to z, A to Z, 0 to 9).
    */
  val alphaNum: NonEmptySet[Char] = alpha ++ digit

}
