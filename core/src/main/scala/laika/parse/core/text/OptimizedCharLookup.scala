/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.core.text

/**
  * @author Jens Halm
  */
object OptimizedCharLookup {


  /**  Returns an optimized, Array-based lookup function
    *  for the specified characters.
    */
  def forChars (chars: Seq[Char]): Array[Int] = {
    val max = chars.max
    val lookup = new Array[Int](max + 1)

    for (c <- chars) lookup(c) = 1

    lookup
  }

  /** Returns an optimized, Array-based lookup function
    *  for the specified ranges of characters.
    */
  def forRanges (ranges: Seq[Traversable[Char]]): Array[Int] = {
    val max = ranges map (_.max) max
    val lookup = new Array[Int](max + 1)

    for (r <- ranges; c <- r) lookup(c) = 1

    lookup
  }


}
