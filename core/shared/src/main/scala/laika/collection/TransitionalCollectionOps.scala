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

package laika.collection

import scala.collection.{AbstractIterator, Iterator}

/** Temporary extension methods for cross-building to Scala 2.12 and 2.13.
  *
  * @author Jens Halm
  */
object TransitionalCollectionOps {

  implicit class TransitionalMapOps[K,V] (val map: Map[K,V]) extends AnyVal {

    /** Temporary replacement for the `mapValues` method of the Scala SDK
      * which is deprecated in Scala 2.13 as it had returned a lazy Map
      * instance in contrast to most other methods in strict Maps.
      *
      * This is a temporary, strict implementation of `mapValues` until
      * the SDK offers such a method.
      */
    def mapValuesStrict[W](f: V => W): Map[K, W] = {
      map.map {
        case (k,v) => (k,f(v))
      }
    }

  }

  /** Temporary replacement for the deprecated Tuple3.zipped method,
    * to be replaced by xs.lazyZip(ys).lazyZip(zs) once support for 2.12 is dropped.
    */
  case class Zip3Iterator[A,B,C](as: Iterable[A], bs: Iterable[B], cs: Iterable[C]) extends Iterator[(A,B,C)] {
    private val aIter = as.iterator
    private val bIter = bs.iterator
    private val cIter = cs.iterator
    def hasNext = aIter.hasNext && bIter.hasNext && cIter.hasNext
    def next() = (aIter.next(), bIter.next(), cIter.next())
  }

  /** Simple utility to avoid having either a dependency to scala-compat or a warning with 2.13.
    * There are very few places within Laika where we need to deal with a Java collection.
    */
  case class JIteratorWrapper[A](underlying: java.util.Iterator[A]) extends AbstractIterator[A] with Iterator[A] {
    def hasNext: Boolean = underlying.hasNext
    def next(): A = underlying.next
  }

}
