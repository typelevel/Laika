/*
 * Copyright 2013-2018 the original author or authors.
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

/** Temporary extension methods to back-port parts of the Collection API for 2.13
  * to the Laika release for Scala 2.12.
  *
  * This will remain in place as long as Laika supports Scala 2.12.
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
    def mapValuesStrict[W](f: V ⇒ W): Map[K, W] = {
      map.map {
        case (k,v) => (k,f(v))
      }
    }

  }

}
