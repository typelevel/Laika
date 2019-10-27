/*
 * Copyright 2012-2019 the original author or authors.
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

/** A defaultKey can be used for commonly used configuration objects like `AutonumberConfig`
  * that are expected to be mapped to a specific key, like `autonumbering` without requiring
  * the user to remember these keys.
  * 
  * {{{
  * val config: Config = ???
  * val res: ConfigResult[AutonumberConfig] = config.get[AutonumberConfig]
  * }}}
  * 
  * In the example above retrieval happens solely based on the type of the result
  * with the associated key being specified by an implicit `DefaultKey`.
  * 
  * @author Jens Halm
  */
trait DefaultKey[T] {
  def value: String
}

object DefaultKey {
  
  /** Creates a new instance for the specified key.
    */
  def apply[T](key: String): DefaultKey[T] = new DefaultKey[T] { val value: String = key }
}
