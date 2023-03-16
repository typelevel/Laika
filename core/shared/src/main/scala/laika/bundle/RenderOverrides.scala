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

package laika.bundle

import laika.ast._

/** Collects custom render functions that adjust the rendered output of one or more AST nodes.
  *
  * @author Jens Halm
  */
trait RenderOverrides {

  /** The type of the Formatter API a custom render function for these overrides uses.
    */
  type Formatter

  /** Specifies a custom render function that overrides one or more of the default
    * renderers for the output format this instance uses.
    *
    * This method expects a function that returns a partial function as the result.
    * The outer function allows to capture the writer instance to write to and will only be invoked once.
    * The partial function will then be invoked for each element it is defined at.
    */
  def value: PartialFunction[(Formatter, Element), String]

}
