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

package laika

/**
  * @author Jens Halm
  */
package object ast {

  /** Specifies how a particular element, document or document
    * tree should be rewritten.
    *
    * If the rule is not defined for a specific element or the rule returns 
    * a `Retain` action as a result the old element remains in the tree unchanged. 
    * 
    * If it returns `Remove` then the node gets removed from the ast,
    * if it returns `Replace` with a new element it will replace the old one. 
    */
  type RewriteRule[T] = PartialFunction[T, RewriteAction[T]]

  /** Specifies a custom render function that may override the rendered
    *  output for one or more node types. For elements this function
    *  is not defined the renderer will fall back to the default
    *  renderer (or the next custom renderer, in case there are multiple).
    */
  type RenderFunction = PartialFunction[Element,Unit]

  /** A wrapper for two result values.
    */
  case class ~[+A,+B] (_1:A, _2:B)

}
