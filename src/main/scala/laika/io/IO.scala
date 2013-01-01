/*
 * Copyright 2013 the original author or authors.
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

package laika.io
  
import java.io.Closeable

/**
 * A simple helper object that manages a resource, automatically closing it after
 * the IO operation has been performed.
 * 
 * @author Jens Halm
 */
object IO {

  /** Calls the specified function, closes the IO resource and
   *  returns the result of the function call.
   *  
   *  @param resource the IO resource to manage
   *  @param f the function to invoke, with the managed resource getting passed into it
   *  @return the result of the function call
   */
  def apply [R <: Closeable, T] (resource: R)(f: R => T) = try f(resource) finally resource.close
  
}