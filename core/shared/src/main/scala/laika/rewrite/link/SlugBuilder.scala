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

package laika.rewrite.link

/** Default implementation for the logic that transforms section titles, document names
 *  and user-provided ids to a slug that is compatible with HTML/XML ids, URLs and file names.
 * 
 * @author Jens Halm
 */
object SlugBuilder {

  def default (text: String): String = {
    text
      .replaceAll("[^a-zA-Z0-9-]+","-")
      .stripPrefix("-")
      .stripSuffix("-")
      .toLowerCase // TODO - retain unicode characters
  }
  
}

