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
  * and user-provided ids to a slug that is compatible with HTML/XML ids, URLs and file names.
  *
  * @author Jens Halm
  */
object SlugBuilder {

  /** Default slug builder that turns the specified string into a slug suitable
    * as HTML or XML id, as a path segment in a URL and as a file name.
    *
    * Iterates over the input text one Unicode code point at a time.
    * All code points that are letters or numbers or underscore are considered valid characters.
    * They are mapped to lower case, and included in the output.
    * All other code points are considered invalid characters,
    * and any sequence of such code points will be replaced by a single dash character (`-`),
    * unless the sequence starts or ends the string in which case it is simply dropped.
    */
  def default(text: String): String = {

    val slug = text.foldLeft(("", true)) { case ((res, inWord), char) =>
      if (Character.isLetter(char) || Character.isDigit(char)) (res + char.toLower, true)
      else if (char == '_') (res + char, true)
      else if (inWord) (res + "-", false)
      else (res, false)
    }
      ._1
      .stripPrefix("-")
      .stripSuffix("-")

    if (slug.headOption.exists(Character.isLetter)) slug
    else "_" + slug

  }

}
