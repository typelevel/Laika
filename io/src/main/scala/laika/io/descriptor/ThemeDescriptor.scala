/*
 * Copyright 2012-2022 the original author or authors.
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

package laika.io.descriptor

/** Provides a short description of a theme and its extensions for tooling or logging.
  *
  * @author Jens Halm
  */
case class ThemeDescriptor(description: String, extensions: Seq[String] = Nil) {

  def formatted: String = {
    val extensionDesc =
      if (extensions.isEmpty) ""
      else
        " - with extensions:\n    " + extensions.mkString("\n    ")
    description + extensionDesc
  }

  def extendWith(extension: ThemeDescriptor): ThemeDescriptor =
    copy(extensions = extensions ++ (extension.description +: extension.extensions))

}
