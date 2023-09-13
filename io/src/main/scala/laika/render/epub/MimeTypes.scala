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

package laika.render.epub

/** Lists the supported file types that get inserted into the EPUB container
  * in addition to the dynamically rendered HTML content.
  *
  * @author Jens Halm
  */
private[epub] object MimeTypes {

  /** Maps files suffixes to mime types.
    */
  val supportedTypes: Map[String, String] = Map(
    "jpg"   -> "image/jpeg",
    "jpeg"  -> "image/jpeg",
    "gif"   -> "image/gif",
    "png"   -> "image/png",
    "svg"   -> "image/svg+xml",
    "mp3"   -> "audio/mpeg",
    "mp4"   -> "audio/mp4",
    "html"  -> "application/xhtml+xml",
    "xhtml" -> "application/xhtml+xml",
    "js"    -> "application/javascript",
    "css"   -> "text/css",
    "woff2" -> "font/woff2",
    "woff"  -> "application/font-woff",
    "ttf"   -> "application/font-sfnt",
    "otf"   -> "application/font-sfnt"
  )

}
