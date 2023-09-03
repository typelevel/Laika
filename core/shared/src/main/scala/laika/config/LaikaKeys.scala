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

package laika.config

/** Constants for configuration keys for the library's core configuration entries.
  *
  * @author Jens Halm
  */
object LaikaKeys {

  val root = Key("laika")

  val title: Key = root.child("title")

  val template: Key = root.child("template")

  def template(format: String): Key = root.child(Key(format, "template"))

  val metadata: Key = root.child("metadata")

  val links: Key = root.child("links")

  val icons: Key = root.child("icons")

  val selections: Key = root.child("selections")

  val autonumbering: Key = root.child("autonumbering")

  val navigationOrder: Key = root.child("navigationOrder")

  val excludeFromNavigation: Key = root.child("excludeFromNavigation")

  val targetFormats: Key = root.child("targetFormats")

  @deprecated("0.19.3", "use laika.links.validation.excluded = <list-of-paths>")
  val validateLinks: Key = root.child("validateLinks")

  val firstHeaderAsTitle: Key = root.child("firstHeaderAsTitle")

  val artifactBaseName: Key = root.child("artifactBaseName")

  val siteBaseURL: Key = root.child("siteBaseURL")

  val coverImage: Key = root.child("coverImage")

  val coverImages: Key = root.child("coverImages")

  val versioned: Key = root.child("versioned")

  val versions: Key = root.child("versions")

  object titleDocuments {
    val inputName: Key  = root.child(Key("titleDocuments", "inputName"))
    val outputName: Key = root.child(Key("titleDocuments", "outputName"))
  }

  object site {
    val apiPath: Key      = root.child(Key("site", "downloadPath"))
    val downloadPath: Key = root.child(Key("site", "apiPath"))
    val metadata: Key     = root.child(Key("site", "metadata"))

    @deprecated("0.19.4", "use Helium configuration for CSS includes")
    val css: Key = root.child(Key("site", "css"))

    @deprecated("0.19.4", "use Helium configuration for JS includes")
    val js: Key = root.child(Key("site", "js"))

  }

  object epub {

    @deprecated("0.19.4", "use Helium configuration for CSS includes")
    val css: Key = root.child(Key("epub", "css"))

    @deprecated("0.19.4", "use Helium configuration for JS includes")
    val js: Key = root.child(Key("epub", "js"))

  }

  object preview {
    private val base = root.child("preview")
    val enabled: Key = base.child("enabled")
  }

}
