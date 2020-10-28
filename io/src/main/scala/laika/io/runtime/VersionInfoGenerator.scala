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

package laika.io.runtime

import laika.io.runtime.VersionedLinkTargets.VersionedDocument
import laika.rewrite.Versions

private[runtime] object VersionInfoGenerator {
  
  private val template = 
    """{
      |  "versions": [
      |    $VERSIONS
      |  ],
      |  "linkTargets": [
      |    $LINK_TARGETS
      |  ]
      |}""".stripMargin
  
  private def generateVersions (versions: Versions): String = versions.allVersions.map { version =>
    val label = version.label.fold(""){ label => s""", "label": "$label""""}
    s"""    { "displayValue": "${version.displayValue}", "pathSegment": "${version.pathSegment}", "fallbackLink": "${version.fallbackLink}"$label }"""
  }.mkString("\n").trim
  
  private def generateLinkTargets (linkTargets: Seq[VersionedDocument]): String = 
    linkTargets.sortBy(_.path.toString).map { doc =>
      s"""    { "path": "${doc.path.toString}", "versions": ["${doc.versions.sorted.mkString(",")}"] }"""
    }.mkString("\n").trim

  def generate (versions: Versions, linkTargets: Seq[VersionedDocument]): String = template
    .replace("$VERSIONS", generateVersions(versions))
    .replace("$LINK_TARGETS", generateLinkTargets(linkTargets))
  
}
