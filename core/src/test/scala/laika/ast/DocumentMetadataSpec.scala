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

package laika.ast

import java.time.Instant
import java.util.Locale

import laika.config.{ConfigBuilder, ConfigParser}
import org.scalatest.{FlatSpec, Matchers}

class DocumentMetadataSpec extends FlatSpec with Matchers {

  "The config-based builder for document metadata" should "fully populate the model from the provided config" in {
    val configString =
      """
        |metadata: {
        |  identifier: "urn:isbn:9781449325299"
        |  date: "2000-01-01T00:00:00Z"
        |  author: "Mia Miller"
        |  language: "en:UK"
        |}
      """.stripMargin
    val config = ConfigParser.parse(configString).resolve.right.get
    DocumentMetadata.fromConfig(config) shouldBe DocumentMetadata(Some("urn:isbn:9781449325299"), Seq("Mia Miller"),
      Some(Locale.forLanguageTag("en:UK")), Some(Instant.parse("2000-01-01T00:00:00Z")))
  }

  it should "populate multiple authors" in {
    val configString =
      """metadata.authors: ["Mia Miller", "Naomi Nader"]"""
    val config = ConfigParser.parse(configString).resolve.right.get
    DocumentMetadata.fromConfig(config) shouldBe DocumentMetadata(authors = Seq("Mia Miller", "Naomi Nader"))
  }

  it should "provide an empty instance when there is no metadata entry" in {
    val config = ConfigBuilder.empty.withValue("foo", "bar").build
    DocumentMetadata.fromConfig(config) shouldBe DocumentMetadata()
  }

}
