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

package laika.theme

import laika.ast.DocumentMetadata
import laika.ast.Path.Root
import laika.config.Config.ConfigResult
import laika.config._
import laika.render.fo.TestTheme
import laika.theme.config.BookConfig
import laika.time.PlatformDateFormat
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class ThemeConfigCodecSpec extends AnyWordSpec with Matchers {

  private val testKey = Key("test")

  def decode[T: ConfigDecoder: DefaultKey] (input: String): ConfigResult[T] =
    ConfigParser.parse(input).resolve().flatMap(_.get[T])

  def decode[T: ConfigDecoder] (config: Config): ConfigResult[T] = config.get[T](testKey)
  
  "The codec for BookConfig" should {

    "decode an instance with all fields populated" in {
      val input =
        """{
          |laika {
          |  metadata {
          |    title = "Hell is around the corner"
          |    description = "Undescribable"
          |    identifier = XX-33-FF-01
          |    authors = [ "Helen North", "Maria South" ]
          |    language = en
          |    date = "2002-10-10T12:00:00"
          |  }
          |  fonts = [
          |    { family = Font-A, weight = normal, style = normal, embedFile = /path/to/font-a.tff }
          |    { family = Font-B, weight = bold, style = normal, embedResource = /path/to/font-b.tff }
          |    { family = Font-C, weight = normal, style = italic, webCSS = "http://fonts.com/font-c.css" }
          |  ]
          |  navigationDepth = 3
          |  coverImage = cover.jpg
          |}}
        """.stripMargin
      decode[BookConfig](input) shouldBe Right(BookConfig(
        DocumentMetadata(
          Some("Hell is around the corner"),
          Some("Undescribable"),
          Some("XX-33-FF-01"),
          Seq("Helen North", "Maria South"),
          Some("en"),
          Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
        ),
        Some(3),
        TestTheme.fonts,
        Some(Root / "cover.jpg")
      ))
    }

    "decode an instance with some fields populated" in {
      val input =
        """{
          |laika {
          |  metadata {
          |    identifier = XX-33-FF-01
          |  }
          |  navigationDepth = 3
          |}}
        """.stripMargin
      decode[BookConfig](input) shouldBe Right(BookConfig(
        DocumentMetadata(
          identifier = Some("XX-33-FF-01")
        ),
        Some(3)
      ))
    }

    "round-trip encode and decode" in {
      val input = BookConfig(DocumentMetadata(Some("XX-33-FF-01")), Some(3), TestTheme.fonts, Some(Root / "cover.jpg"))
      val encoded = ConfigBuilder.empty.withValue(testKey, input).build
      decode[BookConfig](encoded) shouldBe Right(BookConfig(
        DocumentMetadata(
          Some("XX-33-FF-01")
        ),
        Some(3),
        TestTheme.fonts,
        Some(Root / "cover.jpg")
      ))
    }

  }

}
