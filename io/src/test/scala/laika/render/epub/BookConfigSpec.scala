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

import laika.ast.DocumentMetadata
import laika.ast.Path.Root
import laika.config.Config.ConfigResult
import laika.config.{Config, ConfigBuilder, ConfigDecoder, ConfigParser, Key}
import laika.format.EPUB.BookConfig
import laika.render.fo.TestTheme
import laika.time.PlatformDateTime
import munit.FunSuite

/**
  * @author Jens Halm
  */
class BookConfigSpec extends FunSuite {

  private val testKey = Key("test")
  
  def decode[T: ConfigDecoder] (input: String): ConfigResult[T] =
    ConfigParser.parse(input).resolve().flatMap(_.get[T](Key.root))

  def decode[T: ConfigDecoder] (config: Config): ConfigResult[T] = config.get[T](testKey)
  

  test("decode defaults with an empty config") {
    assertEquals(BookConfig.decodeWithDefaults(Config.empty), Right(BookConfig()))
  }
  
  test("decode an instance with fallbacks") {
    val input =
      """{
        |laika {
        |  metadata {
        |    description = "Undescribable"
        |    identifier = XX-33-FF-01
        |    author = "Helen North"
        |    language = en
        |    date = "2002-10-10T12:00:00"
        |  }
        |  fonts = [
        |    { family = Font-C, weight = normal, style = italic, webCSS = "http://fonts.com/font-c.css" }
        |  ]
        |  navigationDepth = 3
        |  coverImage = cover.jpg
        |  epub {
        |    metadata {
        |      title = "Hell is around the corner"
        |      identifier = XX-33-FF-02
        |      author = "Maria South"
        |    }
        |    fonts = [
        |      { family = Font-A, weight = normal, style = normal, embedFile = /path/to/font-a.tff }
        |      { family = Font-B, weight = bold, style = normal, embedResource = /path/to/font-b.tff }
        |    ]
        |    navigationDepth = 4
        |  }
        |}}
      """.stripMargin
    val actual = ConfigParser.parse(input).resolve().flatMap(BookConfig.decodeWithDefaults)  
    val expected = BookConfig(
      DocumentMetadata(
        Some("Hell is around the corner"),
        Some("Undescribable"),
        Some("XX-33-FF-02"),
        Seq("Maria South", "Helen North"),
        Some("en"),
        Some(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get)
      ),
      Some(4),
      TestTheme.fonts,
      Some(Root / "cover.jpg")
    )
    assertEquals(actual, Right(expected))
  }

  test("round-trip encode and decode") {
    val input = BookConfig(DocumentMetadata(Some("XX-33-FF-01")), Some(3), TestTheme.fonts, Some(Root / "cover.jpg"))
    val encoded = ConfigBuilder.empty.withValue(testKey, input).build
    val actual = decode[BookConfig](encoded)
    val expected = BookConfig(
      DocumentMetadata(
        Some("XX-33-FF-01")
      ),
      Some(3),
      TestTheme.fonts,
      Some(Root / "cover.jpg")
    )
    assertEquals(actual, Right(expected))
  }
    
}
