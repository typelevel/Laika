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

import laika.ast.DocumentMetadata
import laika.ast.Path.Root
import laika.config.Config.ConfigResult
import laika.rewrite.nav.BookConfig
import laika.time.PlatformDateFormat
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class ConfigCodecSpec extends AnyWordSpec with Matchers {

  def decode[T: ConfigDecoder] (input: String): ConfigResult[T] =
    ConfigParser.parse(input).resolve().flatMap(_.get[T](Key.root))

  def decode[T: ConfigDecoder] (config: Config): ConfigResult[T] = config.get[T]("test")

  "The codec for DocumentMetadata" should {

    "decode an instance with all fields populated" in {
      val input =
        """{
          |  identifier = XX-33-FF-01
          |  authors = [ "Helen North", "Maria South" ]
          |  language = en
          |  date = "2002-10-10T12:00:00"
          |}
        """.stripMargin
      decode[DocumentMetadata](input) shouldBe Right(DocumentMetadata(
        Some("XX-33-FF-01"),
        Seq("Helen North", "Maria South"),
        Some("en"),
        Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
      ))
    }

    "decode an instance with a single author" in {
      val input =
        """{
          |  identifier = XX-33-FF-01
          |  author = "Dorothea West"
          |  language = en
          |  date = "2002-10-10T12:00:00"
          |}
        """.stripMargin
      decode[DocumentMetadata](input) shouldBe Right(DocumentMetadata(
        Some("XX-33-FF-01"),
        Seq("Dorothea West"),
        Some("en"),
        Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
      ))
    }

    "round-trip encode and decode" in {
      val input = DocumentMetadata(
        Some("XX-33-FF-01"),
        Seq("Helen North", "Maria South"),
        Some("en"),
        Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
      )
      val encoded = ConfigBuilder.empty.withValue("test", input).build
      decode[DocumentMetadata](encoded) shouldBe Right(DocumentMetadata(
        Some("XX-33-FF-01"),
        Seq("Helen North", "Maria South"),
        Some("en"),
        Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
      ))
    }

    "fail with an invalid date" in {
      val input =
        """{
          |  identifier = XX-33-FF-01
          |  author = "Dorothea West"
          |  language = en
          |  date = "2000-XX-01T00:00:00Z"
          |}
        """.stripMargin
      decode[DocumentMetadata](input) shouldBe Left(
        DecodingError("Invalid date format: Text '2000-XX-01T00:00:00Z' could not be parsed at index 5")
      )
    }

  }

  "The codec for BookConfig" should {

    "decode an instance with all fields populated" in {
      val input =
        """{
          |  metadata {
          |    identifier = XX-33-FF-01
          |    authors = [ "Helen North", "Maria South" ]
          |    language = en
          |    date = "2002-10-10T12:00:00"
          |  }
          |  navigationDepth = 3
          |  coverImage = cover.jpg
          |}
        """.stripMargin
      decode[BookConfig](input) shouldBe Right(BookConfig(
        DocumentMetadata(
          Some("XX-33-FF-01"),
          Seq("Helen North", "Maria South"),
          Some("en"),
          Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
        ),
        Some(3),
        Some(Root / "cover.jpg")
      ))
    }

    "decode an instance with some fields populated" in {
      val input =
        """{
          |  metadata {
          |    identifier = XX-33-FF-01
          |  }
          |  navigationDepth = 3
          |}
        """.stripMargin
      decode[BookConfig](input) shouldBe Right(BookConfig(
        DocumentMetadata(
          Some("XX-33-FF-01")
        ),
        Some(3)
      ))
    }

    "round-trip encode and decode" in {
      val input = BookConfig(DocumentMetadata(Some("XX-33-FF-01")), Some(3), Some(Root / "cover.jpg"))
      val encoded = ConfigBuilder.empty.withValue("test", input).build
      decode[BookConfig](encoded) shouldBe Right(BookConfig(
        DocumentMetadata(
          Some("XX-33-FF-01")
        ),
        Some(3),
        Some(Root / "cover.jpg")
      ))
    }

  }

}
