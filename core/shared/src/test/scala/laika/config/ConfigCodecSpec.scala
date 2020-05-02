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

import laika.ast.{DocumentMetadata, ExternalTarget, InternalTarget}
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.config.Config.ConfigResult
import laika.rewrite.link.{ApiLinks, LinkConfig, TargetDefinition}
import laika.rewrite.nav.{AutonumberConfig, BookConfig}
import laika.time.PlatformDateFormat
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class ConfigCodecSpec extends AnyWordSpec with Matchers {

  private val testKey = Key("test")

  def decode[T: ConfigDecoder: DefaultKey] (input: String): ConfigResult[T] =
    ConfigParser.parse(input).resolve().flatMap(_.get[T])

  def decode[T: ConfigDecoder] (config: Config): ConfigResult[T] = config.get[T](testKey)

  "The codec for DocumentMetadata" should {

    "decode an instance with all fields populated" in {
      val input =
        """{ 
          |laika.metadata {
          |  identifier = XX-33-FF-01
          |  authors = [ "Helen North", "Maria South" ]
          |  language = en
          |  date = "2002-10-10T12:00:00"
          |}}
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
          |laika.metadata {
          |  identifier = XX-33-FF-01
          |  author = "Dorothea West"
          |  language = en
          |  date = "2002-10-10T12:00:00"
          |}}
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
      val encoded = ConfigBuilder.empty.withValue(testKey, input).build
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
          |laika.metadata {
          |  identifier = XX-33-FF-01
          |  author = "Dorothea West"
          |  language = en
          |  date = "2000-XX-01T00:00:00Z"
          |}}
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
          |laika {
          |  metadata {
          |    identifier = XX-33-FF-01
          |    authors = [ "Helen North", "Maria South" ]
          |    language = en
          |    date = "2002-10-10T12:00:00"
          |  }
          |  navigationDepth = 3
          |  coverImage = cover.jpg
          |}}
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
          |laika {
          |  metadata {
          |    identifier = XX-33-FF-01
          |  }
          |  navigationDepth = 3
          |}}
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
      val encoded = ConfigBuilder.empty.withValue(testKey, input).build
      decode[BookConfig](encoded) shouldBe Right(BookConfig(
        DocumentMetadata(
          Some("XX-33-FF-01")
        ),
        Some(3),
        Some(Root / "cover.jpg")
      ))
    }

  }

  "The codec for LinkConfig" should {

    def sort (config: ConfigResult[LinkConfig]): ConfigResult[LinkConfig] = config.map { c =>
      c.copy(targets = c.targets.sortBy(_.id))
    }

    val fullyPopulatedInstance = LinkConfig(
      Seq(
        TargetDefinition("bar", InternalTarget(Root, CurrentTree / "bar")),
        TargetDefinition("ext", ExternalTarget("http://ext.com")),
        TargetDefinition("foo", InternalTarget(Root, CurrentTree / "foo"))
      ),
      Seq(Root / "foo", Root / "bar" / "baz"),
      Seq(
        ApiLinks("https://foo.api/", "foo", "package.html"),
        ApiLinks("https://bar.api/", "foo.bar")
      )
    )

    "decode an instance with all fields populated" in {
      val input =
        """{
          |  laika.links {
          |    targets {
          |      foo = foo
          |      bar = bar
          |      ext = "http://ext.com"
          |    }
          |    excludeFromValidation = [
          |      /foo
          |      /bar/baz
          |    ]
          |    api = [
          |      { baseUri = "https://foo.api/", packagePrefix = foo, packageSummary = package.html },
          |      { baseUri = "https://bar.api/", packagePrefix = foo.bar }
          |    ]
          |  }
          |}
        """.stripMargin
      sort(decode[LinkConfig](input)) shouldBe Right(fullyPopulatedInstance)
    }

    "decode an instance with some fields populated" in {
      val input =
        """{
          |  laika.links {
          |    targets {
          |      foo = foo
          |    }
          |    api = [
          |      { baseUri = "https://bar.api/" }
          |    ]
          |  }
          |}
        """.stripMargin
      sort(decode[LinkConfig](input)) shouldBe Right(LinkConfig(
        Seq(TargetDefinition("foo", InternalTarget(Root, CurrentTree / "foo"))),
        Nil,
        Seq(ApiLinks("https://bar.api/"))
      ))
    }

    "round-trip encode and decode" in {
      val encoded = ConfigBuilder.empty.withValue(testKey, fullyPopulatedInstance).build
      sort(decode[LinkConfig](encoded)) shouldBe Right(fullyPopulatedInstance)
    }

  }

  "The codec for AutonumberConfig" should {

    val fullyPopulatedInstance = AutonumberConfig(
      documents = true,
      sections = true,
      maxDepth = 5
    )

    "decode an instance with all fields populated" in {
      val input =
        """{
          |  laika.autonumbering {
          |    scope = all
          |    depth = 5
          |  }
          |}
        """.stripMargin
      decode[AutonumberConfig](input) shouldBe Right(fullyPopulatedInstance)
    }

    "round-trip encode and decode" in {
      val encoded = ConfigBuilder.empty.withValue(testKey, fullyPopulatedInstance).build
      decode[AutonumberConfig](encoded) shouldBe Right(fullyPopulatedInstance)
    }

  }

}
