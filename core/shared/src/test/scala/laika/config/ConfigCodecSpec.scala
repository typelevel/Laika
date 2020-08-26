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

import cats.data.NonEmptyChain
import laika.ast.{DocumentMetadata, ExternalTarget, InternalTarget}
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.config.Config.ConfigResult
import laika.rewrite.link.{ApiLinks, InternalLinkMapping, LinkConfig, SourceLinks, TargetDefinition}
import laika.rewrite.nav.{AutonumberConfig, ChoiceConfig, SelectionConfig, Selections}
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
          |  title = "Monkey Gone To Heaven"
          |  description = "It's undescribable"
          |  identifier = XX-33-FF-01
          |  authors = [ "Helen North", "Maria South" ]
          |  language = en
          |  date = "2002-10-10T12:00:00"
          |}}
        """.stripMargin
      decode[DocumentMetadata](input) shouldBe Right(DocumentMetadata(
        Some("Monkey Gone To Heaven"),
        Some("It's undescribable"),
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
        None, None,
        Some("XX-33-FF-01"),
        Seq("Dorothea West"),
        Some("en"),
        Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
      ))
    }

    "round-trip encode and decode" in {
      val input = DocumentMetadata(
        Some("Monkey Gone To Heaven"),
        Some("Rhubarb, Rhubarb, Rhubarb"),
        Some("XX-33-FF-01"),
        Seq("Helen North", "Maria South"),
        Some("en"),
        Some(PlatformDateFormat.parse("2002-10-10T12:00:00").toOption.get)
      )
      val encoded = ConfigBuilder.empty.withValue(testKey, input).build
      decode[DocumentMetadata](encoded) shouldBe Right(input)
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
      val res = decode[DocumentMetadata](input)
      res.isLeft shouldBe true
      res.left.toOption.get.asInstanceOf[DecodingError].message should startWith("Invalid date format")
    }

  }

  "The codec for LinkConfig" should {

    def sort (config: ConfigResult[LinkConfig]): ConfigResult[LinkConfig] = config.map { c =>
      c.copy(targets = c.targets.sortBy(_.id))
    }

    val fullyPopulatedInstance = LinkConfig(
      Seq(
        TargetDefinition("bar", InternalTarget(CurrentTree / "bar")),
        TargetDefinition("ext", ExternalTarget("http://ext.com")),
        TargetDefinition("foo", InternalTarget(CurrentTree / "foo"))
      ),
      Seq(Root / "foo", Root / "bar" / "baz"),
      Seq(
        InternalLinkMapping(Root / "api", "http://our-project/api"),
        InternalLinkMapping(Root / "blog", "http://our-project/blog")
      ),
      Seq(
        ApiLinks("https://foo.api/", "foo", "package.html"),
        ApiLinks("https://bar.api/", "foo.bar")
      ),
      Seq(
        SourceLinks("https://foo.source/", "scala", "foo"),
        SourceLinks("https://bar.source/", "java", "foo.bar")
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
          |    internalLinkMappings = [
          |      { internalPath = "/api", externalBaseUrl = "http://our-project/api" },
          |      { internalPath = "/blog", externalBaseUrl = "http://our-project/blog" }
          |    ]
          |    api = [
          |      { baseUri = "https://foo.api/", packagePrefix = foo, packageSummary = package.html },
          |      { baseUri = "https://bar.api/", packagePrefix = foo.bar }
          |    ]
          |    source = [
          |      { baseUri = "https://foo.source/", suffix = scala, packagePrefix = foo },
          |      { baseUri = "https://bar.source/", suffix = java, packagePrefix = foo.bar }
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
        targets = Seq(TargetDefinition("foo", InternalTarget(CurrentTree / "foo"))),
        apiLinks = Seq(ApiLinks("https://bar.api/"))
      ))
    }

    "round-trip encode and decode" in {
      val encoded = ConfigBuilder.empty.withValue(testKey, fullyPopulatedInstance).build
      sort(decode[LinkConfig](encoded)) shouldBe Right(fullyPopulatedInstance)
    }

  }
  
  "The codec for ChoiceGroupsConfig" should {

    val sample = Selections(Seq(
      SelectionConfig("foo", NonEmptyChain(
        ChoiceConfig("foo-a", "foo-label-a", selected = true),
        ChoiceConfig("foo-b", "foo-label-b")
      ), separateEbooks = true),
      SelectionConfig("bar", NonEmptyChain(
        ChoiceConfig("bar-a", "bar-label-a"),
        ChoiceConfig("bar-b", "bar-label-b")
      ))
    ))
    
    "decode an instance with all fields populated" in {
      val input =
        """{
          |  laika.selections = [
          |    { 
          |      name = "foo"
          |      choices = [
          |        { name = "foo-a", label = "foo-label-a", selected = true }
          |        { name = "foo-b", label = "foo-label-b" }
          |      ]
          |      separateEbooks = true
          |    }
          |    { 
          |      name = "bar"
          |      choices = [
          |        { name = "bar-a", label = "bar-label-a" }
          |        { name = "bar-b", label = "bar-label-b" }
          |      ]
          |    }
          |  ]
          |}
        """.stripMargin
      decode[Selections](input) shouldBe Right(sample)
    }

    "round-trip encode and decode" in {
      val encoded = ConfigBuilder.empty.withValue(sample).build
      encoded.get[Selections] shouldBe Right(sample)
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
