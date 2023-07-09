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

import cats.data.{ Chain, NonEmptyChain }
import laika.ast.{ DocumentMetadata, ExternalTarget, IconGlyph, IconStyle, InternalTarget }
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.rewrite.{ Version, VersionScannerConfig, Versions }
import laika.rewrite.link.{
  ApiLinks,
  IconRegistry,
  LinkConfig,
  LinkValidation,
  SourceLinks,
  TargetDefinition
}
import laika.rewrite.nav.{ AutonumberConfig, ChoiceConfig, SelectionConfig, Selections }
import laika.time.PlatformDateTime
import munit.FunSuite

import java.net.URI

/** @author Jens Halm
  */
class ConfigCodecSpec extends FunSuite {

  private val testKey = Key("test")

  def failDecode[T: ConfigDecoder: DefaultKey](input: String, messageStart: String): Unit = {
    val res = ConfigParser.parse(input).resolve().flatMap(_.get[T])
    res match {
      case Left(err: DecodingError) if err.message.startsWith(messageStart) => ()
      case Left(err: DecodingError)                                         =>
        fail(s"message '${err.message}' did not start with '$messageStart''")
      case Left(err) => fail(s"Expected DecodingError, but got $err")
      case _         => fail("decoding did not fail as expected")
    }
  }

  def decode[T: ConfigDecoder: DefaultKey](
      input: String,
      expected: T,
      modifyResult: T => T = identity[T](_)
  ): Unit =
    assertEquals(
      ConfigParser.parse(input).resolve().flatMap(_.get[T]),
      Right(modifyResult(expected))
    )

  def roundTrip[T: ConfigDecoder: ConfigEncoder](
      value: T,
      modifyResult: T => T = identity[T](_)
  ): Unit = {
    val result = ConfigBuilder.empty
      .withValue(testKey, value)
      .build
      .get[T](testKey)
      .map(modifyResult)
    assertEquals(result, Right(value))
  }

  test("DocumentMetadata - decode an instance with all fields populated") {
    val input =
      """{ 
        |laika.metadata {
        |  title = "Monkey Gone To Heaven"
        |  description = "It's indescribable"
        |  identifier = XX-33-FF-01
        |  authors = [ "Helen North", "Maria South" ]
        |  language = en
        |  datePublished = "2002-10-10T12:00:00"
        |  dateModified = "2002-12-12T12:00:00"
        |  version = 125
        |  canonicalLink = "http://foo.bar/baz"
        |}}
      """.stripMargin
    decode[DocumentMetadata](
      input,
      DocumentMetadata(
        Some("Monkey Gone To Heaven"),
        Some("It's indescribable"),
        Some("XX-33-FF-01"),
        Seq("Helen North", "Maria South"),
        Some("en"),
        Some(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get),
        Some(PlatformDateTime.parse("2002-12-12T12:00:00").toOption.get),
        Some("125"),
        Some(new URI("http://foo.bar/baz"))
      )
    )
  }

  test("DocumentMetadata - decode an instance with a single author") {
    val input =
      """{ 
        |laika.metadata {
        |  identifier = XX-33-FF-01
        |  author = "Dorothea West"
        |  language = en
        |  datePublished = "2002-10-10T12:00:00"
        |}}
      """.stripMargin
    decode[DocumentMetadata](
      input,
      DocumentMetadata(
        None,
        None,
        Some("XX-33-FF-01"),
        Seq("Dorothea West"),
        Some("en"),
        Some(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get)
      )
    )
  }

  test("DocumentMetadata - round-trip encode and decode") {
    val input = DocumentMetadata(
      Some("Monkey Gone To Heaven"),
      Some("Rhubarb, Rhubarb, Rhubarb"),
      Some("XX-33-FF-01"),
      Seq("Helen North", "Maria South"),
      Some("en"),
      Some(PlatformDateTime.parse("2012-10-10T12:00:00").toOption.get),
      Some(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get),
      Some("125"),
      Some(new URI("http://foo.bar/baz"))
    )
    roundTrip(input)
  }

  test("DocumentMetadata - fail with an invalid date") {
    val input =
      """{ 
        |laika.metadata {
        |  identifier = XX-33-FF-01
        |  author = "Dorothea West"
        |  language = en
        |  dateModified = "2000-XX-01T00:00:00Z"
        |}}
      """.stripMargin
    failDecode[DocumentMetadata](
      input,
      "Error decoding 'laika.metadata.dateModified': Invalid date format"
    )
  }

  test("DocumentMetadata - fail with an invalid URI") {
    val input =
      """{ 
        |laika.metadata {
        |  identifier = XX-33-FF-01
        |  author = "Dorothea West"
        |  language = en
        |  canonicalLink = "?#?@!#"
        |}}
      """.stripMargin
    val msg   =
      "Error decoding 'laika.metadata.canonicalLink': Invalid URI format: java.net.URISyntaxException: "
    failDecode[DocumentMetadata](input, msg)
  }

  object links {

    def sort(config: LinkConfig): LinkConfig = config.copy(targets = config.targets.sortBy(_.id))

    val fullyPopulatedInstance = LinkConfig(
      Seq(
        TargetDefinition("bar", InternalTarget(CurrentTree / "bar")),
        TargetDefinition("ext", ExternalTarget("http://ext.com")),
        TargetDefinition("foo", InternalTarget(CurrentTree / "foo"))
      ),
      Seq(Root / "foo", Root / "bar" / "baz"),
      Seq(
        ApiLinks("https://foo.api/", "foo", "package.html"),
        ApiLinks("https://bar.api/", "foo.bar")
      ),
      Seq(
        SourceLinks("https://foo.source/", "scala", "foo"),
        SourceLinks("https://bar.source/", "java", "foo.bar")
      )
    )

  }

  test("LinkConfig - decode an instance with all fields populated") {
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
        |    source = [
        |      { baseUri = "https://foo.source/", suffix = scala, packagePrefix = foo },
        |      { baseUri = "https://bar.source/", suffix = java, packagePrefix = foo.bar }
        |    ]
        |  }
        |}
      """.stripMargin
    decode[LinkConfig](input, links.fullyPopulatedInstance, links.sort)
  }

  test("LinkConfig - decode an instance with some fields populated") {
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
    decode[LinkConfig](
      input,
      LinkConfig(
        targets = Seq(TargetDefinition("foo", InternalTarget(CurrentTree / "foo"))),
        apiLinks = Seq(ApiLinks("https://bar.api/"))
      ),
      links.sort
    )
  }

  test("LinkConfig - round-trip encode and decode") {
    roundTrip(links.fullyPopulatedInstance, links.sort)
  }

  test("LinkValidation - decode an instance with exclusions") {
    val input =
      """{
        |  laika.links.validation {
        |    scope = global
        |    excluded = [/foo, /bar/baz]
        |  }
        |}
      """.stripMargin
    decode[LinkValidation](
      input,
      LinkValidation.Global(Seq(Root / "foo", Root / "bar" / "baz"))
    )
  }

  test("LinkValidation - decode an instance without exclusions") {
    val input =
      """{
        |  laika.links.validation {
        |    scope = local
        |  }
        |}
      """.stripMargin
    decode[LinkValidation](input, LinkValidation.Local)
  }

  test("LinkValidation - round-trip encode and decode") {
    roundTrip[LinkValidation](LinkValidation.Global(Seq(Root / "foo", Root / "bar" / "baz")))
  }

  object selections {

    val sample = Selections(
      SelectionConfig(
        "foo",
        ChoiceConfig("foo-a", "foo-label-a", selected = true),
        ChoiceConfig("foo-b", "foo-label-b")
      ).withSeparateEbooks,
      SelectionConfig(
        "bar",
        ChoiceConfig("bar-a", "bar-label-a"),
        ChoiceConfig("bar-b", "bar-label-b")
      )
    )

  }

  test("ChoiceGroupsConfig - decode an instance with all fields populated") {
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
    decode[Selections](input, selections.sample)
  }

  test("ChoiceGroupsConfig - round-trip encode and decode") {
    roundTrip(selections.sample)
  }

  object autonumbering {

    val fullyPopulatedInstance = AutonumberConfig(
      documents = true,
      sections = true,
      maxDepth = 5
    )

  }

  test("AutonumberConfig - decode an instance with all fields populated") {
    val input =
      """{
        |  laika.autonumbering {
        |    scope = all
        |    depth = 5
        |  }
        |}
      """.stripMargin
    decode[AutonumberConfig](input, autonumbering.fullyPopulatedInstance)
  }

  test("AutonumberConfig - round-trip encode and decode") {
    roundTrip(autonumbering.fullyPopulatedInstance)
  }

  object versions {

    val testInstance = Versions(
      Version("0.42.x", "0.42", canonical = true),
      Seq(
        Version("0.41.x", "0.41"),
        Version("0.40.x", "0.40", fallbackLink = "toc.html")
      ),
      Seq(
        Version("0.43.x", "0.43", label = Some("dev"))
      ),
      renderUnversioned = false,
      scannerConfig = Some(VersionScannerConfig("/path/to/versions", Seq(Root / "api")))
    )

  }

  test("Versions - decode an instance with all fields populated") {
    val input =
      """{
        |  laika.versions {
        |    currentVersion = { displayValue = "0.42.x", pathSegment = "0.42", fallbackLink = "index.html", canonical = true }
        |    olderVersions = [
        |      { displayValue = "0.41.x", pathSegment = "0.41", fallbackLink = "index.html" }
        |      { displayValue = "0.40.x", pathSegment = "0.40", fallbackLink = "toc.html" }
        |    ]
        |    newerVersions = [
        |      { displayValue = "0.43.x", pathSegment = "0.43", fallbackLink = "index.html", label = "dev" }
        |    ]
        |    renderUnversioned = false,
        |    scannerConfig = { rootDirectory = "/path/to/versions", exclude = [/api] }
        |  }
        |}
       """.stripMargin
    decode[Versions](input, versions.testInstance)
  }

  test("Versions - fail with invalid configuration") {
    val input    =
      """{
        |  laika.versions {
        |    currentVersion = { displayValue = "0.42.x", pathSegment = "0.42", fallbackLink = "index.html", canonical = true }
        |    olderVersions = [
        |      { displayValue = "0.41.x", pathSegment = "0.41", fallbackLink = "index.html", canonical = true }
        |      { displayValue = "0.40.x", pathSegment = "0.41", fallbackLink = "toc.html" }
        |    ]
        |  }
        |}
       """.stripMargin
    val res      = ConfigParser.parse(input).resolve().flatMap(_.get[Versions])
    val expected = ConfigErrors(
      NonEmptyChain(
        ValidationError("Path segments used for more than one version: 0.41"),
        ValidationError("More than one version marked as canonical: 0.41.x, 0.42.x")
      )
    )
    assertEquals(res, Left(expected))
  }

  test("Versions - round-trip encode and decode") {
    roundTrip(versions.testInstance)
  }

  test("IconRegistry - encode a list of icons") {
    val open     = IconStyle("open")
    val close    = IconGlyph('x')
    val registry = IconRegistry("open" -> open, "close" -> close)
    val encoded  = ConfigBuilder.empty.withValue(registry).build
    assertEquals(encoded.get[ConfigValue](LaikaKeys.icons.child("open")), Right(ASTValue(open)))
    assertEquals(encoded.get[ConfigValue](LaikaKeys.icons.child("close")), Right(ASTValue(close)))
  }

}
