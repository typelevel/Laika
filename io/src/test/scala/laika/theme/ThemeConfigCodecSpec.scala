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

import laika.api.config.{
  Config,
  ConfigBuilder,
  ConfigDecoder,
  ConfigEncoder,
  ConfigParser,
  DefaultKey,
  Key
}
import laika.ast.Path.Root
import laika.api.config.Config.ConfigResult
import laika.api.config.ConfigError.DecodingFailed
import laika.config.*
import laika.render.fo.TestTheme
import laika.theme.config.{ BookConfig, DocumentMetadata }
import munit.FunSuite

import java.net.URI

class ThemeConfigCodecSpec extends FunSuite {

  private val testKey = Key("test")

  def decode[T: ConfigDecoder](input: String, key: Key): ConfigResult[T] =
    ConfigParser.parse(input).resolve().flatMap(_.get[T](key))

  def decode[T: ConfigDecoder: DefaultKey](input: String): ConfigResult[T] =
    ConfigParser.parse(input).resolve().flatMap(_.get[T])

  def decode[T: ConfigDecoder](config: Config): ConfigResult[T] = config.get[T](testKey)

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

  def failDecode[T: ConfigDecoder: DefaultKey](input: String, messageStart: String): Unit = {
    val res = ConfigParser.parse(input).resolve().flatMap(_.get[T])
    res match {
      case Left(err: DecodingFailed) if err.message.startsWith(messageStart) => ()
      case Left(err: DecodingFailed)                                         =>
        fail(s"message '${err.message}' did not start with '$messageStart''")
      case Left(err) => fail(s"Expected DecodingError, but got $err")
      case _         => fail("decoding did not fail as expected")
    }
  }

  test("BookConfig - decode an instance with all fields populated") {
    val input    =
      """{
        |laika {
        |  metadata {
        |    title = "Hell is around the corner"
        |    description = "Undescribable"
        |    identifier = XX-33-FF-01
        |    authors = [ "Helen North", "Maria South" ]
        |    language = en
        |    datePublished = "2002-10-10T12:00:00"
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
    val expected = BookConfig.empty
      .withMetadata(
        DocumentMetadata.empty
          .withTitle("Hell is around the corner")
          .withDescription("Undescribable")
          .withIdentifier("XX-33-FF-01")
          .addAuthors("Helen North", "Maria South")
          .withLanguage("en")
          .withDatePublished(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get)
      )
      .withNavigationDepth(3)
      .addFonts(TestTheme.fonts *)
      .withCoverImage(Root / "cover.jpg")
    assertEquals(decode[BookConfig](input, LaikaKeys.root), Right(expected))
  }

  test("BooKConfig - decode an instance with some fields populated") {
    val input    =
      """{
        |laika {
        |  metadata {
        |    identifier = XX-33-FF-01
        |  }
        |  navigationDepth = 3
        |}}
      """.stripMargin
    val expected = BookConfig.empty
      .withMetadata(
        DocumentMetadata.empty
          .withIdentifier("XX-33-FF-01")
      )
      .withNavigationDepth(3)
    assertEquals(decode[BookConfig](input, LaikaKeys.root), Right(expected))
  }

  test("BookConfig - round-trip encode and decode") {
    val input   = BookConfig.empty
      .withMetadata(
        DocumentMetadata.empty
          .withIdentifier("XX-33-FF-01")
      )
      .withNavigationDepth(3)
      .addFonts(TestTheme.fonts *)
      .withCoverImage(Root / "cover.jpg")
    val encoded = ConfigBuilder.empty.withValue(testKey, input).build
    assertEquals(decode[BookConfig](encoded), Right(input))
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
      DocumentMetadata.empty
        .withTitle("Monkey Gone To Heaven")
        .withDescription("It's indescribable")
        .withIdentifier("XX-33-FF-01")
        .addAuthors("Helen North", "Maria South")
        .withLanguage("en")
        .withDatePublished(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get)
        .withDateModified(PlatformDateTime.parse("2002-12-12T12:00:00").toOption.get)
        .withVersion("125")
        .withCanonicalLink(new URI("http://foo.bar/baz"))
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
      DocumentMetadata.empty
        .withIdentifier("XX-33-FF-01")
        .addAuthors("Dorothea West")
        .withLanguage("en")
        .withDatePublished(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get)
    )
  }

  test("DocumentMetadata - round-trip encode and decode") {
    val input = DocumentMetadata.empty
      .withTitle("Monkey Gone To Heaven")
      .withDescription("Rhubarb, Rhubarb, Rhubarb")
      .withIdentifier("XX-33-FF-01")
      .addAuthors("Helen North", "Maria South")
      .withLanguage("en")
      .withDatePublished(PlatformDateTime.parse("2012-10-10T12:00:00").toOption.get)
      .withDateModified(PlatformDateTime.parse("2002-10-10T12:00:00").toOption.get)
      .withVersion("125")
      .withCanonicalLink(new URI("http://foo.bar/baz"))
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

}
