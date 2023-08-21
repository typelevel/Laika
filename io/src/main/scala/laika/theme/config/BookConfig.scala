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

package laika.theme.config

import laika.ast.{ DocumentMetadata, Path }
import laika.config.Config.ConfigResult
import laika.config.{ Config, ConfigDecoder, ConfigEncoder, Key, LaikaKeys }

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.{ Locale, UUID }

/** Captures common configuration element of e-books, used by both EPUB and PDF renderers.
  */
sealed abstract class BookConfig {

  /** Metadata to be embedded in the document in a way that respective reader software can surface. */
  def metadata: DocumentMetadata

  /** The number of levels to provide navigation structure for. */
  def navigationDepth: Option[Int]

  /** The fonts that should be embedded in the e-book output. */
  def fonts: Seq[FontDefinition]

  /** The path to the cover image within the virtual document tree. */
  def coverImage: Option[Path]

  /** The identifier to be used for metadata in the book output,
    * either taken from the provided metadata or, if that is empty, generated randomly.
    */
  def identifier: String

  /** The language to be used for metadata in the book output,
    * either taken from the provided metadata or, if that is empty, taken from the default locale
    * of the running program.
    */
  def language: String

  /** The publication date of the book,
    * either taken from the provided metadata or, if that is empty, using the current time.
    */
  def date: OffsetDateTime

  /** The publication date formatted as an ISO instant. */
  def formattedDate: String

  def withMetadata(value: DocumentMetadata): BookConfig
  def withNavigationDepth(value: Int): BookConfig
  def addFonts(values: FontDefinition*): BookConfig
  def removeFonts(filter: FontDefinition => Boolean): BookConfig
  def clearFonts: BookConfig
  def withCoverImage(value: Path): BookConfig
}

object BookConfig {

  val empty: BookConfig = Impl()

  private final case class Impl(
      metadata: DocumentMetadata = DocumentMetadata.empty,
      navigationDepth: Option[Int] = None,
      fonts: Seq[FontDefinition] = Nil,
      coverImage: Option[Path] = None
  ) extends BookConfig {

    override def productPrefix: String = "BookConfig"

    def withMetadata(value: DocumentMetadata): BookConfig = copy(metadata = value)

    def withNavigationDepth(value: Int): BookConfig = copy(navigationDepth = Some(value))

    def addFonts(values: FontDefinition*): BookConfig = copy(fonts = fonts ++ values)

    def removeFonts(filter: FontDefinition => Boolean): BookConfig =
      copy(fonts = fonts.filterNot(filter))

    def clearFonts: BookConfig = copy(fonts = Nil)

    def withCoverImage(value: Path): BookConfig = copy(coverImage = Some(value))

    lazy val identifier: String =
      metadata.identifier.getOrElse(s"urn:uuid:${UUID.randomUUID.toString}")

    lazy val date: OffsetDateTime =
      metadata.dateModified.orElse(metadata.datePublished).getOrElse(OffsetDateTime.now())

    lazy val formattedDate: String =
      DateTimeFormatter.ISO_INSTANT.format(date.toInstant.truncatedTo(ChronoUnit.SECONDS))

    lazy val language: String = metadata.language.getOrElse(Locale.getDefault.toLanguageTag)
  }

  implicit val decoder: ConfigDecoder[BookConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      metadata   <- config.get[DocumentMetadata](LaikaKeys.metadata.local, DocumentMetadata.empty)
      fonts      <- config.get[Seq[FontDefinition]]("fonts", Nil)
      depth      <- config.getOpt[Int]("navigationDepth")
      coverImage <- config.getOpt[Path]("coverImage")
    } yield {
      Impl(metadata, depth, fonts, coverImage)
    }
  }

  implicit val encoder: ConfigEncoder[BookConfig] = ConfigEncoder[BookConfig] { bc =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue(LaikaKeys.metadata.local, bc.metadata)
      .withValue("navigationDepth", bc.navigationDepth)
      .withValue("fonts", bc.fonts)
      .withValue("coverImage", bc.coverImage)
      .build
  }

  /** Reads a `BookConfig` instance from the given instance using the specified key,
    * while reading fallback values defined directly under the `laika` key.
    *
    * Empty optional values will be populated from the fallback instance,
    * while properties of type `Seq` will accumulate values from the fallback and the value
    * stored under the given key.
    */
  def decodeWithDefaults(config: Config, key: Key): ConfigResult[BookConfig] = for {
    bookConfig <- config.getOpt[BookConfig](key).map(_.getOrElse(BookConfig.empty))
    baseConfig <- config.getOpt[BookConfig](LaikaKeys.root).map(_.getOrElse(BookConfig.empty))
  } yield {
    Impl(
      bookConfig.metadata.withDefaults(baseConfig.metadata),
      bookConfig.navigationDepth.orElse(baseConfig.navigationDepth),
      bookConfig.fonts ++ baseConfig.fonts,
      bookConfig.coverImage.orElse(baseConfig.coverImage)
    )
  }

}
