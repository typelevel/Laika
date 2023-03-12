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

package laika.ast

import laika.config.{ ConfigDecoder, ConfigEncoder, DefaultKey, LaikaKeys }
import laika.time.PlatformDateTime

import java.net.URI

/** Metadata associated with a document.
  *
  * @author Jens Halm
  */
case class DocumentMetadata(
    title: Option[String] = None,
    description: Option[String] = None,
    identifier: Option[String] = None,
    authors: Seq[String] = Nil,
    language: Option[String] = None,
    datePublished: Option[PlatformDateTime.Type] = None,
    dateModified: Option[PlatformDateTime.Type] = None,
    version: Option[String] = None,
    canonicalLink: Option[URI] = None
) {

  /** Populates all empty Options in this instance with the provided defaults in case they are non-empty
    */
  def withDefaults(defaults: DocumentMetadata): DocumentMetadata = DocumentMetadata(
    title.orElse(defaults.title),
    description.orElse(defaults.description),
    identifier.orElse(defaults.identifier),
    authors ++ defaults.authors,
    language.orElse(defaults.language),
    datePublished.orElse(defaults.datePublished),
    dateModified.orElse(defaults.dateModified),
    version.orElse(defaults.version),
    canonicalLink.orElse(defaults.canonicalLink)
  )

  override def equals(obj: Any): Boolean = obj match {
    case other: DocumentMetadata =>
      other.title == title &&
      other.description == description &&
      other.identifier == identifier &&
      other.authors == authors &&
      other.language == language &&
      other.datePublished.toString == datePublished.toString && // equals does not work properly on js.Date
      other.dateModified.toString == dateModified.toString &&
      other.version == version &&
      other.canonicalLink == canonicalLink
    case _                       => false
  }

}

object DocumentMetadata {

  implicit val decoder: ConfigDecoder[DocumentMetadata] = ConfigDecoder.config.flatMap { config =>
    for {
      title         <- config.getOpt[String]("title")
      description   <- config.getOpt[String]("description")
      identifier    <- config.getOpt[String]("identifier")
      author        <- config.getOpt[String]("author")
      authors       <- config.get[Seq[String]]("authors", Nil)
      lang          <- config.getOpt[String]("language")
      datePublished <- config.getOpt[PlatformDateTime.Type]("datePublished")
      dateModified  <- config.getOpt[PlatformDateTime.Type]("dateModified")
      version       <- config.getOpt[String]("version")
      canonicalLink <- config.getOpt[URI]("canonicalLink")
    } yield {
      DocumentMetadata(
        title,
        description,
        identifier,
        authors ++ author.toSeq,
        lang,
        datePublished,
        dateModified,
        version,
        canonicalLink
      )
    }
  }

  implicit val encoder: ConfigEncoder[DocumentMetadata] = ConfigEncoder[DocumentMetadata] {
    metadata =>
      ConfigEncoder.ObjectBuilder.empty
        .withValue("title", metadata.title)
        .withValue("description", metadata.description)
        .withValue("identifier", metadata.identifier)
        .withValue("authors", metadata.authors)
        .withValue("language", metadata.language)
        .withValue("datePublished", metadata.datePublished)
        .withValue("dateModified", metadata.dateModified)
        .withValue("version", metadata.version)
        .withValue("canonicalLink", metadata.canonicalLink)
        .build
  }

  implicit val defaultKey: DefaultKey[DocumentMetadata] = DefaultKey(LaikaKeys.metadata)

}
