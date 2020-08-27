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

import laika.ast.{DocumentMetadata, Path}
import laika.config.{ConfigDecoder, ConfigEncoder, DefaultKey, LaikaKeys}

/** Captures common configuration element of e-books, used by both EPUB and PDF renderers.
  * 
  * @param metadata        metadata to be embedded in the document in a way that respective reader software can surface
  * @param navigationDepth the number of levels to provide navigation structure for
  * @param fonts           the fonts that should be embedded in the e-book output
  * @param coverImage      the path to the cover image within the virtual document tree   
  */
case class BookConfig (metadata: DocumentMetadata = DocumentMetadata(), 
                       navigationDepth: Option[Int] = None,
                       fonts: Seq[FontDefinition] = Nil,
                       coverImage: Option[Path] = None) 

object BookConfig {

  implicit val decoder: ConfigDecoder[BookConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      metadata   <- config.get[DocumentMetadata](LaikaKeys.metadata.local, DocumentMetadata())
      fonts      <- config.get[Seq[FontDefinition]]("fonts", Nil)
      depth      <- config.getOpt[Int]("navigationDepth")
      coverImage <- config.getOpt[Path]("coverImage")
    } yield {
      BookConfig(metadata, depth, fonts, coverImage)
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
  implicit val defaultKey: DefaultKey[BookConfig] = DefaultKey(LaikaKeys.root)
  
}
