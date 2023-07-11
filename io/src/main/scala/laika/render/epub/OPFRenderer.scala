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

import laika.ast._
import laika.format.EPUB
import laika.format.EPUB.ScriptedTemplate
import laika.io.model.{ RenderedDocument, RenderedTreeRoot }
import laika.rewrite.link.SlugBuilder

/** Renders the content of an EPUB Package document (OPF).
  *
  * @author Jens Halm
  */
private[epub] class OPFRenderer {

  /** Inserts the specified spine references into the OPF document template
    * and returns the content of the entire OPF file.
    */
  private def fileContent(
      identifier: String,
      language: String,
      title: String,
      coverImage: Option[String],
      timestamp: String,
      docRefs: Seq[DocumentRef],
      authors: Seq[String]
  ): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<package
       |    version="3.0"
       |    xmlns="http://www.idpf.org/2007/opf"
       |    unique-identifier="epub-id-1"
       |    prefix="ibooks: http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/">
       |  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
       |    <dc:identifier id="epub-id-1">$identifier</dc:identifier>
       |    <dc:title>$title</dc:title>
       |    <dc:date id="epub-date">$timestamp</dc:date>
       |    <dc:language>$language</dc:language>
       |${authors.map(author => s"    <dc:creator>$author</dc:creator>").mkString("\n")}
       |${
        docRefs.filter(ref => coverImage.contains(ref.link)).map { ref =>
          s"""    <meta name="cover" content="${ref.id}" />"""
        }.mkString("\n")
      }
       |    <meta property="dcterms:modified">$timestamp</meta>
       |  </metadata>
       |  <manifest>
       |    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml" />
       |    <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav" />
       |${
        docRefs.map { ref =>
          s"""    <item id="${ref.id}" href="${ref.link}" media-type="${ref.mediaType}" ${
              ref.scriptedProperty
            }/>"""
        }.mkString("\n")
      }
       |  </manifest>
       |  <spine toc="ncx">
       |${
        docRefs.filter(_.isSpine).map { ref => s"""    <itemref idref="${ref.id}" />""" }.mkString(
          "\n"
        )
      }
       |  </spine>
       |  <guide>
       |    <reference type="toc" title="Table of Content" href="nav.xhtml" />
       |${
        docRefs.filter(_.isCover).map { ref =>
          s"""    <reference type="cover" title="Cover" href="${ref.link}" />"""
        }.mkString("\n")
      }
       |  </guide>
       |</package>
    """.stripMargin.replaceAll("[\n]+", "\n")

  private case class DocumentRef(
      path: Path,
      mediaType: String,
      isSpine: Boolean,
      isCover: Boolean = false,
      forceXhtml: Boolean = false,
      isScripted: Boolean = false
  ) {

    val link = NavigationBuilder.fullPath(path, forceXhtml)

    val id = {
      val candidate = link.drop(8).replace("/", "_").replace(".", "_")
      SlugBuilder.default(candidate)
    }

    val scriptedProperty: String = if (isScripted) """properties="scripted"""" else ""

  }

  /** Renders the content of an EPUB Package document (OPF) generated from the specified document tree.
    */
  def render[F[_]](result: RenderedTreeRoot[F], title: String, config: EPUB.BookConfig): String = {

    lazy val hasScriptDocuments: Boolean =
      result.staticDocuments.exists(_.path.suffix.exists(s => s == "epub.js" || s == "shared.js"))

    def isScripted(doc: RenderedDocument): Boolean =
      doc.config.get[ScriptedTemplate].getOrElse(ScriptedTemplate.Never) match {
        case ScriptedTemplate.Never  => false
        case ScriptedTemplate.Always => true
        case ScriptedTemplate.Auto   => hasScriptDocuments
      }

    val coverDoc = result.coverDocument.map(doc =>
      DocumentRef(
        doc.path,
        "application/xhtml+xml",
        isSpine = true,
        isCover = true,
        forceXhtml = true,
        isScripted = isScripted(doc)
      )
    )
    val titleDoc = result.titleDocument.map(doc =>
      DocumentRef(
        doc.path,
        "application/xhtml+xml",
        isSpine = true,
        forceXhtml = true,
        isScripted = isScripted(doc)
      )
    )

    val renderedDocs = result.allDocuments.drop(coverDoc.size + titleDoc.size).map { doc =>
      DocumentRef(
        doc.path,
        "application/xhtml+xml",
        isSpine = true,
        forceXhtml = true,
        isScripted = isScripted(doc)
      )
    }

    val staticDocs = result.staticDocuments.flatMap { in =>
      in.path.suffix
        .flatMap(suf =>
          MimeTypes.supportedTypes.get(suf).orElse(
            MimeTypes.supportedTypes.get(suf.split("\\.").last)
          )
        )
        .map { mediaType =>
          DocumentRef(in.path, mediaType, isSpine = false)
        }
    }

    val docRefs = coverDoc.toSeq ++ titleDoc.toSeq ++ renderedDocs ++ staticDocs

    fileContent(
      config.identifier,
      config.language,
      title,
      config.coverImage.map(p => "content/" + p.relative.toString),
      config.formattedDate,
      docRefs,
      config.metadata.authors
    )
  }

}
