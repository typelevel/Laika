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
import laika.io.model.RenderedTreeRoot

/** Renders the content of an EPUB Package document (OPF).
  *
  * @author Jens Halm
  */
class OPFRenderer {


  /** Inserts the specified spine references into the OPF document template
    * and returns the content of the entire OPF file.
    */
  def fileContent (identifier: String, language: String, title: String, coverImage: Option[String], timestamp: String, docRefs: Seq[DocumentRef], authors: Seq[String] = Nil): String =
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
       |${docRefs.filter(ref => coverImage.contains(ref.link)).map { ref => s"""    <meta name="cover" content="${ref.id}" />"""}.mkString("\n")}
       |    <meta property="dcterms:modified">$timestamp</meta>
       |  </metadata>
       |  <manifest>
       |    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml" />
       |    <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav" />
       |${docRefs.map { ref => s"""    <item id="${ref.id}" href="${ref.link}" media-type="${ref.mediaType}" />""" }.mkString("\n")}
       |  </manifest>
       |  <spine toc="ncx">
       |${docRefs.filter(_.isCover).map { ref => s"""    <itemref idref="${ref.id}" />""" }.mkString("\n")}
       |${docRefs.filter(_.isTitle).map { ref => s"""    <itemref idref="${ref.id}" />""" }.mkString("\n")}
       |    <itemref idref="nav" />
       |${docRefs.filter(_.isSpine).map { ref => s"""    <itemref idref="${ref.id}" />""" }.mkString("\n")}
       |  </spine>
       |  <guide>
       |    <reference type="toc" title="Table of Content" href="nav.xhtml" />
       |${docRefs.filter(_.isCover).map { ref => s"""    <reference type="cover" title="Cover" href="${ref.link}" />""" }.mkString("\n")}
       |  </guide>
       |</package>
    """.stripMargin.replaceAll("[\n]+", "\n")

  private case class DocumentRef (path: Path, mediaType: String, isSpine: Boolean, isTitle: Boolean = false, isCover: Boolean = false, forceXhtml: Boolean = false) {

    val link = BookNavigation.fullPath(path, forceXhtml)

    val id = link.drop(8).replaceAllLiterally("/", "_").replaceAllLiterally(".", "_")

  }

  /** Renders the content of an EPUB Package document (OPF) generated from
    * the specified document tree.
    */
  def render[F[_]] (result: RenderedTreeRoot[F], config: EPUB.Config): String = {

    val coverDoc = result.coverDocument.map(doc => DocumentRef(doc.path, "application/xhtml+xml", isSpine = false, isCover = true, forceXhtml = true))
    val titleDoc = result.titleDocument.map(doc => DocumentRef(doc.path, "application/xhtml+xml", isSpine = false, isTitle = true, forceXhtml = true))
    
    val renderedDocs = result.allDocuments.drop(coverDoc.size + titleDoc.size).map { doc =>
      DocumentRef(doc.path, "application/xhtml+xml", isSpine = true, forceXhtml = true)
    }

    val staticDocs = result.staticDocuments.filter(in => MimeTypes.supportedTypes.contains(in.path.suffix)).map { in =>
      DocumentRef(in.path, MimeTypes.supportedTypes(in.path.suffix), isSpine = false)
    }

    val docRefs = coverDoc.toSeq ++ titleDoc.toSeq ++ renderedDocs ++ staticDocs

    val title = if (result.title.isEmpty) "UNTITLED" else SpanSequence(result.title).extractText
    fileContent(config.identifier, config.language.toLanguageTag, title, config.coverImage.map("content/"+_), config.formattedDate, docRefs, config.metadata.authors)
  }


}
