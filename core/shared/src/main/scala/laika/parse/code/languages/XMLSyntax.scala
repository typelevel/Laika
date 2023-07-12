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

package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.bundle.SyntaxHighlighter
import laika.parse.code.common.{ Keywords, TagFormats }
import laika.parse.code.{ CodeCategory, CodeSpanParser }

/** @author Jens Halm
  */
object XMLSyntax extends SyntaxHighlighter {

  import TagFormats._

  private val pi: CodeSpanParser =
    CodeSpanParser(CodeCategory.XML.ProcessingInstruction, "<?", "?>")

  private val cdata: CodeSpanParser = CodeSpanParser(CodeCategory.XML.CData, "<![CDATA[", "]]>")

  private object DTD {

    private[XMLSyntax] def dtdTag(name: String): TagParser =
      customTag("<!", ">")
        .withCategory(CodeCategory.XML.DTDTagName)
        .forTagName(name)

    val notation: CodeSpanParser = dtdTag("NOTATION").embed(
      Keywords("SYSTEM", "PUBLIC"),
      string,
      name(CodeCategory.Identifier)
    )

    val entity: CodeSpanParser = dtdTag("ENTITY").embed(
      Keywords("SYSTEM", "PUBLIC", "NDATA"),
      string,
      name(CodeCategory.Identifier)
    )

    val attribute: CodeSpanParser = dtdTag("ATTLIST").embed(
      Keywords(
        "CDATA",
        "ID",
        "IDREF",
        "IDREFS",
        "ENTITY",
        "ENTITIES",
        "NMTOKEN",
        "NMTOKENS",
        "#REQUIRED",
        "#IMPLIED",
        "#FIXED",
        "NOTATION"
      ),
      string,
      name(CodeCategory.Identifier)
    )

    val element: CodeSpanParser = dtdTag("ELEMENT").embed(
      Keywords("EMPTY", "ANY", "#PCDATA"),
      name(CodeCategory.Identifier)
    )

  }

  private val xmlDecl: CodeSpanParser =
    customTag("<?", "?>")
      .forTagName("xml")
      .embed(
        string,
        name(CodeCategory.AttributeName)
      )

  private val docType: CodeSpanParser = DTD.dtdTag("DOCTYPE").embed(
    Keywords("SYSTEM", "PUBLIC"),
    string,
    pi,
    comment,
    DTD.notation,
    DTD.entity,
    DTD.attribute,
    DTD.element,
    name(CodeCategory.Identifier)
  )

  val language: NonEmptyList[String] = NonEmptyList.of("xml")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    xmlDecl,
    docType,
    pi,
    comment,
    cdata,
    ref,
    emptyTag,
    startTag,
    endTag
  )

}
