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

import laika.bundle.SyntaxHighlighter
import laika.parse.code.common.{Keywords, TagBasedFormats}
import laika.parse.code.{CodeCategory, CodeSpanParsers}

/**
  * @author Jens Halm
  */
object XML extends TagBasedFormats {
  
  val pi: CodeSpanParsers = CodeSpanParsers(CodeCategory.XML.ProcessingInstruction, "<?", "?>")
  val cdata: CodeSpanParsers = CodeSpanParsers(CodeCategory.XML.CData, "<![CDATA[", "]]>")

  object DTD {
    
    val notation: CodeSpanParsers = TagParser(CodeCategory.XML.DTDTagName, "<!", ">", "NOTATION").embed(
      Keywords("SYSTEM", "PUBLIC"),
      string,
      name(CodeCategory.Identifier),
    )
  
    val entity: CodeSpanParsers = TagParser(CodeCategory.XML.DTDTagName, "<!", ">", "ENTITY").embed(
      Keywords("SYSTEM", "PUBLIC", "NDATA"),
      string,
      name(CodeCategory.Identifier),
    )
  
    val attribute: CodeSpanParsers = TagParser(CodeCategory.XML.DTDTagName, "<!", ">", "ATTLIST").embed(
      Keywords("CDATA", "ID", "IDREF", "IDREFS", "ENTITY", "ENTITIES", "NMTOKEN", "NMTOKENS", "#REQUIRED", "#IMPLIED", "#FIXED", "NOTATION"),
      string,
      name(CodeCategory.Identifier),
    )

    val element: CodeSpanParsers = TagParser(CodeCategory.XML.DTDTagName, "<!", ">", "ELEMENT").embed(
      Keywords("EMPTY", "ANY", "#PCDATA"),
      name(CodeCategory.Identifier)
    )
    
  }

  val xmlDecl: CodeSpanParsers = TagParser(CodeCategory.Tag.Name, "<?", "?>", "xml").embed(
    string,
    name(CodeCategory.AttributeName)
  )

  val docType: CodeSpanParsers = TagParser(CodeCategory.XML.DTDTagName, "<!", ">", "DOCTYPE").embed(
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
  
  val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("xml")(
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
