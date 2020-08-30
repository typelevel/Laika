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

package laika.io.helper

import laika.ast.{TemplateRoot, TemplateString}
import laika.render.fo.TestTheme

object RenderResult {
  
  private def buildResult (template: TemplateRoot, insertions: Seq[String]): String = {
    val it = insertions.iterator
    template.content.map {
      case TemplateString(content, _) => content
      case _ => it.next()
    }.mkString
  }
    

  object html {
    def withDefaultTemplate(title: String, content: String): String = buildResult(TestTheme.htmlTemplate, Seq(content))
  }

  object epub {
    def withDefaultTemplate(title: String, content: String): String = buildResult(TestTheme.htmlTemplate, Seq(content))
  }
  
  object fo {
    def withHeliumTemplate(content: String): String = HeliumFoResult.withContent(content)
    def withFallbackTemplate(content: String): String = buildResult(TestTheme.foTemplate, Seq("", content))
    def withFallbackTemplate(result: String, bookmarks: String = ""): String = buildResult(TestTheme.foTemplate, Seq(bookmarks, result))
  }
  
}

object HeliumFoResult {
  
  def withContent (content: String): String = 
    s"""<?xml version="1.0" encoding="utf-8"?>
       |
       |<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:fox="http://xmlgraphics.apache.org/fop/extensions">
       |
       |  <fo:layout-master-set>
       |
       |    <fo:simple-page-master
       |        master-name="default"
       |        page-height="29.7cm"
       |        page-width="21cm"
       |        margin-top="1cm"
       |        margin-bottom="1cm"
       |        margin-left="2.5cm"
       |        margin-right="2.5cm">
       |      <fo:region-body margin-top="2cm" margin-bottom="2cm"/>
       |      <fo:region-before extent="3cm"/>
       |      <fo:region-after extent="1cm"/>
       |    </fo:simple-page-master>
       |
       |  </fo:layout-master-set>
       |
       |  
       |
       |  
       |
       |  <fo:page-sequence master-reference="default">
       |
       |    <fo:static-content flow-name="xsl-region-before">
       |      <fo:block border-bottom-width="1pt" border-bottom-style="solid"
       |          font-family="sans-serif" font-weight="bold" font-size="9pt" text-align="center">
       |        <fo:retrieve-marker
       |            retrieve-class-name="chapter"
       |            retrieve-position="first-including-carryover"
       |        />
       |      </fo:block>
       |    </fo:static-content>
       |
       |    <fo:static-content flow-name="xsl-region-after">
       |      <fo:block height="100%" font-family="sans-serif" font-weight="bold" font-size="10pt" text-align="center">
       |        <fo:page-number/>
       |      </fo:block>
       |    </fo:static-content>
       |
       |    <fo:flow flow-name="xsl-region-body">
       |
       |      $content
       |
       |    </fo:flow>
       |
       |  </fo:page-sequence>
       |
       |</fo:root>
       |""".stripMargin
  
}
