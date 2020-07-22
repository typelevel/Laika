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

package laika.helium.generate

import laika.ast.{TemplateContextReference, TemplateRoot, TemplateString}
import laika.helium.Helium
import laika.rewrite.ReferenceResolver.CursorKeys

/** The default template for HTML renderers.
  * 
  * @author Jens Halm
  */
class HTMLTemplate (helium: Helium) {
  
  private val fontCSS = helium.fontResources.flatMap { res =>
    res.resource.webCSS.map { url =>
      s"""<link rel="stylesheet" href="$url">"""
    }.mkString("\n    ")
  } // TODO - combine this with the @:styleLinks directive
  
  /*
  
  <meta name="author" content="......"/>
  
  <link rel="icon" type="image/png" href="/img/favicon.png"/>
  <link rel="icon" type="image/png" sizes="32x32" href="/img/favicon32x32.png"/>
  
    <link href='http://fonts.googleapis.com/css?family=Lato:400,700' rel='stylesheet' type='text/css'>  
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
    <link rel="stylesheet" href="../icons/icofont.min.css">
    
    TODO : title, description, keywords from metadata, favicon from Helium config move additional nav items to top bar
   */

  private val templateText = s"""<!DOCTYPE html>
   |<html lang="en">
   |  <head>
   |    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
   |    <meta charset="utf-8">
   |    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
   |
   |    <title>$${cursor.currentDocument.title}</title>
   |
   |    <meta name="viewport" content="width=device-width, initial-scale=1.0">
   |    <meta name="description" content="Customizable and extensible toolkit for transforming lightweight markup languages to various types of output formats, written in Scala" />
   |
   |    $fontCSS
   |    <link href="../css/vars.css" rel="stylesheet">
   |    <link href="../css/container.css" rel="stylesheet">
   |    <link href="../css/content.css" rel="stylesheet">
   |    <link href="../css/nav.css" rel="stylesheet">
   |    <link href="../css/code.css" rel="stylesheet">
   |    <script src="../js/theme.js"></script>
   |
   |  </head>
   |
   |  <body>
   |
   |    <nav id="sidebar">
   |
   |      <a id="close-nav-icon">
   |        <i class="icofont-close-circled icofont-xlg"></i>
   |      </a>
   |
   |      <ul class="nav nav-list">
   |
   |        @:laika-nav
   |        
   |        <li class="nav-header">Project Links</li>
   |        <li><a href="http://github.com/planet42/Laika">Source Code</a></li>
   |        <li><a href="../api/laika/api/">API Documentation</a></li>
   |        <li><a href="http://planet42.org/">Demo App</a></li>
   |        
   |      </ul>
   |
   |    </nav>
   |
   |    <div id="container">
   |
   |      <a id="open-nav-icon">
   |        <i class="icofont-navigation-menu icofont-xlg"></i>
   |      </a>
   |
   |      <div id="page-nav">
   |        <p class="header"><a href="#">§</a></p>
   |        <!--@/:page-nav-->
   |        <p class="footer"><a href="#todo">Source for this Page</a></p>
   |      </div>
   |
   |      <main class="content">
   |
   |        §
   |        
   |      </main>
   |      
   |    </div>
   |
   |  </body>
   |</html>
   |""".stripMargin
  
  // <p class="header"><a href="#">$${cursor.currentDocument.title}</a></p>
  // $${cursor.currentDocument.content}

  /** The default template for HTML renderers.
    * 
    * It can be overridden by placing a custom template document
    * with the name `default.template.html` into the root directory
    * of the input files. Alternatively the default can also be overridden
    * for individual sub-directories with a corresponding file with the same name.
    */
  val root: TemplateRoot = {
    val templateSpans = templateText.split("§").map(TemplateString(_))
    TemplateRoot(
      templateSpans(0),
      TemplateContextReference(CursorKeys.documentTitle, required = true),
      templateSpans(1),
      TemplateContextReference(CursorKeys.documentContent, required = true),
      templateSpans(2)
    )
  }
  
}
