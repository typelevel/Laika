/*
 * Copyright 2014 the original author or authors.
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

package laika.sbt

import sbt._
import Keys._

import laika.api._
import laika.io.InputProvider.Directories
import laika.io.OutputProvider.Directory
import laika.template.ParseTemplate
import laika.template.DefaultTemplate
import laika.tree.Documents._
import laika.tree.Elements._
import laika.directive.Directives._
import laika.parse.rst.ReStructuredText
import laika.parse.rst.Directives.Directive
import laika.parse.rst.TextRoles.TextRole
import laika.parse.markdown.Markdown
import laika.render.HTML
import laika.render.HTMLWriter

object LaikaSbtPlugin extends Plugin {

  
  object LaikaKeys {
    
    val Laika = config("laika")
    
    val site = TaskKey[File]("site", "Generates a static website")
    
    val docTypeMatcher = SettingKey[Option[Path => DocumentType]]("doctype-matcher", "Matches a path to a Laika document type")
    
    val encoding = SettingKey[String]("encoding", "The character encoding")
    
    val markdown = SettingKey[Markdown]("markdown", "The parser for Markdown files")
    
    val rawContent = SettingKey[Boolean]("raw-content", "Indicates whether embedding of raw content (like verbatim HTML) is supported in markup")

    val reStructuredText = SettingKey[ReStructuredText]("reStructuredText", "The parser for reStructuredText files")

    val markupParser = SettingKey[Parse]("markup-parser", "The parser for all text markup files")
    
    val templateParser = SettingKey[ParseTemplate]("template-parser", "The parser for template files")
    
    val parse = TaskKey[DocumentTree]("parse", "Parses all text markup and template files")
    
    val rewriteRules = SettingKey[Seq[DocumentContext => RewriteRule]]("rewrite-rules", "Custom rewrite rules to add to the standard rules")
    
    val rewrite = TaskKey[DocumentTree]("rewrite", "Applies all rewrite rules to the document tree")
    
    val siteRenderers = SettingKey[Seq[HTMLWriter => RenderFunction]]("renderers", "Custom renderers overriding the defaults per node type")
    
    val parallel = SettingKey[Boolean]("parallel", "Indicates whether parsers and renderers should run in parallel")
    
    val spanDirectives = SettingKey[Seq[Spans.Directive]]("span-directives", "Directives for inline markup")

    val blockDirectives = SettingKey[Seq[Blocks.Directive]]("block-directives", "Directives for block-level markup")

    val templateDirectives = SettingKey[Seq[Templates.Directive]]("template-directives", "Directives for templates")
    
    val rstSpanDirectives = SettingKey[Seq[Directive[Span]]]("rst-span-directives", "Inline directives for reStructuredText")
    
    val rstBlockDirectives = SettingKey[Seq[Directive[Block]]]("rst-block-directives", "Block directives for reStructuredText")
    
    val rstTextRoles = SettingKey[Seq[TextRole]]("rst-text-roles", "Custom text roles for reStructuredText")

    val includeAPI = SettingKey[Boolean]("include-api", "Indicates whether API documentation should be copied to the site")
    
    val copyAPI = TaskKey[File]("copy-api", "Copies the API documentation to the site")
    
  }
  
  
  object LaikaPlugin {
    import LaikaKeys._
    import Tasks._
    
    val defaults: Seq[Setting[_]] = inConfig(Laika)(Seq(
        
      sourceDirectories   := Seq(sourceDirectory.value / "docs"),
      
      target              := target.value / "docs",
      
      target in site      := target.value / "site",

      target in copyAPI   := (target in site).value / "api",
      
      excludeFilter       := HiddenFileFilter,
      
      encoding            := "UTF-8",

      docTypeMatcher      := None,
      
      rawContent          := false,
      
      markdown            := {
                            val md = (Markdown withBlockDirectives (blockDirectives.value: _*) withSpanDirectives (spanDirectives.value: _*))
                            if (rawContent.value) md.withVerbatimHTML else md
                          },

      reStructuredText    := { 
                            val rst = (ReStructuredText withLaikaBlockDirectives (blockDirectives.value: _*) withLaikaSpanDirectives 
                            (spanDirectives.value: _*) withBlockDirectives (rstBlockDirectives.value: _*) withSpanDirectives 
                            (rstSpanDirectives.value: _*) withTextRoles (rstTextRoles.value: _*))
                            if (rawContent.value) rst.withRawContent else rst
                          },
      
      markupParser        := (Parse as markdown.value or reStructuredText.value withoutRewrite),
      
      templateParser      := (ParseTemplate as DefaultTemplate.withDirectives(templateDirectives.value: _*)),
      
      rewriteRules        := Nil,
      
      siteRenderers       := Nil,
      
      parallel            := true,
      
      spanDirectives      := Nil,
      blockDirectives     := Nil,
      templateDirectives  := Nil,
      
      rstSpanDirectives   := Nil,
      rstBlockDirectives  := Nil,
      rstTextRoles        := Nil,
      
      includeAPI          := false,
      
      parse               := parseTask.value,
      rewrite             := rewriteTask.value,
      site                := siteTask.value,
      copyAPI             := copyAPITask.value
      
    ))
  }
  
  
  object Tasks {
    import LaikaKeys._
    import Def._
    
    // TODO - add logging

    val parseTask = task {
      val builder = Directories(sourceDirectories.value, excludeFilter.value.accept)(encoding.value)
        .withTemplates(templateParser.value)
      val inputTree = if (parallel.value) builder.inParallel else builder
      markupParser.value fromTree inputTree
    }
    
    val rewriteTask = task {
      parse.value rewrite (rewriteRules.value, AutonumberContext.defaults)
    }
    
    val siteTask = task {
      val apiDir = copyAPI.value
      val targetDir = (target in site).value
      val builder = Directory(targetDir)(encoding.value)
      val outputTree = if (parallel.value) builder.inParallel else builder
      val render = ((Render as HTML) /: siteRenderers.value) { case (render, renderer) => render using renderer }
      render from rewrite.value toTree outputTree
      targetDir
    }
    
    val copyAPITask = taskDyn {
      if (includeAPI.value) task { 
        val apiMappings = (mappings in packageDoc in Compile).value
        val targetDir = (target in copyAPI).value
        val targetMappings = apiMappings map { case (file, target) => (file, targetDir / target) }
        Sync(streams.value.cacheDirectory / "laika")(targetMappings)
        targetDir 
      }
      else task { 
        (target in copyAPI).value 
      }
    }
    
  }
  
  
}
