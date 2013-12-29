package laika.sbt

import sbt._
import Keys._
import laika.api.Parse
import laika.template.ParseTemplate
import laika.tree.Documents._
import laika.tree.Elements._
import laika.directive.Directives._
import laika.parse.rst.Directives.Directive
import laika.parse.rst.TextRoles.TextRole

object LaikaSbtPlugin {

  
  object LaikaKeys {
    
    val site = TaskKey[File]("site", "Generates a static website")
    
    val docTypeMatcher = SettingKey[Path => DocumentType]("docTypeMatcher", "Matches a path to a Laika document type")
    
    val encoding = SettingKey[String]("encoding", "The character encoding")
    
    val markupParser = SettingKey[Parse]("markupParser", "The parser for text markup files")
    
    val templateParser = SettingKey[ParseTemplate]("templateParser", "The parser for template files")
    
    val parse = TaskKey[DocumentTree]("parse", "Parses all text markup and template files")
    
    val rewriteRules = SettingKey[Seq[DocumentContext => PartialFunction[Element,Option[Element]]]]("rewriteRules", "Custom rewrite rules to add to the standard rules")
    
    val rewrite = TaskKey[DocumentTree]("rewrite", "Applies all rewrite rules to the document tree")
    
    val renderers = SettingKey[Seq[_ => PartialFunction[Element,Unit]]]("renderers", "Custom renderers overriding the defaults per node type")
    
    val parallel = SettingKey[Boolean]("parallel", "Indicates whether parsers and renderers should run in parallel")
    
    val spanDirectives = SettingKey[Seq[Spans.Directive]]("spanDirectives", "Directives for inline markup")

    val blockDirectives = SettingKey[Seq[Blocks.Directive]]("blockDirectives", "Directives for block-level markup")

    val templateDirectives = SettingKey[Seq[Templates.Directive]]("templateDirectives", "Directives for templates")
    
    val rstSpanDirectives = SettingKey[Seq[Directive[Span]]]("rstSpanDirectives", "Inline directives for reStructuredText")
    
    val rstBlockDirectives = SettingKey[Seq[Directive[Block]]]("rstBlockDirectives", "Block directives for reStructuredText")
    
    val rstTextRoles = SettingKey[Seq[TextRole]]("rstTextRoles", "Custom text roles for reStructuredText")

    val includeAPI = SettingKey[Boolean]("includeAPI", "Indicates whether API documentation should be copied to the site")
    
    val copyAPI = TaskKey[File]("copyAPI", "Copies the API documentation to the site")
    
  }
  
  
}
