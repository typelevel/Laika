/*
 * Copyright 2013 the original author or authors.
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

package laika.template

import scala.util.parsing.input.Reader

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigParseOptions

import laika.directive.DirectiveParsers
import laika.parse.InlineParsers
import laika.tree.Documents.Path
import laika.tree.Elements.InvalidBlock
import laika.tree.Elements.InvalidSpan
import laika.tree.Elements.LiteralBlock
import laika.tree.Elements.Span
import laika.tree.Elements.SystemMessage
import laika.tree.Elements.Text
import laika.tree.Templates.MarkupContextReference
import laika.tree.Templates.TemplateContextReference
import laika.tree.Templates.TemplateDocument
import laika.tree.Templates.TemplateElement
import laika.tree.Templates.TemplateRoot
import laika.tree.Templates.TemplateSpan
import laika.tree.Templates.TemplateString


/** Provides parsers for the default template format.
 */
trait TemplateParsers extends InlineParsers {

  
  /** Parses a reference enclosed between `{{` and `}}`.
   */
  def reference[T] (f: String => T): Parser[T] = {
    
    val refName = { // TODO - promote to inline parsers
      val alphanum = anyIn('0' to '9', 'a' to 'z', 'A' to 'Z') min 1
      val symbol = anyOf('-', '_', '.', ':', '+') take 1
      
      alphanum ~ ((symbol ~ alphanum)*) ^^ { 
        case start ~ rest => start + (rest map { case a~b => a+b }).mkString
      }
    }
      
    '{' ~ ws ~> refName <~ ws ~ "}}" ^^ f  
    
  }
  
  
}


/** Specializations for the generic template parsers for 
 *  elements embedded in either markup blocks, markup inline
 *  elements or templates.
 */
object TemplateParsers {
  

  /** Parsers specific to templates.
   */
  trait Templates extends TemplateParsers with DirectiveParsers.TemplateDirectives {
    
    protected def prepareSpanParsers = Map(
      '{' -> (reference(TemplateContextReference(_))),    
      '@' -> (templateDirectiveParser),
      '\\'-> ((any take 1) ^^ { Text(_) })
    )
  
    def configParser (path: Path): Parser[Either[InvalidSpan,Config]] = "<%" ~> anyUntil("%>") <~ ws ~ eol ^^ { str =>
      try {
        Right(ConfigFactory.parseString(str, ConfigParseOptions.defaults().setOriginDescription("path:"+path)))
      }
      catch {
        case ex: Exception => Left(InvalidSpan(SystemMessage(laika.tree.Elements.Error, 
            "Error parsing config header: "+ex.getMessage), TemplateString("<%"+str+"%>")))
      }
    } 
    
    lazy val templateSpans = (spans(any, spanParsers) ^^ { _.collect { 
      case s:TemplateSpan => s 
      case Text(s,opt) => TemplateString(s,opt)
    }})
    
    def templateWithConfig (path: Path) = opt(configParser(path)) ~ templateSpans ^^ {
      case Some(Right(config)) ~ root => (config, root)
      case Some(Left(span)) ~ root    => (ConfigFactory.empty(), TemplateElement(span) :: root)
      case None ~ root                => (ConfigFactory.empty(), root)
    }
  
    def parseTemplate (reader: Reader[Char], path: Path) = {
      val (config, root) = parseMarkup(templateWithConfig(path), reader)
      TemplateDocument(path, TemplateRoot(root), config)
    }
    
    def parseTemplatePart (source: String) = parseMarkup(templateSpans, source)
      
  }
  
  
  /** Parsers specific to block elements in text markup.
   */
  trait MarkupBlocks extends TemplateParsers with DirectiveParsers.BlockDirectives {
    
    abstract override protected def prepareBlockParsers (nested: Boolean) = 
      blockDirectiveParser :: super.prepareBlockParsers(nested)
    
    override def config (path: Path): Parser[Either[InvalidBlock,Config]] = "<%" ~> anyUntil("%>") <~ ws ~ eol ^^ { str =>
      try {
        Right(ConfigFactory.parseString(str, ConfigParseOptions.defaults().setOriginDescription("path:"+path)))
      }
      catch {
        case ex: Exception => Left(InvalidBlock(SystemMessage(laika.tree.Elements.Error, 
            "Error parsing config header: "+ex.getMessage), LiteralBlock("<%"+str+"%>")))
      }
    } 
  }
  
  
  /** Parsers specific to inline elements in text markup.
   */
  trait MarkupSpans extends TemplateParsers with DirectiveParsers.SpanDirectives {
    
    abstract override protected def prepareSpanParsers = {
      
      def addOrMerge (base: Map[Char, Parser[Span]], char: Char, parser: Parser[Span]) = {
        val oldParser = base.get(char)
        base + (char -> oldParser.map(parser | _).getOrElse(parser))
      }
      
      val withRef = addOrMerge(super.prepareSpanParsers, '{', reference(MarkupContextReference(_)))    
      addOrMerge(withRef, '@', spanDirectiveParser)
    }
    
  }
  
  
}