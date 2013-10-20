package laika.template

import laika.parse.InlineParsers
import laika.tree.Templates.TemplateRoot
import scala.util.parsing.input.Reader
import laika.tree.Elements._
import laika.directive.DirectiveParsers
import laika.tree.Templates.TemplateSpan
import laika.tree.Templates.ContextReference

trait TemplateParsers extends InlineParsers {

  
  lazy val reference: Parser[TemplateSpan] = {
    
    val refName = { // TODO - promote to inline parsers
      val alphanum = anyIn('0' to '9', 'a' to 'z', 'A' to 'Z') min 1
      val symbol = anyOf('-', '_', '.', ':', '+') take 1
      
      alphanum ~ ((symbol ~ alphanum)*) ^^ { 
        case start ~ rest => start + (rest map { case a~b => a+b }).mkString
      }
    }
      
    '{' ~ ws ~> refName <~ ws ~ "}}" ^^ {ContextReference(_)}  
    
  }
  
  
}


object TemplateParsers {
  
  
  trait Templates extends TemplateParsers with DirectiveParsers.TemplateDirectives {
    
    protected def prepareSpanParsers = Map(
      '{' -> (reference),    
      '@' -> (templateDirectiveParser),
      '\\'-> ((any take 1) ^^ { Text(_) })
    )
  
    
    def parseTemplate (reader: Reader[Char]) = {
      val parser = spans(any, spanParsers) ^^ {_.collect{ case s:TemplateSpan => s }}
      TemplateRoot(parseMarkup(parser, reader))
    }
      
  }
  
  
  trait MarkupBlocks extends TemplateParsers with DirectiveParsers.BlockDirectives {
    
    abstract override protected def prepareBlockParsers (nested: Boolean) = 
      blockDirectiveParser :: super.prepareBlockParsers(nested)
    
  }
  
  
  trait MarkupSpans extends TemplateParsers with DirectiveParsers.SpanDirectives {
    
    abstract override protected def prepareSpanParsers = {
      
      def addOrMerge (base: Map[Char, Parser[Span]], char: Char, parser: Parser[Span]) = {
        val oldParser = base.get(char)
        base + (char -> oldParser.map(parser | _).getOrElse(parser))
      }
      
      val withRef = addOrMerge(super.prepareSpanParsers, '{', reference)    
      addOrMerge(withRef, '@', spanDirectiveParser)
    }
    
  }
  
  
}