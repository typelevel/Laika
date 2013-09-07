package laika.template

import laika.parse.InlineParsers
import laika.tree.Templates.TemplateRoot
import scala.util.parsing.input.Reader
import laika.tree.Elements.Span

trait TemplateParsers extends InlineParsers {

  
  lazy val spanParsers = Map[Char, Parser[Span]]()
  
  
  def parseTemplate (reader: Reader[Char]) = TemplateRoot(Nil)
  
  
  
}