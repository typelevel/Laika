package laika.template

import laika.parse.rst.InlineParsers
import laika.tree.Templates.TemplateRoot
import scala.util.parsing.input.Reader

trait TemplateParsers extends InlineParsers {

  
  def parseTemplate (reader: Reader[Char]) = TemplateRoot(Nil)
  
  
}