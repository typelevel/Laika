package local

import java.io.PrintWriter
import scala.io.Codec
import laika.api.Parse
import laika.api.Render
import laika.parse.rst.ReStructuredText
import laika.render.PrettyPrint
import laika.tree.Elements._
import laika.tree.RewriteRules


class TempTest {
  
  val path = "/Users/jenshalm/Projects/Planet42/Laika/src/test/resources/rst-spec."
    
  def tidy (s: String) = s
  
  def readFile (name: String) = name
  
  implicit val codec = Codec.UTF8
  

  def writeFile (name: String, content: String) {
    val out = new PrintWriter(name,"UTF-8")
    try out.print(content) finally out.close
  }
  
  def invalid = {
    val doc = Parse as ReStructuredText fromFile (path+"rst")
    val spans = (doc.content collect {
      case i @ InvalidSpan(_,_,_) => i
    }).distinct
    val blocks = (doc.content collect {
      case i @ InvalidBlock(_,_,_) => i
    }).distinct
    val invDoc = RootElement(Paragraph(spans) +: blocks)
    Render as PrettyPrint from invDoc toFile (path+"invalid")
  }
  
  def footnotes = {
    val doc = (Parse as ReStructuredText asRawDocument) fromFile (path+"rst")
    val targets = doc.content collect {
      case f: FootnoteDefinition => f
    }
    val refs = doc.content collect {
      case f: FootnoteReference => f
    }
    //val resolved = doc.content.rewrite(RewriteRules chain (doc.rewriteRules ::: List(RewriteRules(doc))))
    val resolved = doc
    val footnotes = resolved.content collect {
      case f: Footnote => f
      case i: InvalidBlock => i
      case d: FootnoteDefinition => d
    }
    val links = resolved.content collect {
      case f: FootnoteLink => f
    }
    val fDoc = RootElement(Paragraph(refs) +: targets ++: Paragraph(links) +: footnotes)
    Render as PrettyPrint from fDoc toFile (path+"footnotes")
  }
  
  def extDefs = {
    val doc = (Parse as ReStructuredText asRawDocument) fromFile (path+"rst")
    val ext = doc.content collect {
      case i @ ExternalLinkDefinition(id,_,_,_) => Text(id)
    }
    val extDoc = RootElement(List(Paragraph(ext)))
    Render as PrettyPrint from extDoc toFile (path+"ext")
  }
  
  def rawTree = {
    val doc = (Parse as ReStructuredText asRawDocument) fromFile (path+"rst")
    Render as PrettyPrint from doc.content toFile (path+"rawTree")
    "done"
  }
  
  
}