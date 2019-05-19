package laika.render.epub

import laika.ast._
import laika.render.HTMLFormatter

/** Customizations of the default HTML renderer for AST elements where attributes
  * specific to EPUB need to be rendered.
  *
  *  @author Jens Halm
  */
object HtmlRenderExtensions {

  val all: PartialFunction[(HTMLFormatter, Element), String] = {
    case (fmt, CitationLink(ref,label,opt)) => fmt.textElement("a", opt + Styles("citation"), "[" + label + "]", "href" -> ("#"+ref), "epub:type" -> "noteref")
    case (fmt, FootnoteLink(ref,label,opt)) => fmt.textElement("a", opt + Styles("footnote"), "[" + label + "]", "href" -> ("#"+ref), "epub:type" -> "noteref")
    case (fmt, Citation(_,content,opt)) => fmt.indentedElement("aside", opt + Styles("citation"), content, "epub:type" -> "footnote")
    case (fmt, Footnote(_,content,opt)) => fmt.indentedElement("aside", opt + Styles("footnote"), content, "epub:type" -> "footnote")
  }

}
