package laika.io.helper

import laika.ast.{TemplateRoot, TemplateString}
import laika.render.{FOTemplate, HTMLTemplate}
import laika.render.epub.{HtmlTemplate => EPUBTemplate}

object RenderResult {
  
  private def buildResult (template: TemplateRoot, insertions: Seq[String]): String = {
    val it = insertions.iterator
    template.content.map {
      case TemplateString(content, _) => content
      case _ => it.next
    }.mkString
  }
    

  object html {
    def withDefaultTemplate(title: String, content: String): String = buildResult(HTMLTemplate.default, Seq(title, content))
  }

  object epub {
    def withDefaultTemplate(title: String, content: String): String = buildResult(EPUBTemplate.default, Seq(title, "", content))
  }
  
  object fo {
    def withDefaultTemplate(content: String): String = buildResult(FOTemplate.default, Seq("", "", content))
    def withDefaultTemplate(result: String, bookmarks: String = ""): String = buildResult(FOTemplate.default, Seq(bookmarks, "", result))
  }
  
}
