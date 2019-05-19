package laika.io.helper

import laika.ast.Path.Root
import laika.execute.InputExecutor

object RenderResult {

  object html {
    
    private lazy val defaultTemplate = InputExecutor.classPathParserInput("/templates/default.template.html", Root / "default.template.html").context.input
    
    def withDefaultTemplate(title: String, content: String): String =
      defaultTemplate.replace("{{document.title}}", title).replace("{{document.content}}", content)
    
  }

  object epub {

    private lazy val defaultTemplate = InputExecutor.classPathParserInput("/templates/default.template.epub.xhtml", Root / "default.template.epub.xhtml").context.input

    def withDefaultTemplate(title: String, content: String): String =
      defaultTemplate.replace("{{document.title}}", title).replace("{{document.content}}", content).replace("@:styleLinks.", "")

  }
  
  object fo {
    
    private lazy val defaultTemplate = InputExecutor.classPathParserInput("/templates/default.template.fo", Root / "default.template.fo").context.input
    
    def withDefaultTemplate(content: String): String =
      defaultTemplate.replace("{{document.content}}", content).replace("{{document.fragments.bookmarks}}", "").replaceAll("(?s)@.*  }", "")

  }
  
}
