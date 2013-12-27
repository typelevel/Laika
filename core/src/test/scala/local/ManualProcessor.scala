package local

import laika.api.Transform
import laika.parse.markdown.Markdown
import laika.render.HTML
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import scala.io.Codec
import laika.tree.Elements._
import java.io.PrintWriter
import laika.api.Parse
import laika.api.Render
import laika.render.PrettyPrint
import laika.io.InputProvider.Directory
import laika.template.ParseTemplate
import laika.template.DefaultTemplate
import scala.io.Source

object ManualProcessor {

  
  case class OrphanedBulletListItems(items: Seq[BulletListItem]) extends Element
  
  object Navigation {
    import laika.tree.Documents._
    import laika.tree.Templates._
    import laika.directive.Directives.Templates
    import Templates.Combinators._
    
    val tocMock = Templates.create("laika-nav") {
      context map { context =>
        TemplateString("foo")
      }
    }
      
    val directive = Templates.create("laika-nav") {
      context map { context =>
      
        val format = StringBullet("*")
    
        def isCurrent (doc: Document) = doc.path == context.document.path
        
        /*
          <li class="nav-header">Introduction</li>
          <li class="active"><a href="#">Overview</a></li>
          <li><a href="architecture.html">Architecture</a></li>
                    
          <li class="nav-header">Using Laika</li>
          <li><a href="../using-laika/basics.html#top">Transformation Basics</a></li>
          <li><a href="../using-laika/markup.html#parse">Supported Markup</a></li>
          
          <li><a href="../using-laika/structure.html#render">Document Structure</a></li>
         */
        
        def title (title: Seq[Span]) = {
          title match {
            case Seq(Text("Laika", _)) => Seq(Text("Overview"))
            case Seq(Text("Implementing a Directive", _)) => Seq(Text("Directives"))
            case Seq(Text("Implementing a Parser", _)) => Seq(Text("Parsers"))
            case Seq(Text("Implementing a Renderer", _)) => Seq(Text("Renderers"))
            case other => other
          }
        }
        
        def buildDocLinks (docs: Seq[Document]): List[BulletListItem] = {
          (for (doc <- docs) yield {
            if (isCurrent(doc)) BulletListItem(List(Paragraph(List(ExternalLink(title(doc.title), "#")))), format, Styles("active"))
            else BulletListItem(List(Paragraph(List(CrossLink(title(doc.title), "", PathInfo(doc.path, doc.path.relativeTo(context.parent.path)))))), format)
          }).toList
        }
        
        def buildNavSection (tree: DocumentTree): List[BulletListItem] = {
          BulletListItem(Seq(Paragraph(tree.title)), format, Styles("nav-header")) :: 
            buildDocLinks(tree.documents)
        }
        
        val items = (for (tree <- context.root.subtrees if tree.documents.nonEmpty) yield {
          buildNavSection(tree)
        }).flatten
        
        TemplateElement(OrphanedBulletListItems(items))
      
      }
    }
  }
  
  
  def process (): Unit = {
    
    val projectDir = "/Users/jenshalm/Projects/"
    val srcDir = projectDir + "Planet42/Laika/docs/"
    val outDir = projectDir + "Planet42/LaikaDocs/"
    
    
    implicit val codec: Codec = Codec.UTF8

    val start = System.currentTimeMillis()
    
    (Transform from Markdown to HTML rendering { out => { 
      case RootElement(content)     => out <<  "<div>" <<|> content <<| "</div>"
      case LiteralBlock(content, _) => out <<  "<pre>" <<<& content <<  "</pre>" 
      case Header(1, content, _)    => out << """<div class="page-header">""" <<| "  <h1>" << content << "</h1>" <<| "</div>"
      case OrphanedBulletListItems(items) => out <<| items
    }} fromDirectory srcDir).inParallel withTemplateDirectives(Navigation.directive) toDirectory outDir
    
    val end = System.currentTimeMillis()
    println("Finished transformation in "+(end-start)+" ms")
  }
  
  def tmp (): Unit = {
    
    val projectDir = "/Users/jenshalm/Projects/"
    val rootDir = projectDir + "Planet42/Laika/docs/tmp"
    
    
    implicit val codec: Codec = Codec.UTF8

    
    val t = (Transform from Markdown to HTML rendering { out => { 
      case RootElement(content)     => out <<  "<div>" <<|> content <<| "</div>"
      case LiteralBlock(content, _) => out <<  "<pre>" <<<& content <<  "</pre>" 
      case Header(1, content, _)    => out << """<div class="page-header">""" <<| "  <h1>" << content << "</h1>" <<| "</div>"
      case OrphanedBulletListItems(items) => out <<| items
    }})

    t withRootDirectory rootDir

    println("done")
    /*
    val s = Source.fromFile(rootDir+"/source/intro.md")
    val in = s.getLines.mkString("/n")
    s.close()

    println("start")
    val start = System.currentTimeMillis()
    
    for (_ <- 1 to 500) { t fromString (in) toString }
    
    val end = System.currentTimeMillis()
    println("Finished transformation in "+(end-start)+" ms")
    
    val start2 = System.currentTimeMillis()
    
    for (_ <- 1 to 500) { t fromString (in) toString }
    
    val end2 = System.currentTimeMillis()
    println("Finished transformation in "+(end2-start2)+" ms")
    * 
    */
  }
  
  def speed (): Unit = {
    
    val projectDir = "/Users/jenshalm/Projects/"
    val rootDir = projectDir + "Planet42/Laika/docs/tmp"
    
    
    implicit val codec: Codec = Codec.UTF8

    val start = System.currentTimeMillis()
    
    (Transform from Markdown to HTML rendering { out => { 
      case RootElement(content)     => out <<  "<div>" <<|> content <<| "</div>"
      case LiteralBlock(content, _) => out <<  "<pre>" <<<& content <<  "</pre>" 
      case Header(1, content, _)    => out << """<div class="page-header">""" <<| "  <h1>" << content << "</h1>" <<| "</div>"
      case OrphanedBulletListItems(items) => out <<| items
    }}) withRootDirectory rootDir
    
    val end = System.currentTimeMillis()
    println("Finished transformation in "+(end-start)+" ms")
  }
  
  def debug (): Unit = {
    
    val projectDir = "/Users/jenshalm/Projects/"
    val srcDir = projectDir + "Planet42/Laika/docs/"
    val outDir = projectDir + "Planet42/LaikaDocs2/debug/"
    
    
    implicit val codec: Codec = Codec.UTF8
    
    
    val tree = Parse as Markdown fromTree Directory(srcDir).withTemplates(ParseTemplate as DefaultTemplate.withDirectives(Navigation.tocMock))
    
    val linksAndErrors = (for {
      subtree <- tree.subtrees
      doc <- subtree.documents
    } yield {
      doc.content collect { 
        case cl: CrossLink => cl
        case is: InvalidSpan => is
      }
    }).flatten
    
    Render as PrettyPrint from SpanSequence(linksAndErrors) toFile outDir+"debug.txt"
    
    println("done")
    
  }
  
}

      //
