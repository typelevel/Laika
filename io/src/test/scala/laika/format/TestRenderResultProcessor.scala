package laika.format

import laika.ast.DocumentTreeRoot
import laika.execute.OutputExecutor
import laika.factory.RenderResultProcessor
import laika.io.{BinaryOutput, RenderedDocument, RenderedTree, RenderedTreeRoot}
import laika.render.TextFormatter

object TestRenderResultProcessor extends RenderResultProcessor[TextFormatter] {

  val format = AST

  def prepareTree (tree: DocumentTreeRoot): DocumentTreeRoot = tree

  def process (result: RenderedTreeRoot, output: BinaryOutput): Unit = {
    
    def append (sb: StringBuilder, result: RenderedTree): Unit = {
      result.content.foreach {
        case d: RenderedDocument => sb.append(d.content + "\n")
        case t: RenderedTree => append(sb, t)
        case _ => ()
      }
    }
    
    val sb = new StringBuilder
    append(sb, result.tree)
    val resultString = sb.toString

    val out = OutputExecutor.asStream(output)
    try {
      out.write(resultString.getBytes("UTF-8"))
    } finally {
      out.close()
    }
    
  }
  
}
