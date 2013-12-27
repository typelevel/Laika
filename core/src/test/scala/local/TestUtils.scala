package local

import laika.tree.Elements.Element

object TestUtils {

  
  object DebugHelper {
    // TODO - move to separate helper object
    import laika.api.Render
    import laika.render.PrettyPrint

    def writeFiles (result: Element, expected: Element) = {
      val path = "/Users/jenshalm/Projects/Planet42/Laika/target/"
      Render as PrettyPrint from result toFile (path+"result.txt")
      Render as PrettyPrint from expected toFile (path+"expected.txt")
    }
  }
    
    
}