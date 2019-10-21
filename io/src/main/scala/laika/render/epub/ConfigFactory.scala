package laika.render.epub

import laika.config.Config
import laika.ast.DocumentMetadata
import laika.format.EPUB

/** Creates the EPUB configuration for a document tree.
  *
  * @author Jens Halm
  */
object ConfigFactory {

  /** Creates the EPUB configuration for the specified document tree configuration.
    * It looks for `metadata` and `epub` sections in the config header
    * of the title document or in the `directory.conf` file in the root
    * directory, or uses defaults if both do not exist.
    */
  def forTreeConfig (config: Config): EPUB.Config = {

    val defaults = EPUB.Config.default
    
    val res = for {
      tocDepth   <- config.get[Int]("epub.toc.depth", defaults.tocDepth)
      tocTitle   <- config.getOpt[String]("epub.toc.title")
      coverImage <- config.getOpt[String]("epub.toc.title")
    } yield {
      val metadata = DocumentMetadata.fromConfig(config)
      EPUB.Config(metadata, tocDepth, tocTitle.orElse(defaults.tocTitle), coverImage.orElse(defaults.coverImage))
    }
    
    res.toOption.getOrElse(defaults) // TODO - 0.13 - error handling
  }

}
