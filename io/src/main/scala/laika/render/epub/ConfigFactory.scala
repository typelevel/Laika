package laika.render.epub

import com.typesafe.config.Config
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

    def getOpt [T](key: String, read: String => T): Option[T] =
      if (config.hasPath(key)) Some(read(key)) else None

    val tocDepth = getOpt("epub.toc.depth", config.getInt).getOrElse(defaults.tocDepth)
    val tocTitle = getOpt("epub.toc.title", config.getString).orElse(defaults.tocTitle)
    val coverImage = getOpt("epub.coverImage", config.getString).orElse(defaults.coverImage)

    val metadata = DocumentMetadata.fromConfig(config)

    EPUB.Config(metadata, tocDepth, tocTitle, coverImage)
  }

}
