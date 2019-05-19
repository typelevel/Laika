package laika.render.epub

/** Lists the supported file types that get inserted into the EPUB container
  * in addition to the dynamically rendered HTML content.
  *
  * @author Jens Halm
  */
object MimeTypes {

  /** Maps files suffixes to mime types.
    */
  val supportedTypes: Map[String, String] = Map(
    "jpg"   -> "image/jpeg",
    "jpeg"  -> "image/jpeg",
    "gif"   -> "image/gif",
    "png"   -> "image/png",
    "svg"   -> "image/svg+xml",
    "mp3"   -> "audio/mpeg",
    "mp4"   -> "audio/mp4",
    "html"  -> "application/xhtml+xml",
    "xhtml" -> "application/xhtml+xml",
    "js"    -> "application/javascript",
    "css"   -> "text/css",
    "woff2" -> "font/woff2",
    "woff"  -> "application/font-woff",
    "ttf"   -> "application/font-sfnt",
    "otf"   -> "application/font-sfnt",
  )

}
