package laika.render.epub

/** Holds static content for EPUB metadata files that do not depend on the input.
  *
  * @author Jens Halm
  */
object StaticContent {

  /** The content of the `mimetype` file in the EPUB root directory.
    */
  val mimeType = "application/epub+zip"

  /** The content of the file `container.xml` in the `META-INF` folder of the EPUB container.
    */
  val container = """<?xml version="1.0" encoding="UTF-8"?>
    |<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
    |  <rootfiles>
    |    <rootfile full-path="EPUB/content.opf" media-type="application/oebps-package+xml" />
    |  </rootfiles>
    |</container>""".stripMargin

  /** iBook configuration options.
    */
  val iBooksOptions = """<?xml version="1.0" encoding="UTF-8"?>
   |<display_options>
   |  <platform name="*">
   |    <option name="specified-fonts">true</option>
   |  </platform>
   |</display_options>""".stripMargin

  /** Minimal fallback CSS that gets used in the EPUB container
    * if no CSS files are present in the input tree of the
    * Transform operation.
    */
  val fallbackStyles =
    """body { margin: 5%; text-align: justify; font-size: medium; }
      |code { font-family: monospace; }
      |ol.toc { padding: 0; list-style: none; }
      |ol.toc a {
      |  display: block;
      |}
      |ol.toc li { list-style-type: none; margin: 0; padding: 0; }
      |ol.toc > li > a {
      |  padding-top: 20px;
      |  font-size: 21px;
      |}
      |ol.toc > li > ol > li > a {
      |  padding-top: 20px;
      |  font-size: 16px;
      |}
      |ol.toc > li > ol > li > ol > li > a {
      |  padding-top: 10px;
      |  padding-bottom: 0;
      |  font-size: 13px;
      |}
      |a.footnote-ref { vertical-align: super; }
    """.stripMargin


}
