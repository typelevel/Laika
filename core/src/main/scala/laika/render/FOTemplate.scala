package laika.render

import laika.ast._

/** The default template for HTML renderers.
  *
  * @author Jens Halm
  */
object FOTemplate {

  private val templateText = """<?xml version="1.0" encoding="utf-8"?>
                               |
                               |<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:fox="http://xmlgraphics.apache.org/fop/extensions">
                               |
                               |  <fo:layout-master-set>
                               |  
                               |    <fo:simple-page-master 
                               |        master-name="default"
                               |        page-height="29.7cm"
                               |        page-width="21cm"
                               |        margin-top="1cm"
                               |        margin-bottom="1cm"
                               |        margin-left="2.5cm"
                               |        margin-right="2.5cm">
                               |      <fo:region-body margin-top="2cm" margin-bottom="2cm"/>
                               |      <fo:region-before extent="3cm"/>
                               |      <fo:region-after extent="1cm"/>
                               |    </fo:simple-page-master>
                               |    
                               |  </fo:layout-master-set>
                               |
                               |  #
                               |
                               |  #
                               |
                               |  <fo:page-sequence master-reference="default">
                               |
                               |    <fo:static-content flow-name="xsl-region-before">
                               |      <fo:block border-bottom-width="1pt" border-bottom-style="solid" 
                               |          font-weight="bold" font-size="9pt" text-align="center">
                               |        <fo:retrieve-marker 
                               |            retrieve-class-name="chapter"
                               |            retrieve-position="first-including-carryover"
                               |        />
                               |      </fo:block>
                               |    </fo:static-content>
                               |    
                               |    <fo:static-content flow-name="xsl-region-after">
                               |      <fo:block height="100%" font-weight="bold" font-size="10pt" text-align="center">
                               |        <fo:page-number/>
                               |      </fo:block>
                               |    </fo:static-content>
                               |      
                               |    <fo:flow flow-name="xsl-region-body">
                               |
                               |      #
                               |
                               |    </fo:flow>
                               |    
                               |  </fo:page-sequence>
                               |  
                               |</fo:root>
                               |""".stripMargin

  // TODO - 0.12 - temporary duplication of the styleLink directive until the directive impl has been rewritten
  case object CoverImage extends SpanResolver with TemplateSpan {

    type Self = this.type
    def withOptions (options: Options): this.type = this
    val options = NoOpt
    
    private val coverImagePath = "pdf.coverImage"

    def resolve (cursor: DocumentCursor): Span = {
      val config = cursor.root.target.config
      val path = if (config.hasPath(coverImagePath)) Some(config.getString(coverImagePath)) else None
      path.fold[TemplateSpan](TemplateSpanSequence(Nil)) { coverPath =>
        val fo = s"""    <fox:external-document src="$coverPath"
                    |      width="21cm" height="29.7cm" content-width="21cm"/>""".stripMargin
        TemplateString(fo)
      }
    }
  }
  
  /** The default template for PDF and XSL-FO renderers.
    *
    * It can be overridden by placing a custom template document
    * with the name `default.template.fo` into the root directory
    * of the input files. Alternatively the default can also be overridden
    * for individual sub-directories with a corresponding file with the same name.
    */
  val default: TemplateRoot = {
    val templateSpans = templateText.split("#").map(TemplateString(_))
    TemplateRoot(Seq(
      templateSpans(0),
      TemplateContextReference("document.fragments.bookmarks"),
      templateSpans(1),
      CoverImage,
      templateSpans(2),
      TemplateContextReference("document.content"),
      templateSpans(3)
    ))
  }
  
}
