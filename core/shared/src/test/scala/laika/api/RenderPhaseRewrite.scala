package laika.api

import cats.syntax.all._
import laika.api.builder.OperationConfig
import laika.ast.{ Document, RewritePhase }
import laika.factory.RenderFormat
import laika.parse.markup.DocumentParser.RendererError

trait RenderPhaseRewrite {

  def rewrite(format: RenderFormat[_])(doc: Document): Either[RendererError, Document] =
    rewrite(OperationConfig.default, format)(doc)

  def rewrite(parser: MarkupParser, format: RenderFormat[_])(
      doc: Document
  ): Either[RendererError, Document] =
    rewrite(parser.config, format)(doc)

  def rewrite(config: OperationConfig, format: RenderFormat[_])(
      doc: Document
  ): Either[RendererError, Document] = {
    config
      .rewriteRulesFor(doc, RewritePhase.Render(format))
      .flatMap(doc.rewrite)
      .leftMap(RendererError(_, doc.path))
  }

}
