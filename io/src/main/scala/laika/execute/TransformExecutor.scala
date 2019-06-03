package laika.execute

import cats.effect.Async
import cats.implicits._
import laika.io.Parallel.{ParallelParser, ParallelRenderer, ParallelTransformer}
import laika.io.RenderedTreeRoot
import laika.io.Sequential.{SequentialParser, SequentialRenderer, SequentialTransformer}
import laika.io.binary

/**
  *  @author Jens Halm
  */
object TransformExecutor {

  def execute[F[_]: Async] (op: SequentialTransformer.Op[F]): F[String] = for {
    doc <- SequentialParser.Op(op.transformer.parser, op.input).parse
    res <- SequentialRenderer.Op(op.transformer.renderer, doc.content, doc.path, op.output).render
  } yield res

  def execute[F[_]: Async] (op: ParallelTransformer.Op[F]): F[RenderedTreeRoot] = for {
    root <- ParallelParser.Op(op.transformer.parser, op.input).parse
    res  <- ParallelRenderer.Op(op.transformer.renderer, root, op.output).render
  } yield res

  def execute[F[_]: Async] (op: binary.SequentialTransformer.Op[F]): F[Unit] = for {
    doc <- SequentialParser.Op(op.transformer.markupParser, op.input).parse
    res <- binary.SequentialRenderer.Op(op.transformer.renderer, doc.content, doc.path, op.output).render
  } yield res

  def execute[F[_]: Async] (op: binary.ParallelTransformer.Op[F]): F[Unit] = for {
    root <- ParallelParser.Op(op.transformer.markupParser, op.input).parse
    res  <- binary.ParallelRenderer.Op[F](op.transformer.renderer, root, op.output).render
  } yield res


  
}
