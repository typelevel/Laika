package laika.runtime

import cats.data.NonEmptyList
import cats.effect.Async
import cats.implicits._
import laika.io.Parallel.{ParallelParser, ParallelRenderer, ParallelTransformer}
import laika.io.{RenderedTreeRoot, binary}
import laika.io.Sequential.{SequentialParser, SequentialRenderer, SequentialTransformer}

/**
  *  @author Jens Halm
  */
object TransformerRuntime {

  def run[F[_]: Async: Runtime] (op: SequentialTransformer.Op[F]): F[String] = for {
    doc <- SequentialParser.Op(op.transformer.parser, op.input).parse
    res <- SequentialRenderer.Op(op.transformer.renderer, doc.content, doc.path, op.output).render
  } yield res

  def run[F[_]: Async: Runtime] (op: ParallelTransformer.Op[F]): F[RenderedTreeRoot] = for {
    tree <- ParallelParser.Op(NonEmptyList.of(op.transformer.parser), op.input).parse
    res  <- ParallelRenderer.Op(op.transformer.renderer, tree.root, op.output, Async[F].pure(tree.staticDocuments)).render
  } yield res

  def run[F[_]: Async: Runtime] (op: binary.SequentialTransformer.Op[F]): F[Unit] = for {
    doc <- SequentialParser.Op(op.transformer.markupParser, op.input).parse
    res <- binary.SequentialRenderer.Op(op.transformer.renderer, doc.content, doc.path, op.output).render
  } yield res

  def run[F[_]: Async: Runtime] (op: binary.ParallelTransformer.Op[F]): F[Unit] = for {
    tree <- ParallelParser.Op(NonEmptyList.of(op.transformer.markupParser), op.input).parse
    res  <- binary.ParallelRenderer.Op[F](op.transformer.renderer, tree.root, op.output, Async[F].pure(tree.staticDocuments)).render
  } yield res

}
