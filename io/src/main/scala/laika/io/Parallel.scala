package laika.io

import cats.data.NonEmptyList
import laika.api.builder._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.factory.BinaryPostProcessor

object Parallel {

  def apply (parser: ParserBuilder): text.ParallelParser.Builder                   = text.ParallelParser.Builder(NonEmptyList.of(parser.build))
  def apply (renderer: RendererBuilder[_]): text.ParallelRenderer.Builder          = text.ParallelRenderer.Builder(renderer.build)
  def apply (transformer: TransformerBuilder[_]): text.ParallelTransformer.Builder = text.ParallelTransformer.Builder(transformer.build)

  def apply (renderer: TwoPhaseRendererBuilder[_, BinaryPostProcessor]): binary.ParallelRenderer.Builder          = binary.ParallelRenderer.Builder(renderer.build)
  def apply (transformer: TwoPhaseTransformerBuilder[_, BinaryPostProcessor]): binary.ParallelTransformer.Builder = binary.ParallelTransformer.Builder(transformer.build)

  def apply (parser: MarkupParser): text.ParallelParser.Builder           = text.ParallelParser.Builder(NonEmptyList.of(parser))
  def apply (renderer: Renderer): text.ParallelRenderer.Builder           = text.ParallelRenderer.Builder(renderer)
  def apply (transformer: Transformer): text.ParallelTransformer.Builder  = text.ParallelTransformer.Builder(transformer)

  def apply (renderer: TwoPhaseRenderer[BinaryPostProcessor]): binary.ParallelRenderer.Builder           = binary.ParallelRenderer.Builder(renderer)
  def apply (transformer: TwoPhaseTransformer[BinaryPostProcessor]): binary.ParallelTransformer.Builder  = binary.ParallelTransformer.Builder(transformer)

}
