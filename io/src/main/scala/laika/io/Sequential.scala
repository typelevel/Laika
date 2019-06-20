package laika.io

import laika.api.builder._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.factory.BinaryPostProcessor

object Sequential {

  def apply (parser: ParserBuilder): text.SequentialParser.Builder = text.SequentialParser.Builder(parser.build)

  def apply (renderer: RendererBuilder[_]): text.SequentialRenderer.Builder = text.SequentialRenderer.Builder(renderer.build)

  def apply (transformer: TransformerBuilder[_]): text.SequentialTransformer.Builder = text.SequentialTransformer.Builder(transformer.build)

  def apply (renderer: TwoPhaseRendererBuilder[_, BinaryPostProcessor]): binary.SequentialRenderer.Builder = binary.SequentialRenderer.Builder(renderer.build)

  def apply (transformer: TwoPhaseTransformerBuilder[_, BinaryPostProcessor]): binary.SequentialTransformer.Builder = binary.SequentialTransformer.Builder(transformer.build)

  def apply (parser: MarkupParser): text.SequentialParser.Builder = text.SequentialParser.Builder(parser)

  def apply (renderer: Renderer): text.SequentialRenderer.Builder = text.SequentialRenderer.Builder(renderer)

  def apply (transformer: Transformer): text.SequentialTransformer.Builder = text.SequentialTransformer.Builder(transformer)

  def apply (renderer: TwoPhaseRenderer[BinaryPostProcessor]): binary.SequentialRenderer.Builder = binary.SequentialRenderer.Builder(renderer)

  def apply (transformer: TwoPhaseTransformer[BinaryPostProcessor]): binary.SequentialTransformer.Builder = binary.SequentialTransformer.Builder(transformer)

}  
