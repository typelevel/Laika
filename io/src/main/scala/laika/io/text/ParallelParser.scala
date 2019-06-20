/*
 * Copyright 2012-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.io.text

import cats.data.NonEmptyList
import cats.effect.{Async, ContextShift}
import laika.api.MarkupParser
import laika.api.builder.{OperationConfig, ParserBuilder}
import laika.ast.{DocumentType, TextDocumentType}
import laika.io.model.{ParsedTree, TreeInput}
import laika.io.ops.ParallelInputOps
import laika.runtime.{ParserRuntime, Runtime}

/**
  * @author Jens Halm
  */
class ParallelParser[F[_]: Async: Runtime] (parsers: NonEmptyList[MarkupParser]) extends ParallelInputOps[F] {

  type Result = ParallelParser.Op[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup

  lazy val config: OperationConfig = parsers.map(_.config).reduce[OperationConfig](_ merge _)


  def fromInput (input: F[TreeInput]): ParallelParser.Op[F] = ParallelParser.Op(parsers, input)

}

object ParallelParser {

  case class Builder (parsers: NonEmptyList[MarkupParser]) {

    def or (parser: MarkupParser): Builder = copy(parsers = parsers.append(parser))
    def or (parser: ParserBuilder): Builder = copy(parsers = parsers.append(parser.build))

    def build[F[_]: Async, G[_]](processingContext: ContextShift[F], blockingContext: ContextShift[F], parallelism: Int)
                                (implicit P: cats.Parallel[F, G]): ParallelParser[F] =
      new ParallelParser[F](parsers)(implicitly[Async[F]], Runtime.parallel(processingContext, blockingContext, parallelism))

    def build[F[_]: Async, G[_]](processingContext: ContextShift[F], blockingContext: ContextShift[F])
                                (implicit P: cats.Parallel[F, G]): ParallelParser[F] =
      build(processingContext, blockingContext, java.lang.Runtime.getRuntime.availableProcessors)

  }

  case class Op[F[_]: Async: Runtime] (parsers: NonEmptyList[MarkupParser], input: F[TreeInput]) {

    val parserMap: Map[String, MarkupParser] = parsers.toList.flatMap(p => p.fileSuffixes.map((_, p))).toMap

    lazy val config: OperationConfig = parsers.map(_.config).reduce[OperationConfig](_ merge _)

    def parse: F[ParsedTree] = ParserRuntime.run(this)

  }

}
