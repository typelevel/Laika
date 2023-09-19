/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.io.internal.runtime

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.effect.{ Async, Sync }
import cats.syntax.all.*
import laika.api.MarkupParser
import laika.api.errors.{ InvalidDocuments, ParserError }
import laika.ast.Path.Root
import laika.ast.*
import laika.api.config.Config.IncludeMap
import laika.api.config.{ ConfigBuilder, ConfigParser }
import laika.config.LinkValidation
import laika.io.api.TreeParser
import laika.io.internal.config.IncludeHandler
import laika.io.internal.config.IncludeHandler.RequestedInclude
import laika.io.model.{ FilePath, InputTree, ParsedTree, TextInput }
import laika.parse.hocon.{ IncludeFile, IncludeResource, ValidStringValue }
import laika.internal.parse.markup.DocumentParser.DocumentInput
import laika.io.internal.errors.{
  DocumentParserError,
  DuplicatePath,
  NoMatchingParser,
  ParserErrors
}

/** Internal runtime for parser operations, for parallel and sequential execution.
  *
  *  @author Jens Halm
  */
private[io] object ParserRuntime {

  import DocumentTreeBuilder._

  /** Run the specified parser operation for an entire input tree, producing an AST tree.
    */
  def run[F[_]: Async: Batch](op: TreeParser.Op[F]): F[ParsedTree[F]] = {

    import DocumentType.{ Config => ConfigType, _ }

    def mergeInputs(userInputs: InputTree[F], themeInputs: InputTree[F]): F[InputTree[F]] = {

      def validateInputPaths(inputs: InputTree[F]): F[Unit] = {
        val duplicates = inputs.allPaths
          .groupBy(identity)
          .collect { case (path, in) if in.size > 1 => DuplicatePath(path) }
        if (duplicates.isEmpty) Sync[F].unit
        else Sync[F].raiseError(ParserErrors(duplicates.toSet))
      }

      for {
        _ <- validateInputPaths(userInputs)
        _ <- validateInputPaths(themeInputs)
      } yield themeInputs.overrideWith(userInputs)
    }

    def parseAll(inputs: InputTree[F]): F[ParsedTree[F]] = {

      val parsers                              = op.parsers.map(p =>
        MarkupParser
          .of(p.format)
          .withConfig(p.config.withBundles(op.theme.extensions))
          .build
      )
      val parserMap: Map[String, MarkupParser] =
        parsers.toList.flatMap(p => p.fileSuffixes.map((_, p))).toMap

      def selectParser(path: Path): ValidatedNel[Throwable, MarkupParser] = parsers match {
        case NonEmptyList(parser, Nil) => parser.validNel
        case multiple                  =>
          path.suffix
            .flatMap(parserMap.get)
            .toValidNel(NoMatchingParser(path, multiple.toList.flatMap(_.fileSuffixes).toSet))
      }

      def parseDocument[D](
          doc: TextInput[F],
          parse: DocumentInput => Either[ParserError, D],
          result: D => BuilderPart
      ): F[ParserResult] =
        doc.asDocumentInput.flatMap(in =>
          Sync[F].fromEither(
            parse(in)
              .map(result)
              .leftMap(e => DocumentParserError(e.message, doc.path))
              .map(ParserResult(_, doc.sourceFile))
          )
        )

      def parseConfig(input: DocumentInput): Either[ParserError, ConfigParser] =
        Right(op.config.configProvider.configDocument(input.source.input))

      val createOps: Either[Throwable, Vector[F[ParserResult]]] = inputs.textInputs.toVector.map {
        in =>
          in.docType match {
            case Markup             =>
              selectParser(in.path).map(parser =>
                Vector(parseDocument(in, parser.parseUnresolved, MarkupPart.apply))
              )
            case Template           =>
              op.templateParser.map(parseDocument(in, _, TemplatePart.apply)).toVector.validNel
            case StyleSheet(format) =>
              Vector(parseDocument(in, op.styleSheetParser, StylePart(_, format))).validNel
            case ConfigType         =>
              Vector(parseDocument(in, parseConfig, HoconPart(in.path, _))).validNel
          }
      }.combineAll.toEither.leftMap(es => ParserErrors(es.toList.toSet))

      def rewriteTree(root: DocumentTreeRoot): Either[InvalidDocuments, ParsedTree[F]] = {
        val parsedTree = ParsedTree(root)
          .addStaticDocuments(inputs.binaryInputs)
          .modifyRoot(_.addStaticDocuments(inputs.providedPaths))
        val finalTree  = for {
          phase1 <- parsedTree.root.rewrite(
            op.config.rewriteRulesFor(parsedTree.root, RewritePhase.Build)
          )
          result <- phase1.rewrite(op.config.rewriteRulesFor(phase1, RewritePhase.Resolve))
        } yield result
        InvalidDocuments
          .from(finalTree, op.config.messageFilters.failOn)
          .map(tree => parsedTree.modifyRoot(_ => tree))
      }

      def loadIncludes(results: Vector[ParserResult]): F[IncludeMap] = {

        val includes = results.flatMap { res =>
          res.includes.map { include =>
            RequestedInclude(
              include,
              res.sourceFile.map(f => IncludeFile(ValidStringValue(f.toString)))
            )
          }
        }

        IncludeHandler.load(includes)
      }

      val globalFallbackConfig = ConfigBuilder.empty.withValue(LinkValidation.Global()).build
      val baseConfig           = op.config.baseConfig.withFallback(globalFallbackConfig)

      def buildTree(
          parsedResults: Seq[ParserResult],
          includes: IncludeMap
      ): Either[DocumentParserError, DocumentTreeRoot] = {
        val allResults = parsedResults.map(_.treePart) ++ inputs.treeBuilder.distinctParts
        new DocumentTreeBuilder(allResults.toList)
          .resolveAndBuildRoot(baseConfig, includes)
          .leftMap(DocumentParserError(_, Root))
      }

      for {
        ops      <- Sync[F].fromEither(createOps)
        results  <- Batch[F].execute(ops)
        includes <- loadIncludes(results)
        tree     <- Sync[F].fromEither(buildTree(results, includes))
        result   <- Sync[F].fromEither(rewriteTree(tree))
      } yield result
    }

    for {
      userInputs <- op.input.build(op.config.docTypeMatcher)
      allInputs  <- mergeInputs(userInputs, op.theme.inputs)
      result     <- parseAll(allInputs)
    } yield result

  }

  private case class ParserResult(treePart: BuilderPart, sourceFile: Option[FilePath]) {

    def includes: Seq[IncludeResource] = treePart match {
      case HoconPart(_, config) => config.includes
      case MarkupPart(doc)      => doc.config.includes
      case TemplatePart(doc)    => doc.config.includes
      case _                    => Vector()
    }

  }

}
