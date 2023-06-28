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

package laika.io.runtime

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.effect.{ Async, Sync }
import cats.implicits._
import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast._
import laika.config.Config.IncludeMap
import laika.config.ConfigParser
import laika.io.api.TreeParser
import laika.io.config.IncludeHandler
import laika.io.config.IncludeHandler.RequestedInclude
import laika.io.model.{ FilePath, InputTree, ParsedTree, TextInput }
import laika.parse.hocon.{ IncludeFile, IncludeResource, ValidStringValue }
import laika.parse.markup.DocumentParser.{ DocumentInput, InvalidDocuments, ParserError }

/** Internal runtime for parser operations, for parallel and sequential execution.
  *
  *  @author Jens Halm
  */
object ParserRuntime {

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
          Sync[F].fromEither(parse(in).map(result).map(ParserResult(_, doc.sourceFile)))
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
        val rootToRewrite = root.copy(
          staticDocuments = inputs.binaryInputs.map(doc =>
            StaticDocument(doc.path, doc.formats)
          ) ++ inputs.providedPaths
        )
        val finalTree     = for {
          phase1 <- rootToRewrite.rewrite(
            op.config.rewriteRulesFor(rootToRewrite, RewritePhase.Build)
          )
          result <- phase1.rewrite(op.config.rewriteRulesFor(phase1, RewritePhase.Resolve))
        } yield result
        InvalidDocuments
          .from(finalTree, op.config.failOnMessages)
          .map(ParsedTree(_, inputs.binaryInputs))
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

      def allResults(parsedResults: Seq[ParserResult]): Seq[BuilderPart] = {
        // TODO - compatibility mode - remove in 1.x
        parsedResults.map(_.treePart) ++ inputs.parsedResults.flatMap {
          case res: TreeResultBuilder.DocumentResult => Some(DocumentPart(res.doc))
          case res: TreeResultBuilder.ConfigResult   => Some(ConfigPart(res.path, res.config))
          case res: TreeResultBuilder.TemplateResult => Some(TemplatePart(res.doc))
          case res: TreeResultBuilder.StyleResult    => Some(StylePart(res.doc, res.format))
          case _                                     => None
        }
      }

      def buildTree(
          parsedResults: Seq[ParserResult],
          includes: IncludeMap
      ): Either[ParserError, DocumentTreeRoot] =
        new DocumentTreeBuilder(allResults(parsedResults).toList)
          .resolveAndBuildRoot(op.config.baseConfig, includes)
          .leftMap(ParserError(_, Root))

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

  case class ParserResult(treePart: BuilderPart, sourceFile: Option[FilePath]) {

    def includes: Seq[IncludeResource] = treePart match {
      case HoconPart(_, config) => config.includes
      case MarkupPart(doc)      => doc.config.includes
      case TemplatePart(doc)    => doc.config.includes
      case _                    => Vector()
    }

  }

  case class NoMatchingParser(path: Path, suffixes: Set[String]) extends RuntimeException(
        s"No matching parser available for path: $path - supported suffixes: ${suffixes.mkString(",")}"
      )

  case class DuplicatePath(path: Path, filePaths: Set[String] = Set.empty) extends RuntimeException(
        s"Duplicate path: $path ${filePathMessage(filePaths)}"
      )

  case class MissingDirectory(path: FilePath) extends RuntimeException(
        s"Path does not exist or is not a directory: ${path.toString}"
      )

  case class ParserErrors(errors: Set[Throwable]) extends RuntimeException(
        s"Multiple errors during parsing: ${errors.map(_.getMessage).mkString(", ")}"
      )

  private def filePathMessage(filePaths: Set[String]): String =
    if (filePaths.isEmpty) "(no matching file paths)"
    else s"with matching file paths: ${filePaths.mkString(", ")}"

}
