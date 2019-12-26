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

package laika.io.runtime

import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.Async
import cats.implicits._
import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast._
import laika.config.Config.IncludeMap
import laika.config.ConfigParser
import laika.io.config.IncludeLoader
import laika.io.model.{ParsedTree, TextInput, TreeInput}
import laika.io.text.{ParallelParser, SequentialParser}
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}

/** Internal runtime for parser operations, for parallel and sequential execution. 
  * 
  *  @author Jens Halm
  */
object ParserRuntime {
  
  /** Run the specified parser operation for a single input,
    * producing a single document.
    */
  def run[F[_]: Async: Runtime] (op: SequentialParser.Op[F]): F[Document] = {
    for {
      parserInput <- InputRuntime.readParserInput(op.input)
      res         <- Async[F].fromEither(op.parser.parse(parserInput))
    } yield res
          
  }
  
  /** Run the specified parser operation for an entire input tree,
    * producing an AST tree.
    */
  def run[F[_]: Async: Runtime] (op: ParallelParser.Op[F]): F[ParsedTree[F]] = {
    
    import DocumentType.{Config => ConfigType, _}
    import TreeResultBuilder._

    def selectParser (path: Path): ValidatedNel[Throwable, MarkupParser] = op.parsers match {
      case NonEmptyList(parser, Nil) => parser.validNel
      case multiple => op.parserMap
        .get(path.suffix)
        .toValidNel(NoMatchingParser(path, multiple.toList.flatMap(_.fileSuffixes).toSet))
    }
      
    def parseAll(inputs: TreeInput[F]): F[ParsedTree[F]] = {
      
      def validateInputPaths: F[Unit] = {
        val duplicates = (inputs.binaryInputs.map(_.path) ++ inputs.textInputs.map(_.path))
          .groupBy(identity)
          .collect { case (path, in) if in.size > 1 => DuplicatePath(path) }
        if (duplicates.isEmpty) Async[F].unit
        else Async[F].raiseError(ParserErrors(duplicates.toSet))
      }

      def parseDocument[D] (doc: TextInput[F], parse: ParserInput => Either[ParserError, D], result: D => ParserResult): F[ParserResult] =
        InputRuntime.readParserInput(doc).flatMap(in => Async[F].fromEither(parse(in).map(result)))
      
      def parseConfig(input: ParserInput): Either[ParserError, ConfigParser] =
        Right(op.config.configProvider.configDocument(input.context.input))
      
      val createOps: Either[Throwable, Vector[F[ParserResult]]] = inputs.textInputs.toVector.map { in => in.docType match {
        case Markup             => selectParser(in.path).map(parser => Vector(parseDocument(in, parser.parseUnresolved, MarkupResult)))
        case Template           => op.templateParser.map(parseDocument(in, _, TemplateResult)).toVector.validNel
        case StyleSheet(format) => Vector(parseDocument(in, op.styleSheetParser, StyleResult(_, format))).validNel
        case ConfigType         => Vector(parseDocument(in, parseConfig, ConfigResult(in.path, _))).validNel
      }}.combineAll.toEither.leftMap(es => ParserErrors(es.toList.toSet))
      
      def rewriteTree (root: DocumentTreeRoot): ParsedTree[F] = { // TODO - 0.13 - move to TreeResultBuilder
        val finalTree = root.rewrite(op.config.rewriteRules)
        val finalRoot = finalTree.copy(staticDocuments = inputs.binaryInputs.map(_.path), sourcePaths = inputs.sourcePaths)
        ParsedTree(finalRoot, inputs.binaryInputs)
      }
      
      def loadIncludes(results: Vector[ParserResult]): F[IncludeMap] = {
        val includes = results.flatMap {
          case ConfigResult(_, config) => config.includes
          case MarkupResult(doc) => doc.config.includes
          case TemplateResult(doc) => doc.config.includes
          case _ => Vector()
        }
        
        IncludeLoader.load(includes)
      } 
      
      for {
        _        <- validateInputPaths
        ops      <- Async[F].fromEither(createOps)
        results  <- Runtime[F].runParallel(ops)
        includes <- loadIncludes(results)
        tree     <- Async[F].fromEither(buildTree(results, op.config.baseConfig, includes).leftMap(ParserError(_, Root)))
      } yield rewriteTree(tree)
    }
    
    op.input.flatMap(parseAll)
    
  }

  case class NoMatchingParser (path: Path, suffixes: Set[String]) extends
    RuntimeException(s"No matching parser available for path: $path - supported suffixes: ${suffixes.mkString(",")}")

  case class DuplicatePath (path: Path, filePaths: Set[String] = Set.empty) extends
    RuntimeException(s"Duplicate path: $path ${filePathMessage(filePaths)}")

  case class ParserErrors (errors: Set[Throwable]) extends
    RuntimeException(s"Multiple errors during parsing: ${errors.map(_.getMessage).mkString(", ")}")

  private def filePathMessage (filePaths: Set[String]): String =
    if (filePaths.isEmpty) "(no matching file paths)"
    else s"with matching file paths: ${filePaths.mkString(", ")}"
}
