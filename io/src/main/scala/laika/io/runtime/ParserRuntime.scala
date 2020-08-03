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

import java.io.File

import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.Sync
import cats.implicits._
import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast._
import laika.config.Config.IncludeMap
import laika.config.ConfigParser
import laika.io.api.TreeParser
import laika.io.config.IncludeHandler
import laika.io.config.IncludeHandler.RequestedInclude
import laika.io.model.{InputTree, ParsedTree, TextInput}
import laika.parse.hocon.{IncludeFile, IncludeResource, ValidStringValue}
import laika.parse.markup.DocumentParser.{InvalidDocuments, ParserError, ParserInput}

/** Internal runtime for parser operations, for parallel and sequential execution. 
  * 
  *  @author Jens Halm
  */
object ParserRuntime {
  
  /** Run the specified parser operation for an entire input tree, producing an AST tree.
    */
  def run[F[_]: Sync: Runtime] (op: TreeParser.Op[F]): F[ParsedTree[F]] = {
    
    import DocumentType.{Config => ConfigType, _}
    import TreeResultBuilder._

    def mergeInputs (userInputs: InputTree[F], themeInputs: InputTree[F]): F[InputTree[F]] = {

      def validateInputPaths(inputs: InputTree[F]): F[Unit] = {
        val duplicates = inputs.allPaths
          .groupBy(identity)
          .collect { case (path, in) if in.size > 1 => DuplicatePath(path) }
        if (duplicates.isEmpty) Sync[F].unit
        else Sync[F].raiseError(ParserErrors(duplicates.toSet))
      }

      def mergedInputs: InputTree[F] = {
        // user inputs override theme inputs
        val userPaths = userInputs.allPaths.toSet
        val filteredThemeInputs = InputTree(
          textInputs = themeInputs.textInputs.filterNot(in => userPaths.contains(in.path)),
          binaryInputs = themeInputs.binaryInputs.filterNot(in => userPaths.contains(in.path)),
          parsedResults = themeInputs.parsedResults.filterNot(in => userPaths.contains(in.path)),
          sourcePaths = themeInputs.sourcePaths
        )
        userInputs ++ filteredThemeInputs
      }

      for {
        _ <- validateInputPaths(userInputs)
        _ <- validateInputPaths(themeInputs)
      } yield mergedInputs
    }
    
    def parseAll(inputs: InputTree[F]): F[ParsedTree[F]] = {

      val parsers = op.parsers.map(p => MarkupParser
        .of(p.format)
        .withConfig(p.config.withBundles(op.theme.extensions))
        .build
      )
      val parserMap: Map[String, MarkupParser] = parsers.toList.flatMap(p => p.fileSuffixes.map((_, p))).toMap

      def selectParser (path: Path): ValidatedNel[Throwable, MarkupParser] = parsers match {
        case NonEmptyList(parser, Nil) => parser.validNel
        case multiple => path.suffix
          .flatMap(parserMap.get)
          .toValidNel(NoMatchingParser(path, multiple.toList.flatMap(_.fileSuffixes).toSet))
      }
      
      def parseDocument[D] (doc: TextInput[F], parse: ParserInput => Either[ParserError, D], result: (D, Option[File]) => ParserResult): F[ParserResult] =
        InputRuntime.readParserInput(doc).flatMap(in => Sync[F].fromEither(parse(in).map(result(_, doc.sourceFile))))
      
      def parseConfig(input: ParserInput): Either[ParserError, ConfigParser] =
        Right(op.config.configProvider.configDocument(input.context.input))
      
      val createOps: Either[Throwable, Vector[F[ParserResult]]] = inputs.textInputs.toVector.map { in => in.docType match {
        case Markup             => selectParser(in.path).map(parser => Vector(parseDocument(in, parser.parseUnresolved, MarkupResult)))
        case Template           => op.templateParser.map(parseDocument(in, _, TemplateResult)).toVector.validNel
        case StyleSheet(format) => Vector(parseDocument(in, op.styleSheetParser, StyleResult(_, format, _))).validNel
        case ConfigType         => Vector(parseDocument(in, parseConfig, HoconResult(in.path, _, _))).validNel
      }}.combineAll.toEither.leftMap(es => ParserErrors(es.toList.toSet))
      
      def rewriteTree (root: DocumentTreeRoot): Either[InvalidDocuments, ParsedTree[F]] = { // TODO - move to TreeResultBuilder
        val finalTree = root.rewrite(op.config.rewriteRulesFor(root))
        val finalRoot = finalTree.copy(staticDocuments = inputs.binaryInputs.map(_.path))
        InvalidDocuments.from(finalRoot, op.config.failOnMessages)
          .toLeft(ParsedTree(finalRoot, inputs.binaryInputs))
      }
      
      def loadIncludes(results: Vector[ParserResult]): F[IncludeMap] = {
        
        def toRequestedInclude(includes: Seq[IncludeResource], sourceFile: Option[File]): Seq[RequestedInclude] =
          includes.map { include => RequestedInclude(include, sourceFile.map(f => IncludeFile(ValidStringValue(f.getPath)))) }
        
        val includes = results.flatMap {
          case HoconResult(_, config, sourceFile) => toRequestedInclude(config.includes, sourceFile)
          case MarkupResult(doc, sourceFile) => toRequestedInclude(doc.config.includes, sourceFile)
          case TemplateResult(doc, sourceFile) => toRequestedInclude(doc.config.includes, sourceFile)
          case _ => Vector()
        }
        
        IncludeHandler.load(includes)
      } 
      
      for {
        ops      <- Sync[F].fromEither(createOps)
        results  <- Runtime[F].runParallel(ops)
        includes <- loadIncludes(results)
        tree     <- Sync[F].fromEither(buildTree(results ++ inputs.parsedResults, op.config.baseConfig, includes).leftMap(ParserError(_, Root)))
        result   <- Sync[F].fromEither(rewriteTree(tree))
      } yield result
    }
    
    for {
      userInputs   <- op.input.build(op.config.docTypeMatcher)
      themeInputs  <- op.theme.inputs
      allInputs    <- mergeInputs(userInputs, themeInputs)
      result       <- parseAll(allInputs)
    } yield result
    
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
