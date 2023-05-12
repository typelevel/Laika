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

package laika.directive.std

import cats.syntax.all._
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast._
import laika.config.{ Config, ConfigParser, ValidationError }
import laika.directive.DirectiveSupport
import laika.format.HTML
import laika.parse.SourceCursor
import laika.parse.combinator.Parsers
import laika.rewrite.TemplateRewriter

/** @author Jens Halm
  */
trait TemplateParserSetup {

  lazy val templateParser = StandardDirectives
    .processExtension(DirectiveSupport)
    .parsers
    .templateParser
    .getOrElse(Parsers.failure("template parser not defined"))

  def parseTemplate(input: String): Either[String, TemplateRoot] =
    templateParser.parse(SourceCursor(input)).toEither

  def parseAndRewriteTemplate(input: String): Either[String, RootElement] =
    parseTemplateWithConfig(input, Config.empty)

  def parseTemplateWithConfig(input: String, config: String): Either[String, RootElement] =
    ConfigParser.parse(config).resolve().leftMap(_.message).flatMap(
      parseTemplateWithConfig(input, _)
    )

  def parseTemplateWithConfig(input: String, config: Config): Either[String, RootElement] = {

    def rewriteTemplate(tRoot: TemplateRoot): Either[String, RootElement] = {
      val docPath  = Root / "docs" / "doc1.md"
      val template = TemplateDocument(Path.Root / "theme" / "test.template.html", tRoot)
      val doc      = Document(docPath, RootElement.empty, config = config)
      val root  = DocumentTreeRoot(DocumentTree(Root, Seq(DocumentTree(Root / "docs", Seq(doc)))))
      val rules = OperationConfig.default.rewriteRulesFor(root, RewritePhase.Render(HTML))
      val res   = for {
        cursor <- RootCursor(root).flatMap(
          _.allDocuments.find(_.path == docPath).toRight(
            ValidationError("cursor under test missing")
          )
        )
        result <- TemplateRewriter.applyTemplate(cursor, rules, template)
      } yield result.content
      res.leftMap(_.message)
    }

    for {
      tRoot <- parseTemplate(input)
      res   <- rewriteTemplate(tRoot)
    } yield res
  }

}
