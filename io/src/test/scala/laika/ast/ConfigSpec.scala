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

package laika.ast

import cats.effect._
import cats.syntax.all._
import laika.api.MarkupParser
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.sample.TestSourceBuilders
import laika.bundle.BundleProvider
import laika.config.Origin.{DocumentScope, TreeScope}
import laika.config._
import laika.format.{HTML, Markdown, ReStructuredText}
import laika.io.FileIO
import laika.io.helper.{InputBuilder, TestThemeBuilder}
import laika.io.implicits._
import laika.io.model.{FilePath, InputTreeBuilder, ParsedTree}
import laika.rewrite.{DefaultTemplatePath, OutputContext, TemplateRewriter}
import munit.CatsEffectSuite


class ConfigSpec extends CatsEffectSuite
  with FileIO
  with TestSourceBuilders
  with InputBuilder {


  object Contents {

    val templateWithRef =
      """<h1>${foo}</h1>
        |<div>${cursor.currentDocument.content}</div>
        |CCC""".stripMargin

    val templateWithMissingRef =
      """<h1>${foox}</h1>
        |<div>${cursor.currentDocument.content}</div>
        |CCC""".stripMargin

    val templateWithOptRef =
      """<h1>${?foox}</h1>
        |<div>${cursor.currentDocument.content}</div>
        |CCC""".stripMargin

    val templateWithConfig =
      """{% foo: bar %}
        |<div>${cursor.currentDocument.content}</div>
        |CCC""".stripMargin

    val templateWithoutConfig =
      """<div>${cursor.currentDocument.content}</div>
        |CCC""".stripMargin

    val markupWithConfig =
      """{% foo: bar %}
        |aaa
        |bbb""".stripMargin

    val markupWithConfigAfterWhitespace =
      """
        |
        |  {% foo: bar %}
        |aaa
        |bbb""".stripMargin

    val markupWithPathConfig =
      """{% foo: ../foo.txt %}
        |aaa
        |bbb""".stripMargin

    val markupWithArrayConfig =
      """{% foo: [a,b,c] %}
        |aaa
        |bbb""".stripMargin

    val markupWithRef =
      """aaa
        |${foo}
        |bbb""".stripMargin

    val markupWithRefs =
      """aaa
        |${a}
        |${b}
        |${c}
        |bbb""".stripMargin
    
    val configDoc = """foo = bar"""
    
    val configWithCpInclude =
      """
        |a = 1
        |
        |include classpath("/config/b.conf")""".stripMargin

    def configWithFileInclude(fileName: String): String =
      s"""
        |a = 1
        |
        |include file("$fileName")""".stripMargin

    val configDocWithPath = """foo = ../foo.txt"""

    val markupWithMergeableConfig =
      """{% foo.bar: 7 %}
        |${foo.bar}
        |${foo.baz}""".stripMargin
    
    val mergeableConfig = """{ foo.baz = 9 }"""
  }
  
  def toResult (parsed: ParsedTree[IO]): IO[RootElement] = 
    IO.fromEither(resultDoc(parsed.root).map(_.content).leftMap(e => ConfigException(e)))

  def resultDoc (root: DocumentTreeRoot): Either[ConfigError, Document] =
    resultTree(root).map(_.content.collect { case doc: Document => doc }.head)

  def resultTree (root: DocumentTreeRoot): Either[ConfigError, DocumentTree] =
    TemplateRewriter.applyTemplates(root, OperationConfig.default.rewriteRulesFor(root, RewritePhase.Render(HTML)), OutputContext(HTML)).map(_.tree)
    
  implicit class ConfigResultOps[A] (res: Either[ConfigError, A]) {
    def toIO: IO[A] = IO.fromEither(res.leftMap(e => ConfigException(e)))
  }
  
  private val markdownParser = MarkupParser.of(Markdown).parallel[IO].build

  private val rstParser = MarkupParser.of(ReStructuredText).parallel[IO].build

  def parseMD (input: InputTreeBuilder[IO]): IO[RootElement] = parseMDTree(input).flatMap(toResult)
  def parseMDTree (input: InputTreeBuilder[IO]): IO[ParsedTree[IO]] = markdownParser.use { p =>
    p.fromInput(input).parse
  }
  def parseRST (input: InputTreeBuilder[IO]): IO[RootElement] = rstParser.use { p =>
    p.fromInput(input).parse.flatMap(toResult)
  }
  
  def result(span: TemplateSpan): RootElement = RootElement(
    TemplateRoot(
      TemplateString("<h1>"),
      span,
      TemplateString("</h1>\n<div>"),
      EmbeddedRoot("aaa\nbbb"),
      TemplateString("</div>\nCCC")
    )
  )

  test("parse configuration sections embedded in Markdown documents") {
    val inputs = Seq(
      Root / "default.template.html" -> Contents.templateWithRef,
      Root / "input.md" -> Contents.markupWithConfig
    )
    val expected = result(TemplateString("bar"))
    parseMD(build(inputs)).assertEquals(expected)
  }

  test("parse configuration sections embedded in Markdown documents after blank lines and whitespace") {
    val inputs = Seq(
      Root / "default.template.html" -> Contents.templateWithRef,
      Root / "input.md" -> Contents.markupWithConfigAfterWhitespace
    )
    val expected = result(TemplateString("bar"))
    parseMD(build(inputs)).assertEquals(expected)
  }

  test("parse configuration sections embedded in reStructuredText documents") {
    val inputs = Seq(
      DefaultTemplatePath.forHTML -> Contents.templateWithRef,
      Root / "input.rst" -> Contents.markupWithConfig
    )
    val expected = result(TemplateString("bar"))
    parseRST(build(inputs)).assertEquals(expected)
  }

  test("insert an invalid element when a required context reference is missing") {
    val inputs = Seq(
      DefaultTemplatePath.forHTML -> Contents.templateWithMissingRef,
      Root / "input.rst" -> Contents.markupWithConfig
    )
    val msg = RuntimeMessage(MessageLevel.Error, "Missing required reference: 'foox'")
    val src = source("${foox}", Contents.templateWithMissingRef, DefaultTemplatePath.forHTML)
    val expected = result(TemplateElement(InvalidSpan(msg, src)))
    parseRST(build(inputs)).assertEquals(expected)
  }

  test("insert an empty string when an optional context reference is missing") {
    val inputs = Seq(
      DefaultTemplatePath.forHTML -> Contents.templateWithOptRef,
      Root / "input.rst" -> Contents.markupWithConfig
    )
    val expected = result(TemplateString(""))
    parseRST(build(inputs)).assertEquals(expected)
  }

  test("make directory configuration available for references in markup") {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.configDoc,
      DefaultTemplatePath.forHTML -> Contents.templateWithoutConfig,
      Root / "input.md" -> Contents.markupWithRef
    )
    val expected = RootElement(
      TemplateRoot(
        TemplateString("<div>"),
        EmbeddedRoot("aaa\nbar\nbbb"),
        TemplateString("</div>\nCCC")
      )
    )
    parseMD(build(inputs)).assertEquals(expected)
  }

  test("include classpath resources in directory configuration") {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.configWithCpInclude,
      DefaultTemplatePath.forHTML -> Contents.templateWithoutConfig,
      Root / "input.md" -> Contents.markupWithRefs
    )
    val expected = RootElement(
      TemplateRoot(
        TemplateString("<div>"),
        EmbeddedRoot("aaa\n1\n2\n3\nbbb"),
        TemplateString("</div>\nCCC")
      )
    )
    parseMD(build(inputs)).assertEquals(expected)
  }

  test("include file resources in directory configuration") {
    def inputs(file: FilePath) = Seq(
      Root / "directory.conf" -> Contents.configWithFileInclude(file.toString),
      DefaultTemplatePath.forHTML -> Contents.templateWithoutConfig,
      Root / "input.md" -> Contents.markupWithRefs
    )
    val expected = RootElement(
      TemplateRoot(
        TemplateString("<div>"),
        EmbeddedRoot("aaa\n1\n2\n3\nbbb"),
        TemplateString("</div>\nCCC")
      )
    )
    val bConf =
      """include "c.conf" 
        |
        |b = 2
      """.stripMargin
    
    val res = for {
      tempDir <- newTempDirectory
      conf    =  tempDir / "b.conf"
      _       <- writeFile(conf, bConf)
      _       <- writeFile(tempDir / "c.conf", "c = 3")
      res     <- parseMD(build(inputs(conf)))
    } yield res
    
    res.assertEquals(expected)
  }

  test("merge objects from config headers in markup with objects in directory configuration") {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.mergeableConfig,
      DefaultTemplatePath.forHTML -> Contents.templateWithoutConfig,
      Root / "input.md" -> Contents.markupWithMergeableConfig
    )
    val expected = Seq(
      Field("bar", LongValue(7), Origin(DocumentScope, Root / "input.md")),
      Field("baz", LongValue(9), Origin(TreeScope, Root / "directory.conf"))
    )
    parseMDTree(build(inputs))
      .map { tree => 
        val doc = tree.root.tree.content.head.asInstanceOf[Document]
        doc.config.get[ConfigValue]("foo").collectFirst {
          case ov: ObjectValue => ov.values.sortBy(_.key)
        }
      }
      .assertEquals(Some(expected))
  }

  test("decode merged objects as a Map") {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.mergeableConfig,
      DefaultTemplatePath.forHTML -> Contents.templateWithoutConfig,
      Root / "input.md" -> Contents.markupWithMergeableConfig
    )
    val expected = Seq(
      ("bar", 7),
      ("baz", 9)
    )
    parseMDTree(build(inputs))
      .flatMap { tree =>
        val doc = tree.root.tree.content.head.asInstanceOf[Document]
        doc.config.get[Map[String, Int]]("foo").map(_.toSeq.sortBy(_._1)).toIO
      }
      .assertEquals(expected)
  }

  test("make directory configuration available for references in templates") {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.configDoc,
      DefaultTemplatePath.forHTML -> Contents.templateWithRef,
      Root / "input.rst" -> "txt"
    )
    val expected = RootElement(
      TemplateRoot(
        TemplateString("<h1>"),
        TemplateString("bar"),
        TemplateString("</h1>\n<div>"),
        EmbeddedRoot("txt"),
        TemplateString("</div>\nCCC")
      )
    )
    parseRST(build(inputs)).assertEquals(expected)
  }

  test("merge configuration found in documents, templates, directories, programmatic setup, bundles, themes and theme extensions") {

    val template =
      """{% key2: val2 %}
        |${key1}
        |${key2}
        |${key3}
        |${key4}
        |${key5}
        |${key6}
        |${key7}""".stripMargin

    val md =
      """{% key1: val1 %}
        |aaa""".stripMargin

    val config3 = "key3: val3"
    val config4 = "key4: val4"
    val config5 = "key5: val5"
    val config6 = "key6: val6"
    val config7 = "key7: val7"

    val inputs = Seq(
      Root / "directory.conf" -> config4,
      Root / "dir" / DefaultTemplatePath.forHTML.relative -> template,
      Root / "dir" / "directory.conf" -> config3,
      Root / "dir" / "input.md" -> md,
    )

    val expected = RootElement(
      TemplateRoot(
        (1 to 7) map (n => List(TemplateString("val" + n))) reduce (_ ++ List(TemplateString("\n")) ++ _)
      )
    )
    
    val theme = TestThemeBuilder
      .forBundle(BundleProvider.forConfigString(config6))
      .extendWith(TestThemeBuilder
        .forBundle(BundleProvider.forConfigString(config7)))

    MarkupParser
      .of(Markdown)
      .using(BundleProvider.forConfigString(config5))
      .parallel[IO]
      .withTheme(theme)
      .build
      .use { p =>
        p.fromInput(build(inputs)).parse
      }
      .flatMap(p => resultTree(p.root).toIO)
      .map { _.selectDocument(RelativePath.CurrentTree / "dir" / "input.md").map(_.content) }
      .assertEquals(Some(expected))
  }

  test("decode a path in a document config header") {
    val inputs = Seq(
      Root / "dir" / "input.md" -> Contents.markupWithPathConfig
    )

    parseMDTree(build(inputs))
      .flatMap(p => resultTree(p.root).toIO)
      .map { tree =>
        tree
          .selectDocument(RelativePath.CurrentTree / "dir" / "input.md")
          .map(_.config.get[Path]("foo"))
      }
      .assertEquals(Some(Right(Root / "foo.txt")))
  }

  test("decode a local path in a document config header") {
    val markupWithPathConfig =
      """{% foo: "#ref" %}
        |aaa
        |bbb""".stripMargin
    val inputs = Seq(
      Root / "dir" / "input.md" -> markupWithPathConfig
    )

    parseMDTree(build(inputs))
      .flatMap(p => resultTree(p.root).toIO)
      .map { tree =>
        tree
          .selectDocument(RelativePath.CurrentTree / "dir" / "input.md")
          .map(_.config.get[Path]("foo"))
      }
      .assertEquals(Some(Right(Root / "dir" / "input.md#ref")))
  }

  test("decode a path in a directory config file in a nested directory") {
    val inputs = Seq(
      Root / "dir" / "directory.conf" -> Contents.configDocWithPath
    )

    parseMDTree(build(inputs))
      .flatMap(p => resultTree(p.root).toIO)
      .map { tree =>
        tree
          .selectSubtree(RelativePath.CurrentTree / "dir")
          .map(_.config.get[Path]("foo"))
      }
      .assertEquals(Some(Right(Root / "foo.txt")))
    
  }

  test("decode an array element in a document config header") {
    val inputs = Seq(
      Root / "dir" / "input.md" -> Contents.markupWithArrayConfig
    )

    parseMDTree(build(inputs))
      .flatMap(p => resultTree(p.root).toIO)
      .map { tree =>
        tree
          .selectDocument(RelativePath.CurrentTree / "dir" / "input.md")
          .map(_.config.get[String]("foo.2"))
      }
      .assertEquals(Some(Right("c")))
  }
  
}
