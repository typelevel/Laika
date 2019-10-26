/*
 * Copyright 2013-2016 the original author or authors.
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
import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast.helper.ModelBuilder
import laika.bundle.BundleProvider
import laika.config.Origin.{DocumentScope, TreeScope}
import laika.config.{ConfigValue, Field, LongValue, ObjectValue, Origin}
import laika.format.{Markdown, ReStructuredText}
import laika.io.implicits._
import laika.io.model.TreeInput
import laika.io.helper.InputBuilder
import laika.rewrite.TemplateRewriter
import laika.runtime.TestContexts._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext

class ConfigSpec extends FlatSpec 
                    with Matchers
                    with ModelBuilder {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  trait Inputs extends InputBuilder  {
      
    val mdMatcher = MarkupParser.of(Markdown).config.docTypeMatcher
    val rstMatcher = MarkupParser.of(ReStructuredText).config.docTypeMatcher
      
    def builder (in: Seq[(Path, String)], docTypeMatcher: Path => DocumentType): TreeInput[IO] = build(in, docTypeMatcher)
    
    object Contents {

      val templateWithRef =
        """<h1>${foo}</h1>
          |<div>${document.content}</div>
          |CCC""".stripMargin

      val templateWithMissingRef =
        """<h1>${config.foox}</h1>
          |<div>${document.content}</div>
          |CCC""".stripMargin

      val templateWithOptRef =
        """<h1>${?config.foox}</h1>
          |<div>${document.content}</div>
          |CCC""".stripMargin

      val templateWithConfig =
        """{% foo: bar %}
          |<div>${document.content}</div>
          |CCC""".stripMargin

      val templateWithoutConfig =
        """<div>${document.content}</div>
          |CCC""".stripMargin

      val markupWithConfig =
        """{% foo: bar %}
          |aaa
          |bbb""".stripMargin

      val markupWithRef =
        """aaa
          |${config.foo}
          |bbb""".stripMargin
      
      val configDoc = """foo = bar"""

      val markupWithMergeableConfig =
        """{% foo.bar: 7 %}
          |${foo.bar}
          |${foo.baz}""".stripMargin
      
      val mergeableConfig = """{ foo.baz = 9 }"""
    }
    
    def resultOf (root: DocumentTreeRoot): RootElement = {
      val result = TemplateRewriter.applyTemplates(root, "html").right.get.tree
      result.content.collect{case doc: Document => doc}.head.content
    }
    
  }
  
  val markdownParser = MarkupParser.of(Markdown).io(blocker).parallel[IO].build
  
  val rstParser = MarkupParser.of(ReStructuredText).io(blocker).parallel[IO].build
  
  "The Config parser" should "parse configuration sections embedded in Markdown documents" in new Inputs {
    val inputs = Seq(
      Root / "default.template.html" -> Contents.templateWithRef,
      Root / "input.md" -> Contents.markupWithConfig
    )
    val expected = root(
      TemplateRoot(List(
        TemplateString("<h1>"),
        TemplateString("bar"),
        TemplateString("</h1>\n<div>"),
        eRoot(p("aaa\nbbb")),
        TemplateString("</div>\nCCC")
      ))
    )
    resultOf(markdownParser.fromInput(IO.pure(builder(inputs, mdMatcher))).parse.unsafeRunSync().root) should be (expected)
  }
  
  it should "parse configuration sections embedded in reStructuredText documents" in new Inputs {
    val inputs = Seq(
      Root / "default.template.html" -> Contents.templateWithRef,
      Root / "input.rst" -> Contents.markupWithConfig
    )
    val expected = root(
      TemplateRoot(List(
        TemplateString("<h1>"),
        TemplateString("bar"),
        TemplateString("</h1>\n<div>"),
        eRoot(p("aaa\nbbb")),
        TemplateString("</div>\nCCC")
      ))
    )
    resultOf(rstParser.fromInput(IO.pure(builder(inputs, rstMatcher))).parse.unsafeRunSync().root) should be (expected)
  }

  it should "insert an invalid element when a required context reference is missing" in new Inputs {
    val inputs = Seq(
      Root / "default.template.html" -> Contents.templateWithMissingRef,
      Root / "input.rst" -> Contents.markupWithConfig
    )
    val expected = root(
      TemplateRoot(List(
        TemplateString("<h1>"),
        InvalidElement(SystemMessage(MessageLevel.Error, "Missing required reference: 'config.foox'"), "${config.foox}").asTemplateSpan,
        TemplateString("</h1>\n<div>"),
        eRoot(p("aaa\nbbb")),
        TemplateString("</div>\nCCC")
      ))
    )
    resultOf(rstParser.fromInput(IO.pure(builder(inputs, rstMatcher))).parse.unsafeRunSync().root) should be (expected)
  }

  it should "insert an empty string when an optional context reference is missing" in new Inputs {
    val inputs = Seq(
      Root / "default.template.html" -> Contents.templateWithOptRef,
      Root / "input.rst" -> Contents.markupWithConfig
    )
    val expected = root(
      TemplateRoot(List(
        TemplateString("<h1>"),
        TemplateString(""),
        TemplateString("</h1>\n<div>"),
        eRoot(p("aaa\nbbb")),
        TemplateString("</div>\nCCC")
      ))
    )
    resultOf(rstParser.fromInput(IO.pure(builder(inputs, rstMatcher))).parse.unsafeRunSync().root) should be (expected)
  }

  it should "make directory configuration available for references in markup" in new Inputs {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.configDoc,
      Root / "default.template.html" -> Contents.templateWithoutConfig,
      Root / "input.md" -> Contents.markupWithRef
    )
    val expected = root(
      TemplateRoot(List(
        TemplateString("<div>"),
        eRoot(p(txt("aaa\nbar\nbbb"))),
        TemplateString("</div>\nCCC")
      ))
    )
    resultOf(markdownParser.fromInput(IO.pure(builder(inputs, mdMatcher))).parse.unsafeRunSync().root) should be (expected)
  }

  it should "merge objects from config headers in markup with objects in directory configuration" in new Inputs {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.mergeableConfig,
      Root / "default.template.html" -> Contents.templateWithoutConfig,
      Root / "input.md" -> Contents.markupWithMergeableConfig
    )
    val tree = markdownParser.fromInput(IO.pure(builder(inputs, mdMatcher))).parse.unsafeRunSync()
    val doc = tree.root.tree.content.head.asInstanceOf[Document]
    doc.config.get[ConfigValue]("foo").right.get.asInstanceOf[ObjectValue].values.sortBy(_.key) should be (Seq(
      Field("bar", LongValue(7), Origin(DocumentScope, Root / "input.md")),
      Field("baz", LongValue(9), Origin(TreeScope, Root / "directory.conf"))
    ))
  }

  it should "make directory configuration available for references in templates" in new Inputs {
    val inputs = Seq(
      Root / "directory.conf" -> Contents.configDoc,
      Root / "default.template.html" -> Contents.templateWithRef,
      Root / "input.rst" -> "txt"
    )
    val expected = root(
      TemplateRoot(List(
        TemplateString("<h1>"),
        TemplateString("bar"),
        TemplateString("</h1>\n<div>"),
        eRoot(p(txt("txt"))),
        TemplateString("</div>\nCCC")
      ))
    )
    resultOf(rstParser.fromInput(IO.pure(builder(inputs, rstMatcher))).parse.unsafeRunSync().root) should be (expected)
  }
  
  it should "merge configuration found in documents, templates, directories and programmatic setup" in new Inputs {
    
    val template = """{% key2: val2 %}
      |${config.key1}
      |${config.key2}
      |${config.key3}
      |${config.key4}
      |${config.key5}""".stripMargin

    val md = """{% key1: val1 %}
               |aaa""".stripMargin
    
    val config3 = "key3: val3"
    val config4 = "key4: val4"
    val config5 = "key5: val5"

    val inputs = Seq(
      Root / "directory.conf"                -> config4,
      Root / "dir" / "default.template.html" -> template,
      Root / "dir" / "directory.conf"        -> config3,
      Root / "dir" / "input.md"              -> md,
    )
      
    val expected = root(
      TemplateRoot(
        (1 to 5) map (n => List(TemplateString("val"+n))) reduce (_ ++ List(TemplateString("\n")) ++ _)
      )
    )
    
    val op = MarkupParser.of(Markdown).using(BundleProvider.forConfigString(config5)).io(blocker).parallel[IO].build
      .fromInput(IO.pure(builder(inputs, mdMatcher)))
    val result = TemplateRewriter.applyTemplates(op.parse.unsafeRunSync().root, "html").right.get.tree
    result.selectDocument(Path.Current / "dir" / "input.md").get.content should be (expected)
  }
  
}
