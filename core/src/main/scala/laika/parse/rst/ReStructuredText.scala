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

package laika.parse.rst

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders, Theme}
import laika.factory.{ParserFactory, RendererFactory}
import laika.io.Input
import laika.parse.core.combinator.Parsers
import laika.parse.core.markup.DocumentParser
import laika.parse.rst.Elements.FieldList
import laika.parse.rst.ext._
import laika.parse.util.WhitespacePreprocessor
import laika.render.{HTML, HTMLWriter}
import laika.rewrite.{DocumentCursor, TreeUtil}
import laika.tree.Documents.Document
import laika.tree.Elements._
import laika.tree.Paths.Path
  
/** A parser for text written in reStructuredText markup. Instances of this class may be passed directly
 *  to the `Parse` or `Transform` APIs:
 *  
 *  {{{
 *  val document = Parse as ReStructuredText fromFile "hello.rst"
 *  
 *  Transform from ReStructuredText to HTML fromFile "hello.rst" toFile "hello.html"
 *  }}}
 * 
 *  reStructuredText has several types of extension points that are fully supported by Laika.
 *  For more information on how to implement and register those see [[laika.parse.rst.ext.RstExtensionRegistry]].
 *
 *  In addition to the standard reStructuredText directives, the API also supports a custom directive
 *  type unique to Laika. They represent a library-wide extension mechanism and allow you to implement
 *  tags which can be used in any of the supported markup formats or in templates. If you need this
 *  level of flexibility, it is recommended to use the Laika directives, if you want to stay compatible
 *  with the reStructuredText reference parser, you should pick the standard directives.
 *  
 *  Laika directives can be registered with the [[laika.directive.DirectiveRegistry]] extension bundle.
 *  The DSLs for creating directives are similar, but still different,
 *  due to differences in the feature set of the two variants. The Laika directives try to avoid some
 *  of the unnecessary complexities of reStructuredText directives.
 * 
 *  @author Jens Halm
 */
object ReStructuredText extends ParserFactory { self =>


  val fileSuffixes: Set[String] = Set("rest","rst")

  val extensions = Seq(
    new ExtensionBundle {
      override val useInStrictMode: Boolean = true

      override def themeFor[Writer](rendererFactory: RendererFactory[Writer]): Theme[Writer] = rendererFactory match {
        case _: HTML => Theme[HTMLWriter](customRenderers = Seq(ExtendedHTML))
        case _ => Theme[Writer]() // TODO - refactor to return Option instead
      }
    },
    RstExtensionSupport,
    StandardExtensions,
    RawContentExtensions
  )

  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  def newParser (parserExtensions: ParserDefinitionBuilders): Input => Document = input => {
    // TODO - extract this logic once ParserFactory API gets finalized (preProcessInput)
    val raw = input.asParserInput.input
    val preprocessed = (new WhitespacePreprocessor)(raw.toString)

    // TODO - extract this logic into DocumentParser and/or OperationConfig and/or ParserFactory
    val rootParser = new RootParser(parserExtensions.blockParsers, parserExtensions.spanParsers)
    val configHeaderParsers = parserExtensions.configHeaderParsers :+ { _:Path => Parsers.success(Right(ConfigFactory.empty)) }
    val configHeaderParser = { path: Path => configHeaderParsers.map(_(path)).reduce(_ | _) }
    val doc = DocumentParser.forMarkup(rootParser.rootElement, configHeaderParser)(Input.fromString(preprocessed, input.path))

    // TODO - extract this logic once ParserFactory API gets finalized (postProcessDocument)
    def extractDocInfo (config: Config, root: RootElement): Config = {
      import scala.collection.JavaConverters._
      val docStart = root.content dropWhile { case c: Comment => true; case h: DecoratedHeader => true; case _ => false } headOption
      val docInfo = docStart collect { case FieldList(fields,_) => fields map (field => (TreeUtil.extractText(field.name),
        field.content collect { case p: Paragraph => TreeUtil.extractText(p.content) } mkString)) toMap }
      docInfo map (i => config.withValue("docInfo", ConfigValueFactory.fromMap(i.asJava))) getOrElse config
    }
    doc.copy(config = extractDocInfo(doc.config, doc.content))
  }
  
}
