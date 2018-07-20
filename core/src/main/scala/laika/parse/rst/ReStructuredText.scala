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

import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders, RootParserHooks, Theme}
import laika.factory.{ParserFactory, RendererFactory}
import laika.parse.core.markup.RootParserBase
import laika.parse.rst.ext._
import laika.parse.util.WhitespacePreprocessor
import laika.render.{HTML, HTMLWriter}
  
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

      override val parserDefinitions: ParserDefinitionBuilders = ParserDefinitionBuilders(
        rootParserHooks = Some(RootParserHooks(
          preProcessInput = WhitespacePreprocessor.forInput,
          postProcessDocument = DocInfoExtractor
        ))
      )

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
  def newRootParser (parserExtensions: ParserDefinitionBuilders): RootParserBase =
    new RootParser(parserExtensions.blockParsers, parserExtensions.spanParsers)
  
}
