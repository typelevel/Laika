/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.api.ext

import com.typesafe.config.{Config, ConfigFactory}
import laika.io.{DefaultDocumentTypeMatcher, DocumentType}
import laika.parse.css.CSSParsers
import laika.rewrite.DocumentCursor
import laika.tree.Elements._
import laika.tree.Paths.Path

/**
  * @author Jens Halm
  */
trait ExtensionBundle { self =>

  /** Base configuration that serves as a fallback for
    * configuration files in the source directories
    * and/or config headers in markup and template documents.
    */
  def baseConfig: Config = ConfigFactory.empty

  /** Specifies the function to use for determining the document type
    * of the input based on its path. Any path for which this function
    * is not defined will be processed by the remaining installed bundles.
    * The documents for paths for which none of the extensions provides
    * a `DocumentType` will be ignored.
    */
  def docTypeMatcher: PartialFunction[Path, DocumentType] = PartialFunction.empty

  /** Specifies extensions and/or replacements for parsers that deal with
    * text markup, templates, CSS or configuration headers.
    */
  def parsers: ParserBundle = ParserBundle()

  def rewriteRules: Seq[DocumentCursor => RewriteRule] = Seq.empty

  def themes: Seq[RenderTheme] = Seq.empty


  // for providing APIs like registering Directives
  def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] = PartialFunction.empty

  def withBase (base: ExtensionBundle): ExtensionBundle = new ExtensionBundle {

    override val useInStrictMode = self.useInStrictMode && base.useInStrictMode

    override lazy val baseConfig = self.baseConfig.withFallback(base.baseConfig)

    override lazy val docTypeMatcher = self.docTypeMatcher.orElse(base.docTypeMatcher)

    override lazy val parsers: ParserBundle = self.parsers withBase base.parsers

    /* flipped on purpose, base rules need to be applied first, so that app rules do not need to deal with potentially
       unknown node types */
    override lazy val rewriteRules = base.rewriteRules ++ self.rewriteRules

    override lazy val themes = self.themes ++ base.themes

    override def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] =
      self.processExtension.orElse(base.processExtension)
  }

  def useInStrictMode: Boolean = false

  def acceptRawContent: Boolean = false

}

object ExtensionBundle {

  object Empty extends ExtensionBundle

  object LaikaDefaults extends ExtensionBundle {

    override val useInStrictMode = true

    override val docTypeMatcher: PartialFunction[Path, DocumentType] = DefaultDocumentTypeMatcher.base

    override val parsers: ParserBundle = ParserBundle(
      styleSheetParser = Some(CSSParsers.styleDeclarationSet)
    )

  }

}
