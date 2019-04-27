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

package laika.api

import laika.ast.DocumentType.Markup
import laika.ast.{Document, DocumentTree, TextDocumentType}
import laika.config.{OperationConfig, ParseConfigBuilder}
import laika.execute.ParseExecutor
import laika.factory.MarkupParser
import laika.io.{InputOps, InputTreeOps, TextInput, TreeInput}

/** API for performing a parse operation from various types of input to obtain
 *  a document tree without a subsequent render operation. 
 *  
 *  In cases where a render operation should follow immediately, it is more 
 *  convenient to use the [[laika.api.Transform]] API instead which 
 *  combines a parse and a render operation directly.
 *  
 *  Example for parsing Markdown from a file:
 *  
 *  {{{
 *  val doc = Parse as Markdown fromFile "hello.md"
 *  }}}
 *  
 *  Example for parsing from an entire directory:
 *  
 *  {{{
 *  val tree = Parse as Markdown fromDirectory "path/to/source"
 *  }}}
 *  
 *  Example for parsing a directory that contains markup documents in different formats:
 *  
 *  {{{
 *  val tree = Parse as Markdown or ReStructuredText fromDirectory "path/to/source"
 *  }}}
 * 
 *  @author Jens Halm
 */
class Parse private (parsers: Seq[MarkupParser], val config: OperationConfig, rewrite: Boolean)
  extends ParseConfigBuilder with InputOps with InputTreeOps {

  val docType: TextDocumentType = Markup
  
  type ThisType = Parse

  type InputResult = Parse.Op

  type InputTreeResult = Parse.TreeOp

  def withConfig(newConfig: OperationConfig): ThisType = new Parse(parsers, newConfig, rewrite)

  /** The file suffixes recognized by this parser.
   *  When transforming entire directories only files with
   *  names ending in one of the specified suffixes will
   *  be consired. 
   */
  val fileSuffixes: Set[String] = parsers flatMap (_.fileSuffixes) toSet
  
  /** Returns a new Parse instance adding the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText.
   *  
   *  This method is useful if you want to combine different markup
   *  formats within a single document tree. 
   * 
   *  @param parser the parser factory to add to the previously specified parsers
   */
  def or (parser: MarkupParser): Parse = new Parse(parsers :+ parser, config.withBundlesFor(parser), rewrite)

  /** Returns a new Parse instance that produces raw document trees without applying
   *  the default rewrite rules. These rules resolve link and image references and 
   *  rearrange the tree into a hierarchy of sections based on the (flat) sequence
   *  of header instances found in the document.
   */
  def withoutRewrite: Parse = new Parse(parsers, config, rewrite = false)

  def fromInput (input: TextInput): Parse.Op = Parse.Op(parsers, config, input, rewrite)
  
  def fromTreeInput(input: TreeInput): Parse.TreeOp = Parse.TreeOp(parsers, config, input, rewrite)
  
}

/** Serves as an entry point to the Parse API.
 * 
 *  @author Jens Halm
 */
object Parse {
  
  case class Op (parsers: Seq[MarkupParser], config: OperationConfig, input: TextInput, rewrite: Boolean) {
    def execute: Document = ParseExecutor.execute(this)
  }
  
  case class TreeOp (parsers: Seq[MarkupParser], config: OperationConfig, input: TreeInput, rewrite: Boolean) {
    def execute: DocumentTree = ParseExecutor.execute(this)
  }
  
  
  /** Returns a new Parse instance for the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText. 
   * 
   *  @param parser the parser factory to use for all subsequent operations
   */
  def as (parser: MarkupParser): Parse = new Parse(
    Seq(parser),
    OperationConfig.default.withBundlesFor(parser),
    rewrite = true
  )

}
