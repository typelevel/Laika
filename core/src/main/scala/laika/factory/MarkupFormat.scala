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

package laika.factory

import laika.ast.Block
import laika.bundle.{BlockParserBuilder, ExtensionBundle, SpanParserBuilder}
import laika.parse.Parser
import laika.parse.text.TextParsers

/** Responsible for creating parser instances for a specific markup format.
 *  A parser is simply a function of type `Input => Document`.
 *  
 *  @author Jens Halm
 */
trait MarkupFormat {

  /** Short string describing the markup format for tooling and logging.
    */
  def description: String = toString
  
  /** The file suffixes recognized by this parser.
   *  When transforming entire directories only files with
   *  names ending in one of the specified suffixes will
   *  be considered.
   * 
   *  It is recommended not to support `txt`
   *  or similarly common suffixes as this might interfere
   *  with other installed formats.
   */
  def fileSuffixes: Set[String]

  /** All block parsers for the markup language this parser processes.
    */
  def blockParsers: Seq[BlockParserBuilder]

  /** All span parsers for the markup language this parser processes.
    */
  def spanParsers: Seq[SpanParserBuilder]

  /** Parses the character after the one that started the escape sequence (usually a backslash).
    *
    * The default implementation parses any character as is, this can be overridden in case
    * the host language has more specific rules for escape sequences.
    */
  def escapedChar: Parser[String] = TextParsers.any.take(1)

  /** The parser-specific extensions that need to be installed
   *  for each transformation that involves this parser.
   * 
   *  One scenario where a parser needs to provide a bundle
   *  is when it produces tree elements that are unknown
   *  to the built-in rewrite rules and renderers.
   */
  def extensions: Seq[ExtensionBundle]

  /** Creates the parser for a sequence of blocks based on the parser
    * for a single block.
    *
    * The parser for a single block is already the result of merging all block parsers
    * defined within this instance with all extension parsers defined
    * by the user.
    *
    * The default implementation simply applies this parser repeatedly while
    * skipping blank lines between the parsed blocks. This method can get overridden
    * for special requirements, for example when the previous result has an influence on
    * which parser to pick for the subsequent block.
    */
  def createBlockListParser (parser: Parser[Block]): Parser[Seq[Block]] = (parser <~ TextParsers.blankLines.?)*

}
