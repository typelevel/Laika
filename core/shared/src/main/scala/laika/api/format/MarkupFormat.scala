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

package laika.api.format

import laika.api.bundle.{ BlockParserBuilder, ExtensionBundle, SpanParserBuilder }
import laika.api.format.MarkupFormat.MarkupParsers
import laika.ast.Block
import laika.parse.Parser
import laika.parse.combinator.Parsers
import laika.parse.text.TextParsers

/** Responsible for creating parser instances for a specific markup format.
  *  A parser is simply a function of type `Input => Document`.
  *
  *  @author Jens Halm
  */
trait MarkupFormat extends Format {

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
  def blockParsers: MarkupParsers[BlockParserBuilder]

  /** All span parsers for the markup language this parser processes.
    */
  def spanParsers: MarkupParsers[SpanParserBuilder]

  /** Parses the character after the one that started the escape sequence (usually a backslash).
    *
    * The default implementation parses any character as is, this can be overridden in case
    * the host language has more specific rules for escape sequences.
    */
  def escapedChar: Parser[String] = TextParsers.oneChar

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
  def createBlockListParser(parser: Parser[Block]): Parser[Seq[Block]] =
    (parser <~ Parsers.opt(TextParsers.blankLines)).rep

}

object MarkupFormat {

  /** API for registering parsers for a specific markup format.
    *
    * The only abstract method is `all` that specifies the list of parsers
    * to register with Laika's parser runtime.
    *
    * But parser authors are encouraged to use this type for also creating
    * public properties for each of their individual parsers so that users
    * can compose them for custom formats or parser extensions.
    *
    * The type of these properties should be either `BlockParserBuilder` or
    * `SpanParserBuilder` which allows to keep all parser implementation details
    * private and only offer high level entry points for extension authors.
    */
  trait MarkupParsers[E] {

    /** List of parsers to register with the runtime for a specific markup format.
      *
      * The order of parsers in this sequence is significant and determines
      * the precedence in which parsers are tried on blocks or spans.
      */
    def all: Seq[E]
  }

}
