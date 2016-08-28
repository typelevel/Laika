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

package laika.factory

import laika.io.Input
import laika.tree.Documents.Document
import laika.tree.Documents.DocumentContext
import laika.tree.Elements.RewriteRule

/** Responsible for creating parser instances for a specific markup format.
 *  A parser is simply a function of type `Input => Document`.
 *  
 *  @author Jens Halm
 */
trait ParserFactory {
  
  
  /** The file suffixes recognized by this parser.
   *  When transforming entire directories only files with
   *  names ending in one of the specified suffixes will
   *  be consired. 
   * 
   *  It is recommended not to support `txt`
   *  or similarly common suffixes as this might interfere
   *  with other installed formats.
   */
  def fileSuffixes: Set[String]
  
  /** The parser-specific rewrite rules that need to be
   *  applied by default before rendering any document
   *  produced by this parser.
   * 
   *  Providing additional rewrite rules is only necessary
   *  if the parser produces tree elements that require
   *  rewriting and are unknown to the default rewrite rules.
   */
  def rewriteRules: Seq[DocumentContext => RewriteRule]

  /**  Creates a new parser instance.
   *   Such an instance is expected to be stateless and thread-safe,
   *   thus capable of repeated and parallel executions.
   */
  def newParser: Input => Document
  

}
