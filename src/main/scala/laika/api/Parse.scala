/*
 * Copyright 2013 the original author or authors.
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

import java.io.Closeable
import java.io.InputStream
import java.io.Reader
import scala.io.Codec
import laika.io.IO
import laika.io.Input
import laika.tree.RewriteRules
import java.io.File
import laika.io.InputProvider
import laika.tree.Documents.Document
import laika.tree.Documents.DocumentTree
  
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
 *  @author Jens Halm
 */
class Parse private (parse: Input => Document, rewrite: Boolean) {

  /** Returns a new Parse instance that produces raw document trees without applying
   *  the default rewrite rules. These rules resolve link and image references and 
   *  rearrange the tree into a hierarchy of sections based on the (flat) sequence
   *  of header instances found in the document.
   */
  def asRawDocument = new Parse(parse, false)
  
  /** Returns a document tree obtained from parsing the specified string.
   *  Any kind of input is valid, including an empty string. 
   */
  def fromString (str: String) = fromInput(Input.fromString(str))
  
  /** Returns a document tree obtained from parsing the input from the specified reader.
   */
  def fromReader (reader: Reader) = fromInput(Input.fromReader(reader))

  /** Returns a document tree obtained from parsing the file with the specified name.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param name the name of the file to parse
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (name: String)(implicit codec: Codec) = fromInput(Input.fromFile(name)(codec))
  
  /** Returns a document tree obtained from parsing the specified file.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param file the file to use as input
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File)(implicit codec: Codec) = fromInput(Input.fromFile(file)(codec))
  
  /** Returns a document tree obtained from parsing the input from the specified stream.
   * 
   *  @param stream the stream to use as input for the parser
   *  @param codec the character encoding of the stream, if not specified the platform default will be used.
   */
  def fromStream (stream: InputStream)(implicit codec: Codec) = fromInput(Input.fromStream(stream)(codec))
  
  def fromInput (input: Input) = {
    
    val doc = IO(input)(parse)

    if (rewrite) doc.rewrite() else doc
  }
  
  // TODO - add fromDirectory, fromDefaultDirectories, fromRootDirectory, Codec and parallelization hooks
  
  def fromTree (input: InputProvider) = {
    
    def collectInputs (provider: InputProvider): Seq[Input] =
      provider.inputs ++ (input.subtrees map collectInputs).flatten
    
    val inputs = collectInputs(input) // TODO - alternatively create Map here (do benchmarks)
    
    val documents = inputs map fromInput // TODO - this step can optionally run in parallel
    
    val docMap = documents map (doc => (doc.path, doc)) toMap
    
    def collectDocuments (provider: InputProvider): DocumentTree = {
      val docs = provider.inputs map (i => docMap(i.path))
      val trees = provider.subtrees map (collectDocuments)
      DocumentTree(provider.path, docs, trees)
    }
    
    collectDocuments(input)
  }
  
  
}

/** Serves as an entry point to the Parse API.
 * 
 *  @author Jens Halm
 */
object Parse {
  
  /** Returns a new Parse instance for the specified parse function.
   *  This function is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText. 
   * 
   *  @param parse the parse function to use for all subsequent operations
   */
  def as (parse: Input => Document) = new Parse(parse, true) 
  
}