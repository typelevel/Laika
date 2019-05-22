/*
 * Copyright 2016 the original author or authors.
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

package laika.render

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}

import laika.api.{Render, Renderer}
import laika.ast.DocumentTreeRoot
import laika.format.PDF
import org.scalatest.{FlatSpec, Matchers}

/** Since there is no straightforward way to validate a rendered PDF document
 *  on the JVM, this Spec merely asserts that a file or OutputStream is non-empty
 *  after rendering. Most of the functionality for PDF rendering that Laika itself
 *  provides lies in its support for XSL-FO rendering and application of CSS to
 *  this render process. This functionality is extensively tested in other Specs.
 *  For the actual creation of the binary PDF format, Laika primarily relies
 *  on Apache FOP for converting the rendered XSL-FO to PDF, therefore having 
 *  limited scope in this particular spec is acceptable.  
 */
class PDFRendererSpec extends FlatSpec with Matchers {
  
  
  trait FileSetup {
    
    val file: File = File.createTempFile("output", null)
    
    def readFile: InputStream = new BufferedInputStream(new FileInputStream(file))
    
  }
  
  
  "The PDF Renderer" should "render a document to a file" ignore new TreeModel with FileSetup {
    Renderer.of(PDF).from(doc(1)).toFile(file).execute
    readFile.read should not be (-1)
  }

  it should "render a document to a file using a custom FopFactory" ignore new TreeModel with FileSetup {
    Renderer.of(PDF.withFopFactory(PDF.defaultFopFactory)).from(doc(1)).toFile(file).execute
    readFile.read should not be (-1)
  }
  
  it should "render a document to an OutputStream" ignore new TreeModel {
//    val stream = new ByteArrayOutputStream
//    Renderer.of(PDF).from(doc(1)).toStream(stream).execute
//    stream.toByteArray should not be empty
  }
  
  it should "render a tree to a file" ignore new TreeModel with FileSetup {
    Renderer.of(PDF).from(DocumentTreeRoot(tree)).toFile(file).execute
    readFile.read should not be (-1)
  }
  
  it should "render a tree to an OutputStream" ignore new TreeModel {
//    val stream = new ByteArrayOutputStream
//    Renderer.of(PDF).from(tree).toStream(stream).execute
//    stream.toByteArray should not be empty
  }
  
  
}
