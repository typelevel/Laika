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

package laika.render

import java.io.{BufferedInputStream, File, FileInputStream}

import cats.effect.IO
import cats.implicits._
import laika.api.Renderer
import laika.ast.DocumentTreeRoot
import laika.format.PDF
import laika.io.{FileIO, IOSpec}
import laika.io.implicits._
import org.scalatest.Assertion

/** Since there is no straightforward way to validate a rendered PDF document
 *  on the JVM, this Spec merely asserts that a file or OutputStream is non-empty
 *  after rendering. Most of the functionality for PDF rendering that Laika itself
 *  provides lies in its support for XSL-FO rendering and application of CSS to
 *  this render process. This functionality is extensively tested in other Specs.
 *  For the actual creation of the binary PDF format, Laika primarily relies
 *  on Apache FOP for converting the rendered XSL-FO to PDF, therefore having 
 *  limited scope in this particular spec is acceptable.  
 */
class PDFRendererSpec extends IOSpec with FileIO {

  
  trait FileSetup {
    
    val file: File = File.createTempFile("output", null)
    
    val firstChar: IO[Int] = for {
      in   <- IO(new BufferedInputStream(new FileInputStream(file)))
      read <- IO(in.read)
    } yield read
    
    def firstCharAvailable(render: IO[Unit]): Assertion = (render >> firstChar).asserting(_ should not be (-1))
  }
  
  
  "The PDF Renderer" should {

    // TODO - 0.16 - there might be the need for enhancing SequentialRenderer/Transformer with passing a template, plus using the default if none is specified
    "render a document to a file" ignore new TreeModel with FileSetup {
      val renderer = Renderer
        .of(PDF)
        .io(blocker)
        .sequential[IO]
        .build
      
      firstCharAvailable(renderer.from(doc(1)).toFile(file).render)
    }

    "render a document to an OutputStream" ignore new TreeModel {
      val renderer = Renderer
        .of(PDF)
        .io(blocker)
        .sequential[IO]
        .build
      
      withByteArrayOutput { out =>
        renderer.from(doc(1)).toStream(IO.pure(out)).render.void
      }.asserting(_ should not be empty)
      
    }

    "render a tree to a file" in new TreeModel with FileSetup {
      val renderer = Renderer.of(PDF)
        .io(blocker)
        .parallel[IO]
        .build

      firstCharAvailable(renderer.from(DocumentTreeRoot(tree)).toFile(file).render)
    }

    "render a tree to an OutputStream" in new TreeModel {
      val renderer = Renderer
        .of(PDF)
        .io(blocker)
        .parallel[IO]
        .build

      withByteArrayOutput { out =>
        renderer.from(DocumentTreeRoot(tree)).toStream(IO.pure(out)).render.void
      }.asserting(_ should not be empty)
    }

  }
}
