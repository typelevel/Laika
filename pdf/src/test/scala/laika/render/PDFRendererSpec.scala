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

import java.io.{BufferedInputStream, ByteArrayOutputStream, File, FileInputStream}

import cats.effect.IO
import cats.implicits._
import laika.api.Renderer
import laika.ast.DocumentTreeRoot
import laika.format.PDF
import laika.io.IOSpec
import laika.io.implicits._
import laika.io.runtime.TestContexts.blocker
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
class PDFRendererSpec extends IOSpec {

  
  trait FileSetup {
    
    val file: File = File.createTempFile("output", null)
    
    val firstChar: IO[Int] = for {
      in   <- IO(new BufferedInputStream(new FileInputStream(file)))
      read <- IO(in.read)
    } yield read
    
    def firstCharAvailable(render: IO[Unit]): Assertion = (render >> firstChar).asserting(_ should not be (-1))
  }
  
  
  "The PDF Renderer" should {

    "render a document to a file" in new TreeModel with FileSetup {
      val renderer = Renderer
        .of(PDF)
        .io(blocker)
        .sequential[IO]
        .build
      
      firstCharAvailable(renderer.from(doc(1)).toFile(file).render)
    }

    "render a document to a file using a custom FopFactory" in new TreeModel with FileSetup {
      val renderer = Renderer
        .of(PDF.withFopFactory(PDF.defaultFopFactory))
        .io(blocker)
        .sequential[IO]
        .build

      firstCharAvailable(renderer.from(doc(1)).toFile(file).render)  
    }

    "render a document to an OutputStream" in new TreeModel {
      val renderer = Renderer
        .of(PDF)
        .io(blocker)
        .sequential[IO]
        .build
      
      val res = for {
        stream <- IO(new ByteArrayOutputStream)
        _      <- renderer.from(doc(1)).toStream(IO.pure(stream)).render
      } yield stream.toByteArray
      
      res.asserting(_ should not be empty)
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

      val res = for {
        stream <- IO(new ByteArrayOutputStream)
        _      <- renderer.from(DocumentTreeRoot(tree)).toStream(IO.pure(stream)).render
      } yield stream.toByteArray

      res.asserting(_ should not be empty)
    }

  }
}
