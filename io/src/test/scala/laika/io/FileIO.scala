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

package laika.io

import cats.effect.IO
import laika.ast.DocumentType
import laika.ast.Path.Root
import laika.io.model.{TextInput, TextOutput}

import java.io.{ByteArrayOutputStream, File}
import java.util.UUID
import scala.io.Codec

trait FileIO {

  def readFile (base: String): IO[String] = readFile(new File(base))
  
  def readFile (f: File): IO[String] = readFile(f, Codec.UTF8)

  def readFile (f: File, codec: Codec): IO[String] = {
    val input = TextInput.fromFile[IO](Root, DocumentType.Markup, f, codec)
    input.asDocumentInput.map(_.source.input)
  }

  def writeFile (f: File, content: String): IO[Unit] =
    TextOutput.forFile[IO](Root, f, Codec.UTF8).writer(content)

  def newTempDirectory: IO[File] = {
    for {
      baseDir <- IO(System.getProperty("java.io.tmpdir")).map(new File(_))
      uuid    <- IO(UUID.randomUUID)
      tempDir =  new File(baseDir, s"temp-$uuid")
      res     <- IO(tempDir.mkdir())
      _       <- if (res) IO.unit else IO.raiseError(new RuntimeException("Unable to create temp directory"))
    } yield tempDir
  }

  def newTempFile: IO[File] =
    for {
      uuid <- IO(UUID.randomUUID)
      file <- IO(File.createTempFile(s"temp-$uuid", null))
    } yield file
  
  def mkDir (f: File): IO[Unit] = for {
    res     <- IO(f.mkdir())
    _       <- if (res) IO.unit else IO.raiseError(new RuntimeException(s"Unable to create directory '${f.getPath}'"))
  } yield ()

  def delete (f: File): IO[Unit] = for {
    res     <- IO(f.delete())
    _       <- if (res) IO.unit else IO.raiseError(new RuntimeException(s"Unable to delete file '${f.getPath}'"))
  } yield ()
  
  def exists (f: File): IO[Boolean] = IO(f.exists())

  def withByteArrayTextOutput (charSet: String)(f: ByteArrayOutputStream => IO[Unit]): IO[String] =
    for {
      stream <- IO(new ByteArrayOutputStream())
      _      <- f(stream)
    } yield stream.toString(charSet)
  
  def withByteArrayTextOutput (f: ByteArrayOutputStream => IO[Unit]): IO[String] = withByteArrayTextOutput("UTF-8")(f)

  def withByteArrayOutput (f: ByteArrayOutputStream => IO[Unit]): IO[Array[Byte]] =
    for {
      stream <- IO(new ByteArrayOutputStream())
      _      <- f(stream)
    } yield stream.toByteArray
    
}
