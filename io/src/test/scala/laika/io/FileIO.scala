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
import fs2.io.file.Files
import laika.ast.Path.Root
import laika.io.model.{FilePath, TextInput, TextOutput}

import java.io.ByteArrayOutputStream
import scala.io.Codec

trait FileIO {

  def readFile (base: String): IO[String] = readFile(FilePath.parse(base))
  
  def readFile (f: FilePath)(implicit codec: Codec): IO[String] = TextInput.fromFile[IO](f).input

  def writeFile (f: FilePath, content: String): IO[Unit] = TextOutput.forFile[IO](f).writer(content)

  def newTempDirectory: IO[FilePath] = Files[IO].createTempDirectory.map(FilePath.fromFS2Path)

  def newTempFile: IO[FilePath] = Files[IO].createTempFile.map(FilePath.fromFS2Path)
  
  def mkDir (f: FilePath): IO[Unit] = Files[IO].createDirectory(f.toFS2Path)

  def delete (f: FilePath): IO[Unit] = Files[IO].delete(f.toFS2Path)
  
  def exists (f: FilePath): IO[Boolean] = Files[IO].exists(f.toFS2Path)

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
