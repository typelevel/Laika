/*
 * Copyright 2012-2022 the original author or authors.
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

package laika.io.model

import munit.FunSuite

import java.io.File
import java.nio.file.{ FileSystems, Paths }

class FilePathSpec extends FunSuite {

  private val platformSeparator = FileSystems.getDefault.getSeparator
  private val platformRoot      = Paths.get(System.getProperty("user.dir")).getRoot.toString

  private val absTestPathString =
    platformRoot + List("foo", "bar", "..", "baz.jpg").mkString(platformSeparator)

  private val relTestPathString = List("foo", "bar", "..", "baz.jpg").mkString(platformSeparator)
  private val expectedPath      = platformRoot + List("foo", "baz.gif").mkString(platformSeparator)

  test("create and modify normalized FilePath from NIO path") {
    val path = FilePath.fromNioPath(Paths.get(absTestPathString))
    assertEquals(path.withSuffix("gif").toString, expectedPath)
  }

  test("create and modify normalized FilePath from fs2 path") {
    val path = FilePath.fromFS2Path(fs2.io.file.Path.fromNioPath(Paths.get(absTestPathString)))
    assertEquals(path.withSuffix("gif").toString, expectedPath)
  }

  test("create and modify normalized FilePath from Java File") {
    val path = FilePath.fromJavaFile(new File(absTestPathString))
    assertEquals(path.withSuffix("gif").toString, expectedPath)
  }

  test("create and modify normalized FilePath from parsed string") {
    val path = FilePath.parse(absTestPathString)
    assertEquals(path.withSuffix("gif").toString, expectedPath)
  }

  test("round trip absolute FilePath from and to NIO path") {
    val inPath  = Paths.get(absTestPathString)
    val outPath = FilePath.fromNioPath(inPath).toNioPath
    assertEquals(outPath, inPath.normalize())
  }

  test("round trip absolute FilePath from and to fs2 path") {
    val inPath  = fs2.io.file.Path.fromNioPath(Paths.get(absTestPathString))
    val outPath = FilePath.fromFS2Path(inPath).toFS2Path
    assertEquals(outPath, inPath.normalize)
  }

  test("round trip absolute FilePath from and to Java file") {
    val inPath  = new File(absTestPathString)
    val outPath = FilePath.fromJavaFile(inPath).toJavaFile
    assertEquals(outPath, inPath.getCanonicalFile)
  }

  test("round trip relative FilePath from and to NIO path") {
    val inPath  = Paths.get(relTestPathString)
    inPath.resolve("foo")
    val outPath = FilePath.fromNioPath(inPath).toNioPath
    assertEquals(outPath, inPath.normalize())
  }

  test("round trip relative FilePath from and to fs2 path") {
    val inPath  = fs2.io.file.Path.fromNioPath(Paths.get(relTestPathString))
    val outPath = FilePath.fromFS2Path(inPath).toFS2Path
    assertEquals(outPath, inPath.normalize)
  }

  test("round trip relative FilePath from and to Java file") {
    val inPath  = new File(relTestPathString)
    val outPath = FilePath.fromJavaFile(inPath).toJavaFile
    assertEquals(outPath, new File(List("foo", "baz.jpg").mkString(platformSeparator)))
  }

}
