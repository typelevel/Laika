package laika.io.runtime

import munit.CatsEffectSuite
import laika.io.model.{TextOutput,TextOutput2}
import cats.effect.IO

class OutputRuntimeSpec extends CatsEffectSuite {
  test("OutputRuntime#write can write text content to file"){
    for {
      tmp <- fs2.io.file.Files[IO].createTempFile
      o = TextOutput.forFile[IO](laika.ast.Path.Root, tmp)
      content = "test"
      _ <- OutputRuntime.write(content,o)
      _ <- fs2.io.file.Files[IO].readAll(tmp).through(fs2.text.utf8.decode).compile.string.assertEquals(content)
    } yield ()
  }
  test("fs2-based OutputRuntime can write text content to file"){
    for {
      tmp <- fs2.io.file.Files[IO].createTempFile
      o = TextOutput2.forFile[IO](laika.ast.Path.Root, tmp.toNioPath.toFile())
      content = "test"
      _ <- OutputRuntime.write(content,o)
      _ <- fs2.io.file.Files[IO].readAll(tmp).through(fs2.text.utf8.decode).compile.string.assertEquals(content)
    } yield ()
  }
}
