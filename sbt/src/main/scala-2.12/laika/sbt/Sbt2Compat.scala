package laika.sbt

import xsbti.{ FileConverter }
import java.io.File
import sbt.{ Attributed, Def }

import scala.annotation.nowarn

object Sbt2Compat {
  type FileRef = java.io.File

  /** Gives sbt.Def the .uncached() method that sbt 2 has natively.
    *  In sbt 2 all tasks are cached by default; Def.uncached(a) opts out.
    *  In sbt 1 there is no caching, so this is a no-op.
    */
  implicit class DefOps(private val singleton: Def.type) extends AnyVal {
    def uncached[A](a: A): A = a
  }

  @nowarn("msg=never used")
  def toFile(a: Attributed[File])(implicit conv: FileConverter): File =
    a.data

  @nowarn("msg=never used")
  def toFile(ref: File)(implicit conv: FileConverter): File =
    ref

  @nowarn("msg=never used")
  def toFileRefsMapping(mappings: Seq[(File, String)])(implicit
      conv: FileConverter
  ): Seq[(FileRef, String)] =
    mappings

}
