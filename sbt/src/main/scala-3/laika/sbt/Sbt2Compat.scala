package laika.sbt

import sbt.*
import xsbti.{ FileConverter, HashedVirtualFileRef }

object Sbt2Compat {
  type FileRef = HashedVirtualFileRef

  /** Source-compatibility stub for explicit imports shared with the sbt 1 build.
    *  Def.uncached is native on sbt 2, so no enrichment is needed here.
    */
  final class DefOps private ()

  private def toFileRef(x: File)(using conv: FileConverter): FileRef =
    conv.toVirtualFile(x.toPath())

  def toFile(a: Attributed[HashedVirtualFileRef])(using conv: FileConverter): File =
    conv.toPath(a.data).toFile()

  def toFile(ref: HashedVirtualFileRef)(using conv: FileConverter): File =
    conv.toPath(ref).toFile()

  def toFileRefsMapping(mappings: Seq[(File, String)])(using
      conv: FileConverter
  ): Seq[(FileRef, String)] =
    mappings.map { case (f, name) => toFileRef(f) -> name }

}
