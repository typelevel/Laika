package laika.io.ops

import java.io.File

import cats.effect.Async
import laika.io.model.{DirectoryOutput, TreeOutput}

import scala.io.Codec

/** Represents a tree of output destinations for recursive render operations.
  *  Various types of output can be specified to trigger the actual rendering.
  */
trait ParallelTextOutputOps[F[_]] {

  def F: Async[F]

  type Result

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param name the name of the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (name: String)(implicit codec: Codec): Result = toDirectory(new File(name))

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param dir the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (dir: File)(implicit codec: Codec): Result = toOutput(F.pure(DirectoryOutput(dir, codec)))

  /** Renders the document tree to the
    *  current working directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDefaultDirectory (implicit codec: Codec): Result = toOutput(F.pure(DirectoryOutput(new File(System.getProperty("user.dir")), codec)))

  /** Renders the document tree to the specified output tree.
    */
  def toOutput (tree: F[TreeOutput]): Result

}
