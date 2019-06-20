package laika.io.ops

import java.io.File

import cats.effect.Async
import laika.api.builder.OperationConfig
import laika.io.model.{DirectoryInput, TreeInput}

import scala.io.Codec

/** API for producing a result from processing various types of input trees.
  *
  * This is essentially a collection of shortcuts that allow any class
  * merging in this trait to define all input related operations in terms of the only
  * abstract method `fromInputTree`. Calling `fromDirectory("src")` for example
  * is only a convenient shortcut for calling `fromInputTree(InputTree.fromDirectory("src")`.
  *
  * @author Jens Halm
  */
trait ParallelInputOps[F[_]] {

  def F: Async[F]

  type FileFilter = File => Boolean

  /** The type of the result returned by all operations of this trait.
    */
  type Result

  /** The configuration to use for all input operations.
    */
  def config: OperationConfig

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String)(implicit codec: Codec): Result =
    fromDirectory(new File(name), DirectoryInput.hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): Result =
    fromDirectory(new File(name), exclude)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: File)(implicit codec: Codec): Result =
    fromDirectory(dir, DirectoryInput.hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): Result =
    fromDirectories(Seq(dir), exclude)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[File])(implicit codec: Codec): Result =
    fromDirectories(roots, DirectoryInput.hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[File], exclude: FileFilter)(implicit codec: Codec): Result =
    fromInput(F.pure(DirectoryInput(roots, codec, config.docTypeMatcher, exclude)))

  /**  Returns the result obtained by parsing files from the
    *  current working directory.
    *
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromWorkingDirectory (exclude: FileFilter = DirectoryInput.hiddenFileFilter)(implicit codec: Codec): Result =
    fromDirectories(Seq(new File(System.getProperty("user.dir"))), exclude)

  /** Returns the result obtained by parsing files from the
    *  specified input tree.
    *
    *  @param input the input tree to process
    */
  def fromInput(input: F[TreeInput]): Result

}
