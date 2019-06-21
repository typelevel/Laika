package laika.io.ops

import java.io.File

import cats.effect.Async
import laika.io.model.{DirectoryOutput, TreeOutput}

import scala.io.Codec

/** API for specifying the tree of character outputs for a rendering operation.
  *
  * It allows any class merging in this trait to define all input related operations
  * in terms of the only abstract method `toOutput`.
  *
  * @author Jens Halm
  */
trait ParallelTextOutputOps[F[_]] {

  def F: Async[F]

  type Result

  /** Builder step that instructs the runtime to render the document tree to files
    * in the specified directory and its subdirectories.
    * 
    * The virtual paths of the document tree will be translated to a directory structure,
    * with the root of the virtual path being the directory specified with this method.
    * 
    * @param name the name of the directory to write to
    * @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (name: String)(implicit codec: Codec): Result = toDirectory(new File(name))

  /** Builder step that instructs the runtime to render the document tree to files
    * in the specified directory and its subdirectories.
    *
    * The virtual paths of the document tree will be translated to a directory structure,
    * with the root of the virtual path being the directory specified with this method.
    *
    * @param dir the directory to write to
    * @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (dir: File)(implicit codec: Codec): Result = toOutput(F.pure(DirectoryOutput(dir, codec)))

  /** Builder step that instructs the runtime to render the document tree to files
    * in the working directory and its subdirectories.
    *
    * The virtual paths of the document tree will be translated to a directory structure,
    * with the root of the virtual path being the directory specified with this method.
    *
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toWorkingDirectory (implicit codec: Codec): Result = 
    toOutput(F.pure(DirectoryOutput(new File(System.getProperty("user.dir")), codec)))

  /** Builder step that instructs the runtime to render
    * to the specified tree output.
    *
    * This is a generic method based on Laika's IO model that concrete
    * methods delegate to.
    */
  def toOutput (tree: F[TreeOutput]): Result

}
