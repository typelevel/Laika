package laika.runtime

import java.io._

import cats.effect.{Async, Resource}
import laika.ast.Path
import laika.io._
import laika.parse.ParserContext
import laika.parse.markup.DocumentParser.ParserInput
import cats.implicits._

import scala.io.Codec

/** 
  * @author Jens Halm
  */
object InputRuntime {
  
  def readParserInput[F[_]: Async] (input: TextInput): F[ParserInput] = input match {
    case StringInput(source, _, path) => 
      Async[F].pure(ParserInput(path, ParserContext(source)))
    case TextFileInput(file, _, path, codec) => 
      fileReader(file, codec).use { reader =>
        readAll(reader, file.length.toInt)
          .map(source => ParserInput(path, ParserContext(source)))
      }
  }

  def fileReader[F[_]: Async] (file: File, codec: Codec): Resource[F, Reader] = Resource.fromAutoCloseable(Async[F].delay {
    new BufferedReader(new InputStreamReader(new FileInputStream(file), codec.charSet))
  })

  def readAll[F[_]: Async] (reader: Reader, sizeHint: Int): F[String] = {
    
    def read(inBuffer: Array[Char], outBuffer: StringBuilder): F[Unit] = {
      for {
        amount <- Async[F].delay(reader.read(inBuffer, 0, inBuffer.length))
        _      <- if (amount == -1) Async[F].unit
                  else Async[F].delay(outBuffer.appendAll(inBuffer, 0, amount)) >> read(inBuffer, outBuffer)
      } yield ()
    }
    
    for {
      inBuffer  <- Async[F].delay(new Array[Char](Math.max(sizeHint, 8)))
      outBuffer = new StringBuilder
      _         <- read(inBuffer, outBuffer)
    } yield outBuffer.toString
  }
  
  def scanDirectories[F[_]: Async] (input: DirectoryInput): F[InputCollection] = ???

//  case class Directory (path: Path, file: File)
  
//  def asInputCollection (input: DirectoryInput): InputCollection = {
//
//    import DocumentType._
//
//    //    require(roots.nonEmpty, "The specified roots sequence must contain at least one directory")
//    //    for (root <- roots) {
//    //      require(root.exists, s"Directory ${root.getAbsolutePath} does not exist")
//    //      require(root.isDirectory, s"File ${root.getAbsolutePath} is not a directory")
//    //    }
//
//    val allDirs: Seq[Directory] = {
//
//      def collectSubDirectories (dir: Directory): Seq[Directory] = {
//        val subDirs = dir.file.listFiles filter (f => f.isDirectory && !input.fileFilter(f) && input.docTypeMatcher(dir.path / f.getName) != Ignored) map (d => Directory(dir.path / d.getName, d))
//        dir +: subDirs.flatMap(collectSubDirectories)
//      }
//
//      input.directories.map(d => Directory(Root, d)).flatMap(collectSubDirectories)
//    }
//
//    val allFiles: Seq[(DocumentType, Path, File)] = {
//      def filesInDir (dir: Directory): Array[(DocumentType, Path, File)] = dir.file.listFiles collect {
//        case file if file.isFile && !input.fileFilter(file) => (input.docTypeMatcher(dir.path / file.getName), dir.path / file.getName, file)
//      }
//      allDirs.flatMap(filesInDir)
//    }
//
//    val textInputs: Seq[TextInput] = allFiles.collect {
//      case (docType: TextDocumentType, filePath, file) => TextFileInput(file, docType, filePath, input.codec)
//    }
//
//    val binaryInputs: Seq[BinaryInput] = allFiles.collect {
//      case (Static, filePath, file) => BinaryFileInput(file, filePath)
//    }
//
//    val sourcePaths: Seq[String] = input.directories map (_.getAbsolutePath)
//
//    InputCollection(textInputs, binaryInputs) // TODO - 0.12 - what about sourceDirectories?
//  }

  // TODO - 0.12 - temporary solution
  def classPathParserInput (resourcePath: String, virtualPath: Path)(implicit codec: Codec): ParserInput = {

    def readAll (reader: Reader, sizeHint: Int): String = {

      val arr = new Array[Char](sizeHint)
      val buffer = new StringBuilder
      var numCharsRead: Int = 0

      while ({numCharsRead = reader.read(arr, 0, arr.length); numCharsRead != -1}) {
        buffer.appendAll(arr, 0, numCharsRead)
      }

      buffer.toString
    }

    val stream = getClass.getResourceAsStream(resourcePath)
    IOX(stream){ in =>
      val reader = new BufferedReader(new InputStreamReader(in, codec.decoder))
      val content = readAll(reader, 8 * 1024)
      ParserInput(virtualPath, ParserContext(content))
    }
  }
  
}
