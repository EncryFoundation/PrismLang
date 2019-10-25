package org.encryfoundation.prismlang

import java.nio.file._

import collection.JavaConverters._
import org.encryfoundation.prismlang.compiler.PCompiler
import scorex.crypto.encode.Base16

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object Main extends App {

  override def main(args: Array[String]): Unit = {

    val matcher: Path => Boolean = (path: Path) => path.toString.toLowerCase().endsWith(".pr")

    assert(args.length == 2 || args.length == 1, "You should provide 1 or 2 arguments")

    val srcPath = Paths.get(args(0))
    val targetPathOpt = args.lift(1).map(Paths.get(_))

    Try {
      if (Files.exists(srcPath)) {
        if (Files.isDirectory(srcPath)) {
          Files
            .walk(srcPath)
            .iterator()
            .asScala
            .filter(p => matcher(p))
            .foreach(file => processFile(file, srcPath, targetPathOpt))
        } else {
          assert(matcher(srcPath), "File should have \'.pr\' extension")
          processFile(srcPath, srcPath.getParent, targetPathOpt)
        }
      } else println(s"File or folder $srcPath does not exist")
    }.recoverWith {
      case NonFatal(th) =>
        println(s"Compiler failed due to ${th.getMessage}")
        Failure(th)
    }
  }

  def processFile(srcFilePath: Path, sourceFolderPath: Path, targetPathOpt: Option[Path]): Unit =
    (Try(Files.readAllLines(srcFilePath).asScala.mkString("\n"))
    .flatMap(PCompiler.compile) match {
      case Success(contract) => Try {

        val dir = targetPathOpt match {
          case Some(targetPath) if targetPath.isAbsolute =>
            val relative = sourceFolderPath.relativize(srcFilePath.getParent)
            targetPath.resolve(relative)
          case Some(targetPath) =>
            val target = Paths.get(".").toAbsolutePath.resolve(targetPath)
            val relative = sourceFolderPath.relativize(srcFilePath.getParent)
            target.resolve(relative)
          case None => srcFilePath.getParent
        }

        if (!Files.exists(dir)) Files.createDirectories(dir)

        val hashPath = dir.resolve(s"${srcFilePath.getFileName}.hash")
        val bytesPath = dir.resolve(s"${srcFilePath.getFileName}.bytes")

        Files.write(hashPath, Base16.encode(contract.hash).getBytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
        Files.write(bytesPath, Base16.encode(contract.bytes).getBytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

        srcFilePath
      }.toEither
      case Failure(th) => Left(StandaloneCompilerException(s"Failed to compile ${srcFilePath.toString} due to: ${th.getMessage}"))
    }) match {
      case Right(file) => println(s"Successfully compiled $file")
      case Left(th) => println(th.getMessage)
    }

}

case class StandaloneCompilerException(message: String) extends RuntimeException(message)
