package reachablelinkage.classpath

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** A directory of class files. */
private[classpath] final class DirSource(val origin: Path) extends ClassSource:
  def classNames: Iterator[String] =
    Using.resource(Files.walk(origin)): stream =>
      stream.iterator.asScala
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class"))
        .map(p => origin.relativize(p).toString.replace('\\', '/').dropRight(6))
        .toList
    .iterator

  def read(internalName: String): Option[Array[Byte]] =
    val p = origin.resolve(internalName + ".class")
    if Files.isRegularFile(p) then Some(Files.readAllBytes(p)) else None

  override def serviceFiles: Iterator[String] =
    val dir = origin.resolve(ClassSource.ServicesDir)
    if !Files.isDirectory(dir) then Iterator.empty
    else
      Using.resource(Files.list(dir)): stream =>
        stream.iterator.asScala.filter(Files.isRegularFile(_)).map(p => ClassSource.ServicesDir + p.getFileName.toString).toList
      .iterator

  override def resourceNames: Iterator[String] =
    Using.resource(Files.walk(origin)): stream =>
      stream.iterator.asScala.filter(p => Files.isRegularFile(p) && !p.getFileName.toString.endsWith(".class"))
        .map(p => origin.relativize(p).toString.replace('\\', '/')).toList
    .iterator

  override def readResource(path: String): Option[Array[Byte]] =
    val p = origin.resolve(path)
    if Files.isRegularFile(p) then Some(Files.readAllBytes(p)) else None

  def close(): Unit = ()
