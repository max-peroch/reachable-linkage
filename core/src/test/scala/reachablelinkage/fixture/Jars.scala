package reachablelinkage.fixture

import java.nio.file.{Files, Path}
import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry
import scala.jdk.CollectionConverters.*

/** Packs a directory of class files into a jar, for tests that need a jar and not a directory. */
object Jars:
  def of(classes: Path): Path =
    val file = Files.createTempFile("fixture", ".jar")
    val out  = new JarOutputStream(Files.newOutputStream(file))
    val walk = Files.walk(classes)
    try walk.iterator.asScala.filter(Files.isRegularFile(_)).foreach: p =>
      out.putNextEntry(new ZipEntry(classes.relativize(p).toString.replace('\\', '/')))
      out.write(Files.readAllBytes(p))
      out.closeEntry()
    finally { walk.close(); out.close() }
    file
