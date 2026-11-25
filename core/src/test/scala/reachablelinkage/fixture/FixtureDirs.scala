package reachablelinkage.fixture

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Temp directories for compiled fixtures, removed when the test JVM exits. */
object FixtureDirs:
  private val root: Path =
    val dir = Files.createTempDirectory("reachable-linkage-fixtures")
    Runtime.getRuntime.addShutdownHook(new Thread(() => deleteTree(dir)))
    dir

  private def deleteTree(p: Path): Unit =
    if Files.exists(p) then
      val stream = Files.walk(p)
      try stream.sorted(java.util.Comparator.reverseOrder[Path]()).iterator.asScala.foreach(Files.delete)
      finally stream.close()

  def fresh(): Path = Files.createTempDirectory(root, "c")

  /** Writes `sources` (relative path -> content) under a new `src` directory; returns its file paths. */
  def writeSources(work: Path, sources: Map[String, String]): Seq[String] =
    val src = Files.createDirectories(work.resolve("src"))
    sources.toSeq.map: (name, content) =>
      val f = src.resolve(name)
      Files.createDirectories(f.getParent)
      Files.writeString(f, content)
      f.toString
