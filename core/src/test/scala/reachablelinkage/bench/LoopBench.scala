package reachablelinkage.bench

import java.nio.file.{Files, Path, Paths}
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.*
import reachablelinkage.classfile.ClassParser
import reachablelinkage.symbols.Symbols

/** On-demand micro-benchmark for loop-shape changes (a `while` against a `@tailrec` method). Two measurements, each the best of several rounds in one warmed
  * JVM, so noise from the JIT and the machine is mostly gone:
  *
  *   - `parse`: `ClassParser.parse` over every class of the jars, bytes preloaded
  *   - `check`: `Linkage.check` at Method precision with a warm summary cache
  *
  * Usage: `LoopBench <classpath-file> <app-dir> <cache-dir>`.
  */
object LoopBench:

  def main(args: Array[String]): Unit =
    val jars  = Files.readAllLines(Paths.get(args(0))).asScala.map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toList
    val app   = Paths.get(args(1))
    val cache = Paths.get(args(2))

    val bytes = jars.flatMap(classBytes)
    val parse = best(7)(() =>
      val syms = new Symbols
      var n    = 0
      bytes.foreach(b => if ClassParser.parse(b, syms).isRight then n += 1)
      n
    )
    println(f"parse: ${bytes.size} classes, best ${parse}%.0f ms")

    def request = LinkageRequest(
      java.util.List.of(app),
      jars.map(j => ClasspathEntry(j.getFileName.toString, j)).asJava,
      CheckScope.App,
      java.util.Set.of(),
      java.util.List.of(),
      CheckPrecision.METHOD,
      java.util.Optional.of(cache)
    )
    Linkage.check(request) // builds the summaries
    val check = best(9)(() => Linkage.check(request).conflicts.size)
    println(f"check: best ${check}%.0f ms")

  private def classBytes(jar: Path): List[Array[Byte]] =
    val zip = new ZipFile(jar.toFile)
    try
      zip.entries.asScala
        .filter(e => e.getName.endsWith(".class") && !e.getName.startsWith("META-INF/") && !e.getName.endsWith("module-info.class"))
        .map(e => zip.getInputStream(e).readAllBytes())
        .toList
    finally zip.close()

  private def best(rounds: Int)(work: () => Int): Double =
    val times = (1 to rounds).map: _ =>
      val t0 = System.nanoTime
      val r  = work()
      require(r >= 0)
      (System.nanoTime - t0) / 1e6
    times.min
