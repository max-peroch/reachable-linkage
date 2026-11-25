package reachablelinkage.bench

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.linkage.{Checker, Precision, Scope}
import reachablelinkage.symbols.Symbols

/** Phase breakdown of one check on a real classpath. `Profile <classpath-file> <app-dir> <class|method>`. */
object Profile:
  private def time[A](label: String)(body: => A): A =
    val t0 = System.nanoTime
    val r  = body
    println(f"  $label%-46s ${(System.nanoTime - t0) / 1e6}%8.0f ms")
    r

  def main(args: Array[String]): Unit =
    val jars    = Files.readAllLines(Paths.get(args(0))).asScala.map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toVector
    val appDir  = Paths.get(args(1))
    val precision = if args(2) == "method" then Precision.Method else Precision.Class
    val t0      = System.nanoTime
    val cache   = Option(System.getProperty("bench.cache")).map(Paths.get(_))
    val sources = time("open the app and jars")((appDir +: jars).map(p => ClassSource.open(p, cache).toOption.get))
    val jdk     = time("ClassSource.jdk()")(ClassSource.jdk(cache))
    val syms    = new Symbols
    val index   = time("ClassIndex (all tables of contents)")(ClassIndex((sources :+ jdk).toIndexedSeq, syms))
    val n       = sources.size
    val scope   = Scope(index, Vector(0), (0 until n).toVector, (0 until n).toVector).fold(e => throw new AssertionError(e.message), identity)
    val found   = time(s"Checker.findings ($precision)")(Checker.findings(index, scope, precision))
    val counts  = index.parseCounts
    println(f"  ${"total"}%-46s ${(System.nanoTime - t0) / 1e6}%8.0f ms")
    println(s"  findings=${found.size}  classes parsed: app+jars=${counts.take(n).sum}  JDK=${counts(n)}  decoded from summaries=${index.summaryLoads.sum}  (index holds ${(0 to n).map(i => index.classesIn(i).size).sum} classes)")
