package reachablelinkage.linkage

import java.nio.file.{Files, Paths}
import java.util.BitSet
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.ClassLoadLog
import reachablelinkage.classpath.{ClassIndex, ClassSource, NamedInResources}
import reachablelinkage.symbols.Symbols

/** On demand, not a test: how much of what a real run loaded does the static class walk reach? Usage: `BlindSpots <classpath-file> <app-dir> <class-load-log>`.
  * The classes the run loaded from the application or its jars that the walk from the application's classes never reaches are the analysis's blind spots, which
  * the class-load log fills in.
  */
object BlindSpots:
  def main(args: Array[String]): Unit =
    val jars     = Files.readAllLines(Paths.get(args(0))).asScala.map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toVector
    val app      = ClassSource.open(Paths.get(args(1))).fold(e => sys.error(e.message), identity)
    val libs     = jars.map(j => ClassSource.open(j).fold(e => sys.error(e.message), identity))
    val syms     = new Symbols
    val index    = ClassIndex((app +: libs) :+ ClassSource.jdk(), syms)
    val n        = libs.length + 1
    val traverse = new BitSet; (0 until n).foreach(traverse.set(_))

    val loaded      = ClassLoadLog.parse(Paths.get(args(2))).fold(e => sys.error(e.message), _.classes.asScala.map(_.replace('.', '/')).toVector)
    val onClasspath = loaded.map(syms.findClass).filterNot(_.isNone).filter(c => index.exists(c) && index.sourceIndex(c) < n)

    val withConfig =
      Keep.standard :+ Keep.ClassesNamedIn(Keep.fileNames(sys.props.getOrElse("config.files", "reference.conf,application.conf").split(",").toSeq*))
    for (label, rules) <- Seq("standard rules" -> Keep.standard, "plus config" -> withConfig, "no keep rules" -> Nil) do
      val kept    = KeepRules.from(syms, rules)
      val named   = if kept.namedFiles.isEmpty then Vector.empty else NamedInResources.classes(index, kept.namedFiles, traverse)
      val reached = Reachability.classes(index, kept, index.classesIn(0).toVector ++ named, traverse)
      val missed  = onClasspath.filterNot(c => reached.get(c.value))
      println(
        f"$label%-15s: ${named.size}%d classes named in config; the run loaded ${loaded.size}%d classes, ${onClasspath.size}%d on the classpath; the walk misses ${missed.size}%d"
      )
      // -Dblind.all lists every miss, so they can be matched against configuration files.
      if named.nonEmpty then
        val loadedSet = onClasspath.map(_.value).toSet
        println(f"   of the ${named.size}%d classes named in config, the run loaded ${named.count(c => loadedSet.contains(c.value))}%d")
      if rules.nonEmpty then
        missed.take(if sys.props.contains("blind.all") then missed.size else 12).foreach(c => println("   " + syms.className(c).replace('/', '.')))
