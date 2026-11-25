package reachablelinkage.linkage

import java.nio.file.{Files, Paths}
import java.util.BitSet
import scala.jdk.CollectionConverters.*

import reachablelinkage.classpath.{ClassIndex, ClassSource, NamedInResources}
import reachablelinkage.symbols.Symbols

/**
 * On demand, not a test: does the call graph reach every method a real run executed? Usage:
 * `Soundness <classpath-file> <app-dir> <executed-methods> [cha|rta] [-v] [-config]`, where the list was written by `tools/coverage-agent` while the
 * application ran. A method that ran but is not reached is a hole in "Latent means no run can get there": the conflicts in it would be
 * labelled Latent though the program really executes it.
 *
 * Only methods of classes on the classpath count (the JDK is not walked). `-v` prints every method not reached.
 */
object Soundness:
  def main(args: Array[String]): Unit =
    val jars  = Files.readAllLines(Paths.get(args(0))).asScala.map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toVector
    val app   = ClassSource.open(Paths.get(args(1))).fold(e => sys.error(e.message), identity)
    val libs  = jars.map(j => ClassSource.open(j).fold(e => sys.error(e.message), identity))
    val syms  = new Symbols
    val index = ClassIndex((app +: libs) :+ ClassSource.jdk(), syms)
    val n     = libs.length + 1
    val traverse = new BitSet; (0 until n).foreach(traverse.set(_))
    val dispatch = if args.lift(3).contains("rta") then Dispatch.Rta else Dispatch.Cha
    val verbose  = args.contains("-v")

    // -config: the classes `reference.conf` and `application.conf` name are roots too (the opt-in `keep-classes-named-in` rule).
    val rules  = if args.contains("-config") then Keep.standard :+ Keep.ClassesNamedIn(Keep.fileNames(sys.props.getOrElse("config.files", "reference.conf,application.conf").split(",").toSeq*)) else Keep.standard
    val kept   = KeepRules.from(syms, rules)
    val named  = if kept.namedFiles.isEmpty then Vector.empty else NamedInResources.classes(index, kept.namedFiles, traverse)
    val graph = MethodGraph.build(index, Resolver(index), kept, dispatch, index.classesIn(0).toVector, named, traverse)

    val executed = Files.readAllLines(Paths.get(args(2))).asScala.map(_.trim).filter(_.nonEmpty)
    var inIndex, reached = 0
    val missed = scala.collection.mutable.ArrayBuffer.empty[(String, Boolean)]   // method, was its class reached at all
    executed.foreach: line =>
      val dot   = line.indexOf('.')
      val owner = line.substring(0, dot)
      val rest  = line.substring(dot + 1)
      val paren = rest.indexOf('(')
      val name  = rest.substring(0, paren)
      val desc  = rest.substring(paren)
      val c     = syms.findClass(owner)
      if !c.isNone && index.exists(c) && index.sourceIndex(c) < n then
        index.get(c) match
          case null => ()
          case info =>
            val m = info.methods.indexWhere(x => syms.name(x.name) == name && syms.desc(x.desc) == desc)
            if m >= 0 then
              inIndex += 1
              if graph.methodReached(c, m) then reached += 1 else missed += ((line, graph.classReached(c)))

    println(f"$dispatch: ${executed.size}%d methods ran, ${inIndex}%d of them in classes of the classpath; the graph reaches $reached%d and misses ${missed.size}%d")
    val byKind = missed.groupBy: (line, classReached) =>
      val name = line.substring(line.indexOf('.') + 1, line.indexOf('('))
      if !classReached then "class not reached at all"
      else if name.contains("$anonfun") || name.startsWith("lambda$") then "lambda body"
      else if name == "<clinit>" then "static initialiser"
      else if name == "<init>" then "constructor"
      else "other method of a reached class"
    byKind.toList.sortBy(-_._2.size).foreach((kind, ms) => println(f"  ${ms.size}%5d  $kind"))
    if verbose then missed.foreach((line, reachedClass) => println((if reachedClass then "  [class reached] " else "  [class missed]  ") + line))
    else missed.take(12).foreach((line, reachedClass) => println((if reachedClass then "    [class reached] " else "    [class missed]  ") + line))
