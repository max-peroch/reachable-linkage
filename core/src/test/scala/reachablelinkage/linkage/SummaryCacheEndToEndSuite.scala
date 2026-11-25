package reachablelinkage.linkage

import java.nio.file.{Files, Path, Paths}

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.Scopes
import reachablelinkage.symbols.Symbols

/** The per-jar summary cache must be invisible to the answer: a whole check on a real classpath gives identical findings with no cache, with a cold cache
  * (summaries built on the way) and with a warm one (classes decoded from summaries). The warm run must really not parse the jars' classes.
  */
class SummaryCacheEndToEndSuite extends munit.FunSuite:
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(5, "min")

  private val jars: Seq[Path] =
    val p  = new java.util.Properties()
    val in = getClass.getResourceAsStream("/scala-compilers.properties")
    try p.load(in)
    finally in.close()
    p.getProperty("2.13").split(java.io.File.pathSeparator).toSeq.map(Paths.get(_))

  /** The findings, rendered with names so two runs' ids compare, and how many classes were parsed or decoded. */
  private def run(cache: Option[Path]): (Vector[String], Int, Int) =
    val sources  = jars.map(j => ClassSource.open(j, cache).toOption.get)
    val syms     = new Symbols
    val index    = ClassIndex((sources :+ ClassSource.jdk()).toIndexedSeq, syms)
    val all      = sources.indices.toVector
    val scope    = Scopes.of(index, all, all, all)
    val findings = Checker.findings(index, scope, Precision.Method)
    val shown    = findings.map(f => s"${f.label} ${f.kind} ${f.refIndex} ${f.conflict.show(syms)}")
    val n        = sources.size
    (shown, index.parseCounts.take(n).sum, index.summaryLoads.take(n).sum)

  test("cold cache, warm cache and no cache give identical findings; the warm run parses no jar class") {
    val cache                             = Files.createTempDirectory("e2e-cache")
    val (plain, plainParsed, plainLoaded) = run(None)
    val (cold, coldParsed, coldLoaded)    = run(Some(cache))
    val (warm, warmParsed, warmLoaded)    = run(Some(cache))

    assert(plain.nonEmpty, "the classpath has conflicts, so there is something to compare")
    assertEquals(cold, plain, "a cold cache must not change the answer")
    assertEquals(warm, plain, "a warm cache must not change the answer")

    assert(plainParsed > 5000 && plainLoaded == 0, s"no cache: everything is parsed ($plainParsed parsed, $plainLoaded loaded)")
    assertEquals(coldParsed, 0, "a cold cache builds each summary, then takes classes from it")
    assert(coldLoaded > 5000)
    assertEquals(warmParsed, 0, "a warm cache parses no class of the jars")
    assertEquals(warmLoaded, coldLoaded, "and decodes the same classes")
  }
