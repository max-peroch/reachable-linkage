package reachablelinkage.linkage

import java.nio.file.Paths

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.symbols.Symbols

/** Method precision on real code: scan a whole classpath (every class is checked, as with `missinglinkScanDependencies := true`) but measure reachability from
  * the compiler jar alone, the way an application depends on its libraries. Method precision must report exactly the conflicts Class precision does, and the
  * call graph is what separates the ones the app can hit.
  */
class RealJarsMethodPrecisionSuite extends munit.FunSuite:
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  private val props =
    val p  = new java.util.Properties()
    val in = getClass.getResourceAsStream("/scala-compilers.properties")
    try p.load(in)
    finally in.close()
    p

  // (conflicts at Class precision, of which reachable from the compiler jar). The 2.12 ones are inside the
  // compiler jar itself, so they are the application's own code; the rest are optional library parts
  // (jline's JNA support and the like) the compiler never calls into. The Scala 3 counts also include
  // abstract `$outer` accessors of inner traits that the compiler leaves unimplemented (see below).
  // 2.13 was 118 before the classes a lambda call site names (its descriptor and the bootstrap's method types) counted as references: jline's optional
  // SSH built-ins use Apache SSHD types in lambdas, which the classpath does not have, so five more rows, none reachable.
  // 2.12 was (75, 75) before a missing class counted as probed when its whole library tree is absent: the 62 conflicts in the compiler's Ant tasks
  // (`org.apache.tools.ant`, absent as a whole, asked for by name) became `Probed`; the 13 left are its JLine 2 REPL classes.
  private val expected = Map("2.12" -> (75, 13), "2.13" -> (123, 0), "3-lts" -> (89, 0), "3-next" -> (106, 0))

  for key <- Seq("2.12", "2.13", "3-lts", "3-next") do
    test(s"Scala $key compiler classpath: Method precision only labels Class precision's conflicts") {
      val srcs  = props.getProperty(key).split(java.io.File.pathSeparator).toSeq.map(j => ClassSource.open(Paths.get(j)).toOption.get)
      val syms  = new Symbols
      val index = ClassIndex((srcs :+ ClassSource.jdk()).toIndexedSeq, syms)
      val all   = srcs.indices.toVector
      // Entry 0 is the compiler jar: the application. Everything else is a library.
      val scope = Scope(index, all, all, all, Vector(0)).fold(e => throw new AssertionError(e.message), identity)

      val byClass  = Checker.findings(index, scope, Precision.Class)
      val t0       = System.nanoTime
      val byMethod = Checker.findings(index, scope, Precision.Method)
      val ms       = (System.nanoTime - t0) / 1_000_000

      assertEquals(byMethod.map(_.conflict), byClass.map(_.conflict), "same conflicts, only labelled")
      // Rapid type analysis only narrows: nothing is Reachable under it that is not under class hierarchy analysis.
      val byRta = Checker.findings(index, scope, Precision.MethodRta)
      assertEquals(byRta.map(_.conflict), byClass.map(_.conflict), "same conflicts under RTA")
      val reachableCha = byMethod.filter(_.label == Label.Reachable).map(_.conflict).toSet
      assert(byRta.filter(_.label == Label.Reachable).forall(f => reachableCha.contains(f.conflict)), "RTA made something reachable")
      println(s"[rta] scala $key: reachable ${reachableCha.size} (CHA) -> ${byRta.filter(_.label == Label.Reachable).map(_.conflict).distinct.size} (RTA)")
      val conflicts = byClass.map(_.conflict).distinct
      val reachable = byMethod.filter(_.label == Label.Reachable).map(_.conflict).distinct
      val latent    = byMethod.filter(_.label == Label.Latent).map(_.conflict).distinct.filterNot(reachable.contains)
      println(s"[method] scala $key: conflicts=${conflicts.size} reachable=${reachable.size} latent-only=${latent.size} (method precision ${ms} ms)")
      // A conflict in a class reached only by a constant class name (jline loads its Nano builtin that
      // way) is Guarded here, not Latent: its reference sits inside a handler.
      val other = byMethod.filter(f => f.label == Label.Guarded || f.label == Label.Probed).map(_.conflict).distinct.filterNot(reachable.contains)
      assertEquals(reachable.size + latent.size + other.size, conflicts.size)

      // Scala 3 declares an abstract `$outer` accessor in an inner trait and some implementing classes
      // have none under that name (they have their own, differently named). Nothing calls it, so it is
      // harmless, and every one here is Latent. Any other unimplemented abstract method is a surprise.
      val abstracts = conflicts.collect { case c: Conflict.AbstractMethodNotImplemented => syms.name(c.name) }
      assert(abstracts.forall(_.endsWith("$$$outer")), s"unexpected unimplemented abstract methods: ${abstracts.filterNot(_.endsWith("$$$outer")).take(10)}")
      assert(reachable.forall(!_.isInstanceOf[Conflict.AbstractMethodNotImplemented]), "an unimplemented abstract method is Reachable")
      assertEquals((conflicts.size, reachable.size), expected(key))
    }
