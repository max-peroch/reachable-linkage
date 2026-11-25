package reachablelinkage.linkage

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.ScalaFixture.{compile, librarySources, source}
import reachablelinkage.fixture.ScalaVersion.*
import reachablelinkage.fixture.{FixtureDirs, RuntimeOracle, ScalaFixture, ScalaVersion, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * Method-level reachability on what scalac really emits, with 2.12, 2.13 and Scala 3. As in
 * `MethodPrecisionSuite`, each scenario is an app built against library v1 and then checked and run
 * with a v2 in which `lib.Gone.x()` is gone, and a run that really fails must have been labelled
 * Reachable. The Scala library is on the classpath and the call graph walks through it.
 */
class ScalaMethodPrecisionSuite extends munit.FunSuite:

  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(15, "min")

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent
  private val Deserialized = Label.Deserialized

  // One library for every scenario: a class only counts once the scenario's app reaches it.
  private val libSource =
    """package lib
      |
      |object Util { def used(): Unit = (); def unused(): Unit = Gone.x() }
      |
      |trait Greeter { def hello(): Unit = (); def bye(): Unit = Gone.x() }
      |class Impl extends Greeter
      |
      |trait Loud { def shout(): Unit = Gone.x() }
      |class LoudImpl extends Loud
      |
      |object Task {
      |  def make(): () => Unit = () => Gone.x()
      |  def unusedMake(): () => Unit = () => Gone.x()
      |}
      |
      |case class P(a: Int) { def boom(): Unit = Gone.x() }
      |
      |object Cfg { val v: Int = { Gone.x(); 1 } }
      |object L { lazy val v: Int = { Gone.x(); 1 } }
      |
      |class M(val v: Int) extends AnyVal { def double: Int = { Gone.x(); v * 2 } }
      |
      |object Fns { val f: Int => Unit = _ => Gone.x() }
      |
      |class Cmp extends java.util.Comparator[Integer] { def compare(a: Integer, b: Integer): Int = { Gone.x(); 0 } }
      |""".stripMargin

  private val goneV1 = "package lib\nobject Gone { def x(): Unit = () }"
  private val goneV2 = "package lib\nobject Gone { }"

  /** The library with `Gone.x()` (to compile the app against) and the same classes with it removed. */
  private final class Libraries(val v1: Path, val v2: Path)

  private val libraries = new java.util.concurrent.ConcurrentHashMap[ScalaVersion, Libraries]()

  private def librariesFor(v: ScalaVersion): Libraries =
    libraries.computeIfAbsent(v, _ =>
      val v1 = compile(v, Map("lib/Gone.scala" -> goneV1, "lib/Lib.scala" -> libSource))
      val v2 = FixtureDirs.fresh().resolve("v2")
      val stream = Files.walk(v1)
      try stream.iterator.asScala.foreach: p =>
        val target = v2.resolve(v1.relativize(p).toString)
        if Files.isDirectory(p) then Files.createDirectories(target) else Files.copy(p, target)
      finally stream.close()
      // Replace every class file of Gone, however scalac names the companion/module pair.
      Files.list(v2.resolve("lib")).iterator.asScala.filter(_.getFileName.toString.startsWith("Gone")).foreach(Files.delete)
      val gone = compile(v, Map("lib/Gone.scala" -> goneV2))
      Files.list(gone.resolve("lib")).iterator.asScala.foreach(p => Files.copy(p, v2.resolve("lib").resolve(p.getFileName.toString)))
      Libraries(v1, v2)
    )

  /** What to expect. `exactFor` gives the rows per Scala version (most are the same everywhere); `mustReach` names classes that must have a Reachable row. */
  private final class Scenario(
      val name: String,
      val body: String,
      val exactFor: ScalaVersion => Seq[Row],
      val failsAtRuntime: Boolean,
      val mustReach: Set[String] = Set.empty,
      val extra: String = ""
  )

  private def same(rows: Row*): ScalaVersion => Seq[Row] = _ => rows

  // The app consumes values through `use`, so scalac cannot drop a pure-looking expression
  // (Scala 3 discards a case class `apply` in statement position, and with it the reference).
  private val scenarios = Seq(
    Scenario("a direct call in the app", "lib.Gone.x()", same(("app.Main$", "run", Reachable)), failsAtRuntime = true),

    Scenario("a method of a companion object that nothing calls", "lib.Util.used()",
      same(("lib.Util$", "unused", Latent)), failsAtRuntime = false),

    Scenario("a concrete trait method called through a class that mixes it in", "new lib.LoudImpl().shout()",
      same(("lib.Loud", "shout", Reachable)), failsAtRuntime = true),

    Scenario("a trait mixed into a class of the app", "new A().shout()",
      same(("lib.Loud", "shout", Reachable)), failsAtRuntime = true, extra = "class A extends lib.Loud"),

    Scenario("a trait method nothing calls", "new lib.Impl().hello()",
      same(("lib.Greeter", "bye", Latent)), failsAtRuntime = false),

    // Scala function literals are serializable, so every class with one has $deserializeLambda$, which mentions every lambda of
    // the class and is a root after the direct and rule walks: the lambda nothing creates can still be rebuilt from bytes, so it is Deserialized.
    Scenario("a lambda that is created and one that is not", "lib.Task.make()()",
      same(("lib.Task$", "lambda", Reachable), ("lib.Task$", "lambda", Deserialized)), failsAtRuntime = true),

    Scenario("a case class method that nothing calls", "use(lib.P(1))",
      same(("lib.P", "boom", Latent)), failsAtRuntime = false),

    Scenario("a case class method that is called", "lib.P(1).boom()",
      same(("lib.P", "boom", Reachable)), failsAtRuntime = true),

    // 2.12 runs a module's initialiser in <init>; 2.13 and Scala 3 run it in <clinit>.
    Scenario("an object whose initialiser breaks", "use(lib.Cfg.v)",
      { case V212 => Seq(("lib.Cfg$", "<init>", Reachable)); case _ => Seq(("lib.Cfg$", "<clinit>", Reachable)) },
      failsAtRuntime = true),

    // The lazy val machinery differs a lot between 2.12, 2.13 and Scala 3, so only the class is checked.
    Scenario("a lazy val", "use(lib.L.v)", same(), failsAtRuntime = true, mustReach = Set("lib.L$")),

    Scenario("a value class method", "use(new lib.M(2).double)",
      same(("lib.M$", "double$extension", Reachable)), failsAtRuntime = true),

    Scenario("a function stored in an object and run by the Scala library", "List(1).foreach(lib.Fns.f)",
      same(("lib.Fns$", "lambda", Reachable)), failsAtRuntime = true),

    Scenario("an override the JDK calls back, from Scala", "java.util.Arrays.sort(Array[Integer](2, 1), new lib.Cmp)",
      same(("lib.Cmp", "compare", Reachable)), failsAtRuntime = true)
  )

  private def normalise(method: String): String =
    if method.contains("$anonfun") then "lambda" else method.replaceAll("\\$\\d+$", "")

  private def analyse(v: ScalaVersion, app: Path, lib: Path, precision: Precision): Vector[Row] =
    val syms  = new Symbols
    val scala = librarySources(v)
    val index = ClassIndex((Vector(source(app), source(lib)) ++ scala :+ ClassSource.jdk()), syms)
    // The app and the library are checked; the Scala library is only walked through.
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), (0 until 2 + scala.length).toVector)
    val locator = Locator(index)
    Checker.findings(index, scope, precision).map: f =>
      (syms.className(f.conflict.inClass).replace('/', '.'), normalise(locator.locate(f).methodName), f.label)
    .distinct.sortBy(_.toString)

  for v <- ScalaVersion.values; s <- scenarios do
    test(s"[Scala $v] ${s.name}") {
      val libs = librariesFor(v)
      val app = compile(v, Map("app/Main.scala" ->
        s"package app\n${s.extra}\nobject Main { def use(x: Any): Unit = (); def run(): Unit = { ${s.body} } }"), Seq(libs.v1))

      val rows = analyse(v, app, libs.v2, Precision.Method)
      val expected = s.exactFor(v).sortBy(_.toString).toVector
      assertEquals(rows.filterNot(r => s.mustReach(r._1)), expected, s"all rows: $rows")
      s.mustReach.foreach(c => assert(rows.exists(r => r._1 == c && r._3 == Reachable), s"no Reachable row in $c: $rows"))

      // Method precision labels exactly the conflicts Class precision reports.
      val classRows = analyse(v, app, libs.v2, Precision.Class)
      assert(classRows.forall(_._3 == Reachable))
      assertEquals(classRows.map(r => (r._1, r._2)).distinct, rows.map(r => (r._1, r._2)).distinct)

      // The JVM agrees: a failing run must have been predicted as Reachable.
      val error = RuntimeOracle.run(Seq(app, libs.v2) ++ ScalaFixture.library(v))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(error.exists(_.isInstanceOf[LinkageError]), s"expected a linkage error, got $error")
        assert(rows.exists(_._3 == Reachable), s"a real failure had no Reachable conflict: $rows")
        assert(analyse(v, app, libs.v2, Precision.MethodRta).exists(_._3 == Reachable), "a real failure had no Reachable conflict under RTA")
    }
