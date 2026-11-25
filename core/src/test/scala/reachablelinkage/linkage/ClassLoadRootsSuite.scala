package reachablelinkage.linkage

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.ClassLoadLog
import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RunLog, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * What a real run loaded as roots. Each scenario runs the app in a child JVM with `-Xlog:class+load`,
 * feeds the real log to the analysis, and also runs it in-process: a class the program really uses
 * that nothing in the bytecode names is missed without the log and found with it.
 */
class ClassLoadRootsSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val body: String,
      /** Rows per precision without the log, then with the log. */
      val withoutLog: Map[Precision, Seq[Row]],
      val withLog: Map[Precision, Seq[Row]],
      val failsAtRuntime: Boolean
  )

  private val svc    = "lib/Svc.java"   -> "package lib; public interface Svc { void run(); }"
  private val impl   = "lib/Impl.java"  -> "package lib; public class Impl implements Svc { public void run() { Gone.x(); } }"
  private val shape  = "lib/Shape.java" -> "package lib; public abstract class Shape { public abstract void area(); }"
  private val circle = "lib/Circle.java" -> "package lib; public class Circle extends Shape { public void area() { Gone.x(); } }"
  private val implRun   = ("lib.Impl", "run")
  private val circleArea = ("lib.Circle", "area")
  private def row(r: (String, String), l: Label): Row = (r._1, r._2, l)
  /** Class precision has no call graph, so it has no Reflective label; the Method levels say what only the log reaches. */
  private def byPrecision(classLevel: Seq[Row], methodLevels: Seq[Row]): Map[Precision, Seq[Row]] =
    Map(Precision.Class -> classLevel, Precision.Method -> methodLevels, Precision.MethodRta -> methodLevels)
  private def all(rows: Seq[Row]): Map[Precision, Seq[Row]] = Map(Precision.Class -> rows, Precision.Method -> rows, Precision.MethodRta -> rows)

  // A name the compiler cannot fold into a constant, so no instruction or string in the bytecode names the class.
  private def computed(parts: String*) = s"String.join(\"\", ${parts.map(p => "\"" + p + "\"").mkString(", ")})"

  private val scenarios = Seq(
    Scenario("a class loaded by a computed name is missed without the log and found with it", Map(svc, impl),
      s"try { ((lib.Svc) Class.forName(${computed("lib.", "Im", "pl")}).getDeclaredConstructor().newInstance()).run(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }",
      withoutLog = all(Seq.empty), withLog = byPrecision(Seq(row(implRun, Reachable)), Seq(row(implRun, Label.Reflective))), failsAtRuntime = true),

    Scenario("a class the run never loaded stays unexamined", Map(svc, impl),
      s"if (System.getProperty(\"rl.never\") != null) { try { ((lib.Svc) Class.forName(${computed("lib.", "Im", "pl")}).getDeclaredConstructor().newInstance()).run(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); } }",
      withoutLog = all(Seq.empty), withLog = all(Seq.empty), failsAtRuntime = false),

    // The class is named (instanceof), so it is reached; it is built by a name RTA cannot see, so RTA calls it
    // Latent until the log says a Circle was loaded.
    Scenario("an instantiation RTA cannot see is covered by the log", Map(shape, circle),
      s"try { lib.Shape s = (lib.Shape) Class.forName(${computed("lib.Ci", "rcle")}).getDeclaredConstructor().newInstance(); boolean b = s instanceof lib.Circle; s.area(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }",
      withoutLog = Map(Precision.Class -> Seq(row(circleArea, Reachable)), Precision.Method -> Seq(row(circleArea, Reachable)), Precision.MethodRta -> Seq(row(circleArea, Latent))),
      withLog = Map(Precision.Class -> Seq(row(circleArea, Reachable)), Precision.Method -> Seq(row(circleArea, Reachable)), Precision.MethodRta -> Seq(row(circleArea, Label.Reflective))), failsAtRuntime = true)
  )

  private def analyse(app: Path, lib: Path, precision: Precision, loaded: Set[String]): Vector[Row] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val locator = Locator(index)
    Checker.findings(index, scope, CheckOptions(precision, loaded = loaded)).map: f =>
      (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label)
    .distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(s.lib)
      val app = compile(Map(
        "app/Main.java"   -> s"package app; public class Main { public static void run() { ${s.body} } }",
        "app/Launch.java" -> "package app; public class Launch { public static void main(String[] a) { try { Main.run(); } catch (Throwable t) { } } }"), Seq(v1))

      val log    = RunLog.of(Seq(app, v2), "app.Launch")
      val loaded = ClassLoadLog.parse(log).fold(e => fail(e.message), _.classes.asScala.map(_.replace('.', '/')).toSet)

      for (precision, expected) <- s.withoutLog do
        assertEquals(analyse(app, v2, precision, Set.empty), expected.sortBy(_.toString).toVector, s"without the log, $precision")
      for (precision, expected) <- s.withLog do
        assertEquals(analyse(app, v2, precision, loaded), expected.sortBy(_.toString).toVector, s"with the log, $precision")

      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      // With the log, every real failure is Reachable (called) or Reflective (only the log reaches it) at every precision.
      if error.isDefined then
        for precision <- Seq(Precision.Class, Precision.Method, Precision.MethodRta) do
          assert(analyse(app, v2, precision, loaded).exists(r => r._3 == Reachable || r._3 == Label.Reflective), s"a real failure had no Reachable or Reflective conflict, $precision")
    }

  test("names that are not on the classpath, or not classes at all, change nothing") {
    val (v1, v2) = GoneLibrary.build(Map(svc, impl))
    val app = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { } }"), Seq(v1))
    assertEquals(analyse(app, v2, Precision.MethodRta, Set("ghost/Nope", "java/lang/Object", "", "lib/Missing")), Vector.empty)
  }
