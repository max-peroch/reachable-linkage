package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * A `catch` around a call also protects the code that call runs: a method is guarded against an
 * error if every call that can reach it sits inside a handler for that error (slf4j's `bind()`
 * guards `reportActualBinding` this way). Only Method precision has a call graph to follow, so
 * Class precision labels these Reachable. A root and a method handle's target can run from anywhere,
 * so they inherit nothing. Every scenario is run, so Guarded must really survive.
 */
class GuardPropagationSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Guarded   = Label.Guarded

  private val util  = "lib/Util.java" -> "package lib; public class Util { public static void risky() { Vanished.y(); } }"
  private val api   = "lib/Api.java" -> "package lib; public interface Api { void m(); }"
  private val impl  = "lib/Impl.java" -> "package lib; public class Impl implements Api { public void m() { Vanished.y(); } }"

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean
  )

  private val scenarios = Seq(
    Scenario("a callee of a guarded call is guarded", Map(util),
      "try { lib.Util.risky(); } catch (NoClassDefFoundError e) { }",
      Seq(("lib.Util", "risky", Guarded)), failsAtRuntime = false),

    Scenario("a second caller outside any handler removes the guard", Map(util),
      "try { lib.Util.risky(); } catch (LinkageError e) { } lib.Util.risky();",
      Seq(("lib.Util", "risky", Reachable)), failsAtRuntime = true),

    Scenario("the guard reaches through a chain of calls",
      Map("lib/Util.java" -> "package lib; public class Util { public static void risky() { deeper(); } static void deeper() { Vanished.y(); } }"),
      "try { lib.Util.risky(); } catch (NoClassDefFoundError e) { }",
      Seq(("lib.Util", "deeper", Guarded)), failsAtRuntime = false),

    Scenario("a handler for a different error is not inherited", Map(util),
      "try { lib.Util.risky(); } catch (NoSuchMethodError e) { }",
      Seq(("lib.Util", "risky", Reachable)), failsAtRuntime = true),

    Scenario("a static initialiser that guards its call", Map(util,
      "lib/Holder.java" -> "package lib; public class Holder { static { try { Util.risky(); } catch (NoClassDefFoundError e) { } } public static void touch() {} }"),
      "lib.Holder.touch();",
      Seq(("lib.Util", "risky", Guarded)), failsAtRuntime = false),

    // The lambda is made inside the try but run after it, so the handler does not cover its body.
    Scenario("a lambda made inside a handler but run outside it is not guarded",
      Map("lib/Task.java" -> "package lib; public class Task { public static Runnable make() { return () -> Vanished.y(); } }"),
      "Runnable r = null; try { r = lib.Task.make(); } catch (LinkageError e) { } r.run();",
      Seq(("lib.Task", "lambda$make", Reachable)), failsAtRuntime = true),

    Scenario("an interface call through a guarded site guards the implementation", Map(api, impl),
      "lib.Api a = new lib.Impl(); try { a.m(); } catch (LinkageError e) { }",
      Seq(("lib.Impl", "m", Guarded)), failsAtRuntime = false),

    Scenario("an unguarded call to the same interface method removes it", Map(api, impl),
      "lib.Api a = new lib.Impl(); try { a.m(); } catch (LinkageError e) { } a.m();",
      Seq(("lib.Impl", "m", Reachable)), failsAtRuntime = true),

    // `.m()` is seen while expanding run; Impl is only reached later, through the factory.
    Scenario("an override reached after the guarded call is seen is still guarded", Map(api, impl,
      "lib/Factory.java" -> "package lib; public class Factory { public static Api make() { return new Impl(); } }"),
      "try { lib.Factory.make().m(); } catch (LinkageError e) { }",
      Seq(("lib.Impl", "m", Guarded)), failsAtRuntime = false),

    // LIMITATION, pinned: the JDK calls compare, no call in the program does, so compare is a root
    // and inherits nothing from the `try` around Arrays.sort even though that handler does catch it.
    Scenario("a callback the JDK makes does not inherit the guard of the call that triggers it",
      Map("lib/Cmp.java" -> "package lib; public class Cmp implements java.util.Comparator<Integer> { public int compare(Integer a, Integer b) { Vanished.y(); return 0; } }"),
      "try { java.util.Arrays.sort(new Integer[] {2, 1}, new lib.Cmp()); } catch (LinkageError e) { }",
      Seq(("lib.Cmp", "compare", Reachable)), failsAtRuntime = false)
  )

  private def analyse(app: Path, lib: Path, precision: Precision): Vector[Row] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val locator = Locator(index)
    Checker.findings(index, scope, precision).map: f =>
      val method = locator.locate(f).methodName.replaceAll("\\$\\d+$", "")
      (syms.className(f.conflict.inClass).replace('/', '.'), method, f.label)
    .distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(
        s.lib + ("lib/Vanished.java" -> "package lib; public class Vanished { public static void y() {} }"),
        removed = Set("lib/Vanished"))
      val app = compile(Map("app/Main.java" ->
        s"package app; public class Main { public static void run() { ${s.body} } }"), Seq(v1))

      assertEquals(analyse(app, v2, Precision.Method), s.expected.sortBy(_.toString).toVector)

      // Class precision has no call graph: the same conflicts, but nothing is inherited.
      val classRows = analyse(app, v2, Precision.Class)
      assertEquals(classRows.map(r => (r._1, r._2)), s.expected.map(r => (r._1, r._2)).sortBy(_.toString).toVector)
      assert(classRows.forall(_._3 == Reachable), s"Class precision must not inherit guards: $classRows")

      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(error.exists(_.isInstanceOf[LinkageError]), s"expected a linkage error, got $error")
        assert(analyse(app, v2, Precision.Method).exists(_._3 == Reachable), "a real failure had no Reachable conflict")
        assert(analyse(app, v2, Precision.MethodRta).exists(_._3 == Reachable), "a real failure had no Reachable conflict under RTA")
    }
