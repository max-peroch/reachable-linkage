package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/** Method-level reachability, checked three ways for every scenario: the labels are what we expect, Method precision reports exactly the conflicts Class
  * precision does (it only labels them), and the app is really run against the broken library, where anything that fails must have been labelled Reachable.
  * Every scenario is an app built against library v1 and checked and run with v2, in which `lib.Gone.x()` has been removed.
  */
class MethodPrecisionSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean,
      val extraAppMembers: String = ""
  )

  private val api = "lib/Api.java" -> "package lib; public interface Api { void m(); }"

  private val scenarios = Seq(
    Scenario("a direct call in the app", Map.empty, "lib.Gone.x();", Seq(("app.Main", "run", Reachable)), failsAtRuntime = true),

    Scenario(
      "a dependency method nothing calls",
      Map("lib/Util.java" -> "package lib; public class Util { public static void used() {} public static void unused() { Gone.x(); } }"),
      "lib.Util.used();",
      Seq(("lib.Util", "unused", Latent)),
      failsAtRuntime = false
    ),

    // The app's own methods are all roots: this is the code under test, so a dead one still counts.
    Scenario(
      "a method of the app that nothing calls",
      Map.empty,
      "",
      Seq(("app.Main", "dead", Reachable)),
      failsAtRuntime = false,
      extraAppMembers = "static void dead() { lib.Gone.x(); }"
    ),

    Scenario(
      "an interface call dispatching to an implementation",
      Map(api, "lib/Impl.java" -> "package lib; public class Impl implements Api { public void m() { Gone.x(); } }"),
      "lib.Api a = new lib.Impl(); a.m();",
      Seq(("lib.Impl", "m", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "an implementation nothing references is not even reached",
      Map(
        api,
        "lib/ImplA.java" -> "package lib; public class ImplA implements Api { public void m() { Gone.x(); } }",
        "lib/ImplB.java" -> "package lib; public class ImplB implements Api { public void m() { Gone.x(); } }"
      ),
      "lib.Api a = new lib.ImplA(); a.m();",
      Seq(("lib.ImplA", "m", Reachable)),
      failsAtRuntime = true
    ),

    // The call `.m()` is seen while expanding the app's `run`; Impl is only reached later, when the
    // factory's `make` is expanded. The dispatch must still find Impl.m.
    Scenario(
      "an implementation reached only after the call that dispatches to it",
      Map(
        api,
        "lib/Impl.java"    -> "package lib; public class Impl implements Api { public void m() { Gone.x(); } }",
        "lib/Factory.java" -> "package lib; public class Factory { public static Api make() { return new Impl(); } }"
      ),
      "lib.Factory.make().m();",
      Seq(("lib.Impl", "m", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a method inherited from a superclass",
      Map(
        "lib/Base.java" -> "package lib; public class Base { public void m() { Gone.x(); } }",
        "lib/Sub.java"  -> "package lib; public class Sub extends Base { }"
      ),
      "new lib.Sub().m();",
      Seq(("lib.Base", "m", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a default method inherited from an interface",
      Map(
        "lib/Dflt.java"     -> "package lib; public interface Dflt { default void hi() { Gone.x(); } }",
        "lib/DfltImpl.java" -> "package lib; public class DfltImpl implements Dflt { }"
      ),
      "new lib.DfltImpl().hi();",
      Seq(("lib.Dflt", "hi", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a lambda body reached through its invokedynamic handle",
      Map(
        "lib/Task.java" ->
          """package lib;
          |public class Task {
          |  public static Runnable make() { return () -> Gone.x(); }
          |  // A different body: javac 17 merges identical lambda bodies into one method, which would make this one reachable too.
  public static Runnable unusedMake() { return () -> { Gone.x(); Gone.x(); }; }
          |}""".stripMargin
      ),
      "lib.Task.make().run();",
      Seq(("lib.Task", "lambda$make", Reachable), ("lib.Task", "lambda$unusedMake", Latent)),
      failsAtRuntime = true
    ),

    // `Base` does not implement `I`, so it is not a subtype of the call's owner; the implementation `Impl` runs is the one it inherits from `Base`.
    Scenario(
      "an implementation inherited from a superclass that is not a subtype of the interface",
      Map(
        "lib/I.java"    -> "package lib; public interface I { void m(); }",
        "lib/Base.java" -> "package lib; public class Base { public void m() { Gone.x(); } }",
        "lib/Impl.java" -> "package lib; public class Impl extends Base implements I { }"
      ),
      "lib.I i = new lib.Impl(); i.m();",
      Seq(("lib.Base", "m", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a static initialiser runs when the class is used",
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public static int v = 1; static { Gone.x(); } }"),
      "int y = lib.Cfg.v;",
      Seq(("lib.Cfg", "<clinit>", Reachable)),
      failsAtRuntime = true
    ),

    // The JDK calls compare; no code in the program does. Methods overriding JDK methods are roots.
    Scenario(
      "an override the JDK calls back",
      Map(
        "lib/Cmp.java" ->
          "package lib; public class Cmp implements java.util.Comparator<Integer> { public int compare(Integer a, Integer b) { Gone.x(); return 0; } }"
      ),
      "Integer[] xs = {2, 1}; java.util.Arrays.sort(xs, new lib.Cmp());",
      Seq(("lib.Cmp", "compare", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a serialization hook is assumed to be called",
      Map(
        "lib/Ser.java" ->
          "package lib; public class Ser implements java.io.Serializable { private void readObject(java.io.ObjectInputStream in) { Gone.x(); } }"
      ),
      "Object o = new lib.Ser();",
      Seq(("lib.Ser", "readObject", Reachable)),
      failsAtRuntime = false
    ),

    // Known over-approximation: any referenced class counts as instantiated. RTA will make this Latent.
    Scenario(
      "a class only mentioned by instanceof is still treated as instantiated",
      Map(
        api,
        "lib/ImplA.java" -> "package lib; public class ImplA implements Api { public void m() { } }",
        "lib/ImplC.java" -> "package lib; public class ImplC implements Api { public void m() { Gone.x(); } }"
      ),
      "lib.Api a = new lib.ImplA(); a.m(); Object o = a; if (o instanceof lib.ImplC) { }",
      Seq(("lib.ImplC", "m", Reachable)),
      failsAtRuntime = false
    )
  )

  private def analyse(app: Path, lib: Path, precision: Precision): Vector[Row] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val locator = Locator(index)
    Checker
      .findings(index, scope, precision)
      .map: f =>
        val method = locator.locate(f).methodName.replaceAll("\\$\\d+$", "")
        (syms.className(f.conflict.inClass).replace('/', '.'), method, f.label)
      .distinct
      .sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(s.lib)
      val app      = compile(
        Map(
          "app/Main.java" ->
            s"package app; public class Main { public static void run() { ${s.body} } ${s.extraAppMembers} }"
        ),
        Seq(v1)
      )

      // 1. The labels.
      assertEquals(analyse(app, v2, Precision.Method), s.expected.sortBy(_.toString).toVector)

      // 2. Method precision only labels: the conflicts are the ones Class precision reports.
      val classRows = analyse(app, v2, Precision.Class)
      assert(classRows.forall(_._3 == Reachable), "Class precision labels everything Reachable")
      assertEquals(classRows.map(r => (r._1, r._2)), s.expected.map(r => (r._1, r._2)).sortBy(_.toString).toVector)

      // 3. The JVM agrees: a run that fails must have been predicted as Reachable.
      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(error.exists(_.isInstanceOf[LinkageError]), s"expected a linkage error, got $error")
        assert(analyse(app, v2, Precision.Method).exists(_._3 == Reachable), "a real failure had no Reachable conflict")
        assert(analyse(app, v2, Precision.MethodRta).exists(_._3 == Reachable), "a real failure had no Reachable conflict under RTA")
    }
