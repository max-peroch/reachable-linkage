package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/** Rapid type analysis: a virtual or interface call lands only in classes something instantiates. Each scenario states what class hierarchy analysis (Method)
  * and RTA (MethodRta) say, and is run on the JVM: whatever really fails must be Reachable under both, so RTA may only turn Reachable into Latent for code that
  * really cannot run.
  */
class RtaSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val body: String,
      val cha: Seq[Row],
      val rta: Seq[Row],
      val failsAtRuntime: Boolean,
      val resources: Map[String, String] = Map.empty
  )

  private val shape  = "lib/Shape.java"  -> "package lib; public abstract class Shape { public abstract void area(); }"
  private val circle = "lib/Circle.java" -> "package lib; public class Circle extends Shape { public void area() { Gone.x(); } }"
  private val square = "lib/Square.java" -> "package lib; public class Square extends Shape { public void area() { } }"
  private val svc    = "lib/Svc.java"    -> "package lib; public interface Svc { void run(); }"
  private val impl   = "lib/Impl.java"   -> "package lib; public class Impl implements Svc { public void run() { Gone.x(); } }"
  private val other  = "lib/Other.java"  -> "package lib; public class Other implements Svc { public void run() { } }"

  private val circleArea                              = ("lib.Circle", "area")
  private val implRun                                 = ("lib.Impl", "run")
  private def row(r: (String, String), l: Label): Row = (r._1, r._2, l)

  private val scenarios = Seq(
    Scenario(
      "a subclass that is referenced but never instantiated",
      Map(shape, circle, square),
      "lib.Shape s = new lib.Square(); s.area(); boolean c = s instanceof lib.Circle;",
      cha = Seq(row(circleArea, Reachable)),
      rta = Seq(row(circleArea, Latent)),
      failsAtRuntime = false
    ),

    Scenario(
      "a subclass instantiated after another one",
      Map(shape, circle, square),
      "Object first = new lib.Square(); lib.Shape s = new lib.Circle(); s.area();",
      cha = Seq(row(circleArea, Reachable)),
      rta = Seq(row(circleArea, Reachable)),
      failsAtRuntime = true
    ),

    // The lambda body is a later method of Main than `run`, which already has a `new`: the merge must start at that method.
    Scenario(
      "a subclass instantiated in a later method of the class",
      Map(shape, circle, square),
      "Object first = new lib.Square(); Runnable r = () -> { lib.Shape s = new lib.Circle(); s.area(); }; r.run();",
      cha = Seq(row(circleArea, Reachable)),
      rta = Seq(row(circleArea, Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "the same subclass once it is instantiated",
      Map(shape, circle, square),
      "lib.Shape s = new lib.Circle(); s.area();",
      cha = Seq(row(circleArea, Reachable)),
      rta = Seq(row(circleArea, Reachable)),
      failsAtRuntime = true
    ),

    // Leaf is built; the call lands in Mid.area, which nothing builds: the target is looked up from Leaf.
    Scenario(
      "an inherited method counts when only the subclass is instantiated",
      Map(
        shape,
        "lib/Mid.java"  -> "package lib; public class Mid extends Shape { public void area() { Gone.x(); } }",
        "lib/Leaf.java" -> "package lib; public class Leaf extends Mid { }"
      ),
      "lib.Shape s = new lib.Leaf(); s.area();",
      cha = Seq(("lib.Mid", "area", Reachable)),
      rta = Seq(("lib.Mid", "area", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "an interface implementation that is referenced but never instantiated",
      Map(svc, impl, other),
      "lib.Svc s = new lib.Other(); s.run(); Object o = lib.Impl.class;",
      cha = Seq(row(implRun, Reachable)),
      rta = Seq(row(implRun, Latent)),
      failsAtRuntime = false
    ),

    Scenario(
      "an interface default method is the target for an implementation that inherits it",
      Map(
        "lib/Svc.java"  -> "package lib; public interface Svc { default void run() { Gone.x(); } }",
        "lib/Impl.java" -> "package lib; public class Impl implements Svc { }"
      ),
      "lib.Svc s = new lib.Impl(); s.run();",
      cha = Seq(("lib.Svc", "run", Reachable)),
      rta = Seq(("lib.Svc", "run", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a class built by reflection counts as instantiated",
      Map(svc, impl, other),
      "try { lib.Svc s = (lib.Svc) Class.forName(\"lib.Impl\").getDeclaredConstructor().newInstance(); s.run(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }",
      cha = Seq(row(implRun, Label.Reflective)),
      rta = Seq(row(implRun, Label.Reflective)),
      failsAtRuntime = true
    ),

    // No no-argument constructor to root: the name alone says the framework builds it.
    Scenario(
      "a class built by reflection through another constructor counts as instantiated",
      Map(svc, "lib/Impl.java" -> "package lib; public class Impl implements Svc { public Impl(String s) { } public void run() { Gone.x(); } }", other),
      "try { lib.Svc s = (lib.Svc) Class.forName(\"lib.Impl\").getConstructor(String.class).newInstance(\"x\"); s.run(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }",
      cha = Seq(row(implRun, Label.Reflective)),
      rta = Seq(row(implRun, Label.Reflective)),
      failsAtRuntime = true
    ),

    Scenario(
      "a constructor reference instantiates",
      Map(svc, impl),
      "java.util.function.Supplier<lib.Svc> s = lib.Impl::new; s.get().run();",
      cha = Seq(row(implRun, Reachable)),
      rta = Seq(row(implRun, Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a ServiceLoader provider counts as instantiated",
      Map(svc, impl),
      "for (lib.Svc s : java.util.ServiceLoader.load(lib.Svc.class, Main.class.getClassLoader())) s.run();",
      cha = Seq(row(implRun, Reachable)),
      rta = Seq(row(implRun, Reachable)),
      failsAtRuntime = true,
      resources = Map("META-INF/services/lib.Svc" -> "lib.Impl\n")
    ),

    // The JDK builds a Serializable class without a visible `new`, so a class with a hook is assumed built.
    Scenario(
      "a class with a serialization hook is assumed to be deserialised",
      Map(
        "lib/Hooked.java" -> "package lib; public class Hooked implements java.io.Serializable { private void readObject(java.io.ObjectInputStream in) { Gone.x(); } }"
      ),
      "Object o = lib.Hooked.class;",
      cha = Seq(("lib.Hooked", "readObject", Reachable)),
      rta = Seq(("lib.Hooked", "readObject", Reachable)),
      failsAtRuntime = false
    ),

    // The call is on `Base`; the default that runs is `Fn`'s override, which the lambda's class inherits. Nothing says `new` for that class
    // (Scala's `JFunction1$mcII$sp.apply(Object)` bridge is this shape).
    Scenario(
      "a default method of a sub-interface, run on a lambda through the super-interface",
      Map(
        "lib/Base.java" -> "package lib; public interface Base { void go(); }",
        "lib/Fn.java"   -> "package lib; public interface Fn extends Base { void run(); default void go() { Gone.x(); } }"
      ),
      "lib.Base b = (lib.Fn) () -> { }; b.go();",
      cha = Seq(("lib.Fn", "go", Reachable)),
      rta = Seq(("lib.Fn", "go", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a lambda body is reached through its handle, not through dispatch",
      Map(svc),
      "Runnable r = () -> lib.Gone.x(); r.run();",
      cha = Seq(("app.Main", "lambda$run$0", Reachable)),
      rta = Seq(("app.Main", "lambda$run$0", Reachable)),
      failsAtRuntime = true
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
        (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label)
      .distinct
      .sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(s.lib, s.resources)
      val app      = compile(
        Map(
          "app/Main.java" ->
            s"package app; public class Main { public static void run() { ${s.body} } }"
        ),
        Seq(v1)
      )
      assertEquals(analyse(app, v2, Precision.Method), s.cha.sortBy(_.toString).toVector, "class hierarchy analysis")
      val rta = analyse(app, v2, Precision.MethodRta)
      assertEquals(rta, s.rta.sortBy(_.toString).toVector, "rapid type analysis")
      assertEquals(analyse(app, v2, Precision.Class).map(r => (r._1, r._2)), rta.map(r => (r._1, r._2)), "the same conflicts, only labelled")

      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(error.exists(_.isInstanceOf[LinkageError]), s"expected a linkage error, got $error")
        assert(rta.exists(r => r._3 == Reachable || r._3 == Label.Reflective), s"a real failure had no Reachable or Reflective conflict under RTA: $rta")
    }

  test("Checker.findings labels by default (Method precision), and Checker.check stays class-level") {
    val (v1, v2) =
      GoneLibrary.build(Map("lib/Util.java" -> "package lib; public class Util { public static void used() { } public static void unused() { Gone.x(); } }"))
    val app   = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.used(); } }"), Seq(v1))
    val syms  = new Symbols
    val index = ClassIndex(Vector(source(app), source(v2), ClassSource.jdk()), syms)
    val scope = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    assertEquals(Checker.findings(index, scope).map(_.label).distinct, Vector(Label.Latent))
    assertEquals(Checker.findings(index, scope, Precision.Class).map(_.label).distinct, Vector(Label.Reachable))
    assertEquals(Checker.check(index, scope).size, 1)
  }

  test("CheckOptions defaults to Method precision, the standard rules and no loaded classes") {
    assertEquals(CheckOptions(), CheckOptions(Precision.Method, Keep.standard, Set.empty))
    assertEquals(CheckOptions(precision = Precision.MethodRta).keep, Keep.standard)
  }
