package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{ChangedLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/** Access flags (JVMS 5.4.4; spotify #11): a member or class that still exists but is no longer usable from where it is used throws IllegalAccessError. Each
  * scenario is an app built against library v1, checked and run against v2, and the app is really run. The scenarios that must stay clean matter as much:
  * reporting a call that works would be worse than missing one.
  */
class AccessFlagsSuite extends munit.FunSuite:

  private type Row = (String, String, Label, String)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent
  private val Guarded   = Label.Guarded

  private val fooV1 =
    """package lib;
      |public class Foo {
      |  public static void bar() {}
      |  public void inst() {}
      |  public static int X = 1;
      |  public Foo() {}
      |}""".stripMargin

  private def foo(
      bar: String = "public static void bar() {}",
      inst: String = "public void inst() {}",
      x: String = "public static int X = 1;",
      ctor: String = "public Foo() {}",
      cls: String = "public class Foo"
  ) =
    s"package lib;\n$cls {\n  $bar\n  $inst\n  $x\n  $ctor\n}"

  private final class Scenario(
      val name: String,
      val body: String,
      val fooV2: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean,
      val extraApp: String = "",
      val extraLib: Map[String, String] = Map.empty
  )

  private def run(l: Label, kind: String): Seq[Row] = Seq(("app.Main", "run", l, kind))

  private val scenarios = Seq(
    // ---- things that break ----
    Scenario(
      "a public method became private",
      "lib.Foo.bar();",
      foo(bar = "private static void bar() {}"),
      run(Reachable, "InaccessibleMethod"),
      failsAtRuntime = true
    ),
    Scenario(
      "a public method became package-private",
      "lib.Foo.bar();",
      foo(bar = "static void bar() {}"),
      run(Reachable, "InaccessibleMethod"),
      failsAtRuntime = true
    ),
    Scenario(
      "a public method became protected and the caller is not a subclass",
      "lib.Foo.bar();",
      foo(bar = "protected static void bar() {}"),
      run(Reachable, "InaccessibleMethod"),
      failsAtRuntime = true
    ),
    Scenario(
      "a public field became private",
      "int v = lib.Foo.X;",
      foo(x = "private static int X = 1;"),
      run(Reachable, "InaccessibleField"),
      failsAtRuntime = true
    ),
    Scenario(
      "a public constructor became private",
      "new lib.Foo();",
      foo(ctor = "private Foo() {}"),
      run(Reachable, "InaccessibleMethod"),
      failsAtRuntime = true
    ),
    Scenario("a public class became package-private", "lib.Foo.bar();", foo(cls = "class Foo"), run(Reachable, "InaccessibleClass"), failsAtRuntime = true),
    Scenario(
      "a class the app extends became package-private",
      "new Sub();",
      foo(cls = "class Foo"),
      Seq(("app.Main$Sub", "", Reachable, "InaccessibleClass"), ("app.Main$Sub", "<init>", Reachable, "InaccessibleClass")),
      failsAtRuntime = true,
      extraApp = "static class Sub extends lib.Foo { }"
    ),

    // ---- things that must stay clean ----
    Scenario("access that did not change", "lib.Foo.bar(); int v = lib.Foo.X; new lib.Foo().inst();", fooV1, Seq.empty, failsAtRuntime = false),
    Scenario(
      "a protected method is usable from a subclass",
      "new Sub().go();",
      foo(inst = "protected void inst() {}"),
      Seq.empty,
      failsAtRuntime = false,
      extraApp = "static class Sub extends lib.Foo { void go() { inst(); } }"
    ),
    Scenario(
      "a package-private member is usable from its own package",
      "lib.Helper.go();",
      fooV1,
      Seq.empty,
      failsAtRuntime = false,
      extraLib = Map(
        "lib/Pkg.java"    -> "package lib; public class Pkg { static void pkg() {} }",
        "lib/Helper.java" -> "package lib; public class Helper { public static void go() { Pkg.pkg(); } }"
      )
    ),
    Scenario(
      "a private member is usable from a nestmate",
      "lib.Outer.Inner.go();",
      fooV1,
      Seq.empty,
      failsAtRuntime = false,
      extraLib = Map(
        "lib/Outer.java" ->
          "package lib; public class Outer { private static void secret() {} public static class Inner { public static void go() { secret(); } } }"
      )
    ),

    // ---- labels ----
    Scenario(
      "an access change in a method nothing calls is Latent",
      "lib.Util.used();",
      foo(bar = "private static void bar() {}"),
      Seq(("lib.Util", "unused", Latent, "InaccessibleMethod")),
      failsAtRuntime = false,
      extraLib = Map("lib/Util.java" -> "package lib; public class Util { public static void used() {} public static void unused() { Foo.bar(); } }")
    ),
    Scenario(
      "an IllegalAccessError handler makes it Guarded",
      "try { lib.Foo.bar(); } catch (IllegalAccessError e) { }",
      foo(bar = "private static void bar() {}"),
      run(Guarded, "InaccessibleMethod"),
      failsAtRuntime = false
    ),
    Scenario(
      "an IncompatibleClassChangeError handler, a superclass, also guards",
      "try { lib.Foo.bar(); } catch (IncompatibleClassChangeError e) { }",
      foo(bar = "private static void bar() {}"),
      run(Guarded, "InaccessibleMethod"),
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
        (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label, f.conflict.productPrefix)
      .distinct
      .sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = ChangedLibrary.build(Map("lib/Foo.java" -> fooV1) ++ s.extraLib, Map("lib/Foo.java" -> s.fooV2))
      val app      = compile(
        Map(
          "app/Main.java" ->
            s"package app; public class Main { public static void run() { ${s.body} } ${s.extraApp} }"
        ),
        Seq(v1)
      )

      assertEquals(analyse(app, v2, Precision.Method), s.expected.sortBy(_.toString).toVector)

      val classRows = analyse(app, v2, Precision.Class)
      assert(classRows.forall(_._3 != Latent))
      assertEquals(classRows.map(r => (r._1, r._2, r._4)), s.expected.map(r => (r._1, r._2, r._4)).sortBy(_.toString).toVector)

      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(error.exists(_.isInstanceOf[IllegalAccessError]), s"expected an IllegalAccessError, got $error")
        assert(analyse(app, v2, Precision.Method).exists(_._3 == Reachable), "a real failure had no Reachable conflict")
        assert(analyse(app, v2, Precision.MethodRta).exists(_._3 == Reachable), "a real failure had no Reachable conflict under RTA")
    }
