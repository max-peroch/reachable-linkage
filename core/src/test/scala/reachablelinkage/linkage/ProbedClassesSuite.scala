package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/** Optional dependencies are usually probed before use: `Class.forName("x.Y")`, `isAvailable("x.Y")`, or a message that names the class (slf4j 1.7 and its
  * absent `StaticLoggerBinder`; jjwt and `android.util.Base64`). A missing class whose name is a string constant in reached code is labelled Probed. Exact
  * match only, reached code only, classes only: a heuristic that labels and never removes. Each scenario is also run, so a probed class really is skipped at
  * runtime.
  */
class ProbedClassesSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent
  private val Guarded   = Label.Guarded
  private val Probed    = Label.Probed

  private val opt = "lib/Opt.java" ->
    """package lib;
      |public class Opt {
      |  public static boolean has() {
      |    try { Class.forName("lib.Vanished"); return true; } catch (ClassNotFoundException e) { return false; }
      |  }
      |}""".stripMargin

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean
  )

  private def run(l: Label): Seq[Row] = Seq(("app.Main", "run", l))

  private val scenarios = Seq(
    Scenario("a probe in a reached library class", Map(opt), "if (lib.Opt.has()) { lib.Vanished.y(); }", run(Probed), failsAtRuntime = false),

    Scenario(
      "a probe in the same class",
      Map.empty,
      "boolean has = false; try { Class.forName(\"lib.Vanished\"); has = true; } catch (ClassNotFoundException e) { } if (has) { lib.Vanished.y(); }",
      run(Probed),
      failsAtRuntime = false
    ),

    Scenario(
      "the internal (slash) form counts too",
      Map.empty,
      "boolean has = false; try { Class.forName(\"lib/Vanished\".replace('/', '.')); has = true; } catch (ClassNotFoundException e) { } if (has) { lib.Vanished.y(); }",
      run(Probed),
      failsAtRuntime = false
    ),

    // ---- asking for one class of a library that is wholly absent asks whether the library is there ----
    Scenario(
      "when no class of the package is there, the other missing classes of it are probed too",
      Map(
        "lib/Opt2.java" -> "package lib; public class Opt2 { public static boolean has() { try { Class.forName(\"opt.First\"); return true; } catch (ClassNotFoundException e) { return false; } } }"
      ),
      "if (lib.Opt2.has()) { opt.Second.y(); }",
      run(Probed),
      failsAtRuntime = false
    ),

    // The library is the tree under a prefix of the package asked about: asking for `tree.a.First` is asking whether `tree` is there, so a missing class of
    // another sub-package (`tree.b`) is the same optional dependency, whatever its package, as long as nothing of that tree is on the classpath.
    Scenario(
      "when the whole tree is absent, a class of another sub-package is probed too",
      Map(
        "lib/Opt3.java" -> "package lib; public class Opt3 { public static boolean has() { try { Class.forName(\"tree.a.First\"); return true; } catch (ClassNotFoundException e) { return false; } } }"
      ),
      "if (lib.Opt3.has()) { tree.b.Second.y(); }",
      run(Probed),
      failsAtRuntime = false
    ),

    // `part` is still partly there (`part.c.Stay`): only the asked sub-package's own tree is absent, so a class of a sibling tree is a real break.
    Scenario(
      "a sibling sub-package of a tree that is partly there is not hidden",
      Map(
        "lib/Opt4.java" -> "package lib; public class Opt4 { public static boolean has() { try { Class.forName(\"part.a.First\"); return true; } catch (ClassNotFoundException e) { return false; } } }"
      ),
      "if (lib.Opt4.has()) { part.b.Second.y(); }",
      run(Reachable),
      failsAtRuntime = false
    ),

    // ---- what must not count ----
    // A package that is still partly on the classpath is a library whose version changed: a real break, which a probe of one class must not hide.
    Scenario(
      "a package that is still partly there has no siblings: that is version skew, not an absent library",
      Map(opt),
      "if (lib.Opt.has()) { lib.Sibling.y(); }",
      run(Reachable),
      failsAtRuntime = false
    ),

    Scenario(
      "a vanished class of a package that is still there, used for certain, stays a real failure next to a probe",
      Map(opt),
      "lib.Opt.has(); lib.Sibling.y();",
      run(Reachable),
      failsAtRuntime = true
    ),

    Scenario(
      "a class of another package is not a sibling",
      Map(
        "lib/Opt2.java" -> "package lib; public class Opt2 { public static boolean has() { try { Class.forName(\"opt.First\"); return true; } catch (ClassNotFoundException e) { return false; } } }"
      ),
      "if (lib.Opt2.has()) { other.Elsewhere.y(); }",
      run(Reachable),
      failsAtRuntime = false
    ),

    Scenario("a package nobody asked about has no siblings to speak of", Map.empty, "opt.Second.y();", run(Reachable), failsAtRuntime = true),

    Scenario(
      "a probe for a class that is there says nothing about the library's other classes",
      Map.empty,
      "try { Class.forName(\"lib.Gone\"); } catch (ClassNotFoundException e) { } opt.Second.y();",
      run(Reachable),
      failsAtRuntime = true
    ),

    Scenario("an unrelated string is not a probe", Map.empty, "String s = \"unrelated.Thing\"; lib.Vanished.y();", run(Reachable), failsAtRuntime = true),

    Scenario(
      "the name inside a longer message is not a probe",
      Map.empty,
      "String s = \"lib.Vanished is gone\"; lib.Vanished.y();",
      run(Reachable),
      failsAtRuntime = true
    ),

    Scenario(
      "a probe in a class nothing reaches does not count",
      Map("lib/Probe.java" -> "package lib; public class Probe { public static String n() { return \"lib.Vanished\"; } }"),
      "lib.Vanished.y();",
      run(Reachable),
      failsAtRuntime = true
    ),

    Scenario(
      "only a missing class can be probed, not a missing method",
      Map.empty,
      "Object o = \"lib.Gone\"; lib.Gone.x();",
      run(Reachable),
      failsAtRuntime = true
    ),

    // ---- precedence: Latent, then Guarded, then Probed ----
    Scenario(
      "a handler wins over a probe",
      Map(opt),
      "try { if (lib.Opt.has()) { lib.Vanished.y(); } } catch (NoClassDefFoundError e) { }",
      run(Guarded),
      failsAtRuntime = false
    ),

    Scenario(
      "unreached code stays Latent whatever it names",
      Map(opt, "lib/Util.java" -> "package lib; public class Util { public static void used() {} public static void unused() { Vanished.y(); } }"),
      "lib.Opt.has(); lib.Util.used();",
      Seq(("lib.Util", "unused", Latent)),
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
        (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label)
      .distinct
      .sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(
        s.lib + ("lib/Vanished.java" -> "package lib; public class Vanished { public static void y() {} }")
          + ("lib/Sibling.java"      -> "package lib; public class Sibling { public static void y() {} }")
          + ("opt/First.java"        -> "package opt; public class First { public static void y() {} }")
          + ("opt/Second.java"       -> "package opt; public class Second { public static void y() {} }")
          + ("other/Elsewhere.java"  -> "package other; public class Elsewhere { public static void y() {} }")
          + ("tree/a/First.java"     -> "package tree.a; public class First { public static void y() {} }")
          + ("tree/b/Second.java"    -> "package tree.b; public class Second { public static void y() {} }")
          + ("part/a/First.java"     -> "package part.a; public class First { public static void y() {} }")
          + ("part/b/Second.java"    -> "package part.b; public class Second { public static void y() {} }")
          + ("part/c/Stay.java"      -> "package part.c; public class Stay { public static void y() {} }"),
        removed =
          Set("lib/Vanished", "lib/Sibling", "opt/First", "opt/Second", "other/Elsewhere", "tree/a/First", "tree/b/Second", "part/a/First", "part/b/Second")
      )
      val app = compile(
        Map(
          "app/Main.java" ->
            s"package app; public class Main { public static void run() { ${s.body} } }"
        ),
        Seq(v1)
      )

      assertEquals(analyse(app, v2, Precision.Method), s.expected.sortBy(_.toString).toVector)

      // Probes need no call graph, so Class precision agrees, except that it has no Latent.
      val classRows = analyse(app, v2, Precision.Class)
      assertEquals(classRows.map(r => (r._1, r._2)), s.expected.map(r => (r._1, r._2)).sortBy(_.toString).toVector)
      assert(classRows.forall(_._3 != Latent))

      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(error.exists(_.isInstanceOf[LinkageError]), s"expected a linkage error, got $error")
        assert(analyse(app, v2, Precision.Method).exists(_._3 == Reachable), "a real failure had no Reachable conflict")
        assert(analyse(app, v2, Precision.MethodRta).exists(_._3 == Reachable), "a real failure had no Reachable conflict under RTA")
    }
