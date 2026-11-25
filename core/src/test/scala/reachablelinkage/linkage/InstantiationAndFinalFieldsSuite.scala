package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{ChangedLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.{ClassId, Symbols}

/**
 * `new` of a class that became abstract or an interface (`InstantiationError`), and a write to a field
 * that became `final` from another class (`IllegalAccessError`). Each scenario builds an app against
 * v1, runs it on the JVM against v2, and requires that a real failure was labelled Reachable.
 */
class InstantiationAndFinalFieldsSuite extends munit.FunSuite:

  private type Row = (String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent
  private val Guarded   = Label.Guarded

  private final class Scenario(
      val name: String,
      val v1: Map[String, String],
      val v2: Map[String, String],
      val app: Map[String, String],
      val expected: Seq[Row],
      val failsAtRuntime: Boolean
  )

  private def main(body: String) = "app/Main.java" -> s"package app; public class Main { public static void run() { $body } }"

  private val thing = Map("lib/Thing.java" -> "package lib; public class Thing { }")

  private val scenarios = Seq(
    Scenario("new of a class that became abstract", thing,
      Map("lib/Thing.java" -> "package lib; public abstract class Thing { }"),
      Map(main("new lib.Thing();")),
      Seq(("AbstractInstantiated(lib.Thing,app.Main,Abstract)", Reachable)), failsAtRuntime = true),

    // The abstract class is the second `new` in the method, so the lists of type references and of `new`s must stay aligned.
    Scenario("a later new of a class that became abstract, after an ordinary one",
      thing + ("lib/Fine.java" -> "package lib; public class Fine { }"),
      Map("lib/Thing.java" -> "package lib; public abstract class Thing { }"),
      Map(main("Object o = new lib.Fine(); Object c = lib.Fine.class; new lib.Thing();")),
      Seq(("AbstractInstantiated(lib.Thing,app.Main,Abstract)", Reachable)), failsAtRuntime = true),

    Scenario("new of a class that became an interface", thing,
      Map("lib/Thing.java" -> "package lib; public interface Thing { }"),
      Map(main("new lib.Thing();")),
      Seq(("AbstractInstantiated(lib.Thing,app.Main,Interface)", Reachable)), failsAtRuntime = true),

    Scenario("an InstantiationError handler guards the new", thing,
      Map("lib/Thing.java" -> "package lib; public abstract class Thing { }"),
      Map(main("try { new lib.Thing(); } catch (InstantiationError e) { }")),
      Seq(("AbstractInstantiated(lib.Thing,app.Main,Abstract)", Guarded)), failsAtRuntime = false),

    Scenario("so does a handler for its superclass IncompatibleClassChangeError", thing,
      Map("lib/Thing.java" -> "package lib; public abstract class Thing { }"),
      Map(main("try { new lib.Thing(); } catch (IncompatibleClassChangeError e) { }")),
      Seq(("AbstractInstantiated(lib.Thing,app.Main,Abstract)", Guarded)), failsAtRuntime = false),

    Scenario("a concrete subclass of an abstract class can be built",
      Map("lib/Base.java" -> "package lib; public abstract class Base { }", "lib/Sub.java" -> "package lib; public class Sub extends Base { }"),
      Map("lib/Other.java" -> "package lib; public class Other { }"),
      Map(main("new lib.Sub();")),
      Seq.empty, failsAtRuntime = false),

    Scenario("a class that is still concrete is not reported", thing,
      Map("lib/Other.java" -> "package lib; public class Other { }"),
      Map(main("new lib.Thing();")),
      Seq.empty, failsAtRuntime = false),

    Scenario("an instance field that became final, written from another class",
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public int n; }"),
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public final int n; public Cfg() { n = 1; } }"),
      Map(main("new lib.Cfg().n = 5;")),
      Seq(("FinalFieldWritten(lib.Cfg,n,app.Main)", Reachable)), failsAtRuntime = true),

    Scenario("a static field that became final, written from another class",
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public static int s; }"),
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public static final int s; static { s = 1; } }"),
      Map(main("lib.Cfg.s = 5;")),
      Seq(("FinalFieldWritten(lib.Cfg,s,app.Main)", Reachable)), failsAtRuntime = true),

    Scenario("an inherited field that became final, written by a subclass",
      Map("lib/Base.java" -> "package lib; public class Base { protected int n; }"),
      Map("lib/Base.java" -> "package lib; public class Base { protected final int n = 0; }"),
      Map("app/Sub.java" -> "package app; public class Sub extends lib.Base { public Sub() { n = 1; } }", main("new Sub();")),
      Seq(("FinalFieldWritten(app.Sub,n,app.Sub)", Reachable)), failsAtRuntime = true),

    Scenario("reading a field that became final is fine",
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public int n; }"),
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public final int n; public Cfg() { n = 1; } }"),
      Map(main("int x = new lib.Cfg().n; if (x < 0) throw new IllegalStateException();")),
      Seq.empty, failsAtRuntime = false),

    Scenario("a field that is not final can still be written",
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public int n; }"),
      Map("lib/Other.java" -> "package lib; public class Other { }"),
      Map(main("new lib.Cfg().n = 5;")),
      Seq.empty, failsAtRuntime = false),

    Scenario("a final field written by its own class is not reported",
      Map("lib/Own.java" -> "package lib; public class Own { public final int n; public Own() { n = 1; } }"),
      Map("lib/Other.java" -> "package lib; public class Other { }"),
      Map(main("new lib.Own();")),
      Seq.empty, failsAtRuntime = false),

    Scenario("a guarded write to a field that became final",
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public int n; }"),
      Map("lib/Cfg.java" -> "package lib; public class Cfg { public final int n; public Cfg() { n = 1; } }"),
      Map(main("try { new lib.Cfg().n = 5; } catch (IllegalAccessError e) { }")),
      Seq(("FinalFieldWritten(lib.Cfg,n,app.Main)", Guarded)), failsAtRuntime = false)
  )

  private def show(c: Conflict, s: Symbols): String =
    def n(id: ClassId) = s.className(id).replace('/', '.')
    c match
      case Conflict.AbstractInstantiated(t, f, why) => s"AbstractInstantiated(${n(t)},${n(f)},$why)"
      case Conflict.FinalFieldWritten(o, name, _, f) => s"FinalFieldWritten(${n(o)},${s.name(name)},${n(f)})"
      case other                                     => other.show(s)

  private def analyse(app: Path, lib: Path, precision: Precision): Vector[Row] =
    val syms  = new Symbols
    val index = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    Checker.findings(index, scope, precision).map(f => (show(f.conflict, syms), f.label)).distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (lib1, lib2) = ChangedLibrary.build(s.v1, s.v2)
      val app  = compile(s.app, Seq(lib1))
      val rows = analyse(app, lib2, Precision.Method)
      assertEquals(rows, s.expected.sortBy(_.toString).toVector)
      assertEquals(analyse(app, lib2, Precision.Class).map(_._1), rows.map(_._1), "Method precision only labels the Class precision conflicts")

      val error = RuntimeOracle.run(Seq(app, lib2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      error.foreach: e =>
        assert(e.isInstanceOf[LinkageError], s"expected a linkage error, got $e")
        assert(rows.exists(_._2 == Reachable), s"a real failure had no Reachable conflict: $rows")
        assert(analyse(app, lib2, Precision.MethodRta).exists(_._2 == Reachable), "a real failure had no Reachable conflict under RTA")
    }

  test("a broken new or write that nothing reachable runs is Latent at Method precision") {
    val (lib1, lib2) = ChangedLibrary.build(
      Map("lib/Thing.java" -> "package lib; public class Thing { }",
        "lib/Cfg.java" -> "package lib; public class Cfg { public int n; }",
        "lib/Util.java" -> "package lib; public class Util { public static void used() { } public static void unused() { new Thing(); new Cfg().n = 1; } }"),
      Map("lib/Thing.java" -> "package lib; public abstract class Thing { }",
        "lib/Cfg.java" -> "package lib; public class Cfg { public final int n = 0; }"))
    val app  = compile(Map(main("lib.Util.used();")), Seq(lib1))
    val rows = analyse(app, lib2, Precision.Method)
    assertEquals(rows.size, 2)
    assert(rows.forall(_._2 == Latent), s"expected only Latent rows, got $rows")
  }
