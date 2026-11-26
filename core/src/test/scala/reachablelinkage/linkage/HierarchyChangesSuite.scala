package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{ChangedLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/** `final` and interface-versus-class changes, which the JVM rejects when it loads or links the code: a class extending something that became final or an
  * interface, implementing something that became a class, overriding a method that became final, and a call linked as an interface method (or class method) on
  * an owner that is now the other. Each scenario builds an app against v1, runs it on the JVM against v2, and requires that a real failure was labelled
  * Reachable.
  */
class HierarchyChangesSuite extends munit.FunSuite:

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

  private val scenarios = Seq(
    Scenario(
      "a superclass that became final",
      Map("lib/Base.java" -> "package lib; public class Base { }"),
      Map("lib/Base.java" -> "package lib; public final class Base { }"),
      Map("app/Sub.java"  -> "package app; public class Sub extends lib.Base { }", main("new Sub();")),
      Seq(("IncompatibleHierarchy(app.Sub,lib.Base,SuperclassIsFinal)", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a superclass that became an interface",
      Map("lib/Base.java" -> "package lib; public class Base { }"),
      Map("lib/Base.java" -> "package lib; public interface Base { }"),
      Map("app/Sub.java"  -> "package app; public class Sub extends lib.Base { }", main("new Sub();")),
      Seq(("IncompatibleHierarchy(app.Sub,lib.Base,SuperclassIsInterface)", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "an interface that became a class",
      Map("lib/Api.java"  -> "package lib; public interface Api { }"),
      Map("lib/Api.java"  -> "package lib; public class Api { }"),
      Map("app/Impl.java" -> "package app; public class Impl implements lib.Api { }", main("new Impl();")),
      Seq(("IncompatibleHierarchy(app.Impl,lib.Api,InterfaceIsClass)", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a method that became final while a subclass overrides it",
      Map("lib/Base.java" -> "package lib; public class Base { public void m() { } }"),
      Map("lib/Base.java" -> "package lib; public class Base { public final void m() { } }"),
      Map("app/Sub.java"  -> "package app; public class Sub extends lib.Base { public void m() { } }", main("new Sub();")),
      Seq(("OverridesFinalMethod(app.Sub,lib.Base,m)", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "an override of a method that stayed non-final is fine",
      Map("lib/Base.java" -> "package lib; public class Base { public void m() { } }"),
      Map("lib/Base.java" -> "package lib; public class Base { public void m() { } public final void other() { } }"),
      Map("app/Sub.java"  -> "package app; public class Sub extends lib.Base { public void m() { } }", main("new Sub().m();")),
      Seq.empty,
      failsAtRuntime = false
    ),

    // A package-private method is not overridable from another package, so `m` in app.Sub is a new method.
    Scenario(
      "a package-private method that became final is not overridden from another package",
      Map("lib/Base.java" -> "package lib; public class Base { void m() { } }"),
      Map("lib/Base.java" -> "package lib; public class Base { final void m() { } }"),
      Map("app/Sub.java"  -> "package app; public class Sub extends lib.Base { void m() { } }", main("new Sub();")),
      Seq.empty,
      failsAtRuntime = false
    ),

    Scenario(
      "a package-private method that became final is overridden from its own package",
      Map(
        "lib/Base.java" -> "package lib; public class Base { void m() { } }",
        "lib/Sub.java"  -> "package lib; public class Sub extends Base { void m() { } }",
        "lib/Make.java" -> "package lib; public class Make { public static Object sub() { return new Sub(); } }"
      ),
      Map("lib/Base.java" -> "package lib; public class Base { final void m() { } }"),
      Map(main("lib.Make.sub();")),
      Seq(("OverridesFinalMethod(lib.Sub,lib.Base,m)", Reachable)),
      failsAtRuntime = true
    ),

    // The call is linked as an interface method; the owner is now a class.
    Scenario(
      "an interface method called on an owner that became a class",
      Map(
        "lib/Svc.java"    -> "package lib; public interface Svc { void run(); }",
        "lib/Impl.java"   -> "package lib; public class Impl implements Svc { public void run() { } }",
        "lib/Holder.java" -> "package lib; public class Holder { public static Svc get() { return new Impl(); } }"
      ),
      Map(
        "lib/Svc.java"    -> "package lib; public class Svc { public void run() { } }",
        "lib/Holder.java" -> "package lib; public class Holder { public static Svc get() { return new Svc(); } }"
      ),
      Map(main("lib.Holder.get().run();")),
      Seq(("InterfaceKindChanged(lib.Svc,run,Class)", Reachable)),
      failsAtRuntime = true
    ),

    // The call is linked as a class method; the owner is now an interface.
    Scenario(
      "a class method called on an owner that became an interface",
      Map(
        "lib/Svc.java"    -> "package lib; public class Svc { public void run() { } }",
        "lib/Holder.java" -> "package lib; public class Holder { public static Svc get() { return new Svc(); } }"
      ),
      Map(
        "lib/Svc.java"    -> "package lib; public interface Svc { void run(); }",
        "lib/Impl.java"   -> "package lib; public class Impl implements Svc { public void run() { } }",
        "lib/Holder.java" -> "package lib; public class Holder { public static Svc get() { return new Impl(); } }"
      ),
      Map(main("lib.Holder.get().run();")),
      Seq(("InterfaceKindChanged(lib.Svc,run,Interface)", Reachable)),
      failsAtRuntime = true
    ),

    Scenario(
      "a call that is guarded against the error it would throw",
      Map(
        "lib/Svc.java"    -> "package lib; public interface Svc { void run(); }",
        "lib/Impl.java"   -> "package lib; public class Impl implements Svc { public void run() { } }",
        "lib/Holder.java" -> "package lib; public class Holder { public static Svc get() { return new Impl(); } }"
      ),
      Map(
        "lib/Svc.java"    -> "package lib; public class Svc { public void run() { } }",
        "lib/Holder.java" -> "package lib; public class Holder { public static Svc get() { return new Svc(); } }"
      ),
      Map(main("try { lib.Holder.get().run(); } catch (IncompatibleClassChangeError e) { }")),
      Seq(("InterfaceKindChanged(lib.Svc,run,Class)", Guarded)),
      failsAtRuntime = false
    ),

    Scenario(
      "an unchanged interface and class are not reported",
      Map(
        "lib/Svc.java"    -> "package lib; public interface Svc { void run(); }",
        "lib/Impl.java"   -> "package lib; public class Impl implements Svc { public void run() { } }",
        "lib/Holder.java" -> "package lib; public class Holder { public static Svc get() { return new Impl(); } }"
      ),
      Map("lib/Other.java" -> "package lib; public class Other { }"),
      Map(main("lib.Holder.get().run(); java.util.List<String> l = new java.util.ArrayList<>(); l.size(); new java.util.ArrayList<String>().size();")),
      Seq.empty,
      failsAtRuntime = false
    )
  )

  /** A conflict as a short string, so a scenario can state it. */
  private def show(c: Conflict, s: Symbols): String =
    def n(id: reachablelinkage.symbols.ClassId) = s.className(id).replace('/', '.')
    c match
      case Conflict.IncompatibleHierarchy(cls, t, change)    => s"IncompatibleHierarchy(${n(cls)},${n(t)},$change)"
      case Conflict.OverridesFinalMethod(cls, d, name, _)    => s"OverridesFinalMethod(${n(cls)},${n(d)},${s.name(name)})"
      case Conflict.InterfaceKindChanged(o, name, _, _, now) => s"InterfaceKindChanged(${n(o)},${s.name(name)},$now)"
      case other                                             => other.show(s)

  private def analyse(app: Path, lib: Path, precision: Precision): Vector[Row] =
    val syms  = new Symbols
    val index = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    Checker.findings(index, scope, precision).map(f => (show(f.conflict, syms), f.label)).distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (lib1, lib2) = ChangedLibrary.build(s.v1, s.v2)
      val app          = compile(s.app, Seq(lib1))
      val rows         = analyse(app, lib2, Precision.Method)
      assertEquals(rows, s.expected.sortBy(_.toString).toVector)
      assertEquals(analyse(app, lib2, Precision.Class).map(_._1), rows.map(_._1), "Method precision only labels the Class precision conflicts")

      val error = RuntimeOracle.run(Seq(app, lib2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      error.foreach: e =>
        assert(e.isInstanceOf[LinkageError], s"expected a linkage error, got $e")
        assert(rows.exists(_._2 == Reachable), s"a real failure had no Reachable conflict: $rows")
        assert(analyse(app, lib2, Precision.MethodRta).exists(_._2 == Reachable), "a real failure had no Reachable conflict under RTA")
    }

  test("a broken class nothing reachable calls into is Latent at Method precision") {
    val (lib1, lib2) = ChangedLibrary.build(
      Map(
        "lib/Svc.java"  -> "package lib; public interface Svc { void run(); }",
        "lib/Impl.java" -> "package lib; public class Impl implements Svc { public void run() { } }",
        "lib/Util.java" -> "package lib; public class Util { public static void used() { } public static void unused() { new Impl().run(); Svc s = new Impl(); s.run(); } }"
      ),
      Map("lib/Svc.java" -> "package lib; public class Svc { public void run() { } }")
    )
    val app  = compile(Map(main("lib.Util.used();")), Seq(lib1))
    val rows = analyse(app, lib2, Precision.Method)
    assert(rows.nonEmpty && rows.forall(_._2 == Latent), s"expected only Latent rows, got $rows")
  }
