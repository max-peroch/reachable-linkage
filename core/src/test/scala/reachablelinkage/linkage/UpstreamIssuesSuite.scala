package reachablelinkage.linkage

import reachablelinkage.fixture.Scopes
import reachablelinkage.fixture.MissinglinkOracle.Key
import reachablelinkage.fixture.JavaFixture.{compile, source}
import java.nio.file.Files

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.{JavaFixture, MissinglinkOracle, OracleSuite}
import reachablelinkage.symbols.Symbols

/**
 * One fixture per upstream issue (spotify/missinglink) or case a checker should get right,
 * compiled from real Java. For each we pin down BOTH what missinglink-core does today and what we
 * do, so a claim like "missinglink gives a false positive here" stays verified instead of assumed.
 *
 * Fixtures marked Pending describe the behaviour we want but do not have yet; they are ignored
 * (and listed as such by munit) until their milestone lands.
 */
class UpstreamIssuesSuite extends OracleSuite:

  private def cnf(from: String, target: String)             = Key("CLASS_NOT_FOUND", from, target, "")
  private def mnf(from: String, target: String, m: String)  = Key("METHOD_SIGNATURE_NOT_FOUND", from, target, m)
  private def fnf(from: String, target: String, f: String)  = Key("FIELD_NOT_FOUND", from, target, f)

  /** Expected conflicts when only the app is checked, and when reachable dependencies are too. */
  private final case class Expect(app: Set[Key], full: Set[Key])
  private object Expect:
    val none: Expect                   = Expect(Set.empty, Set.empty)
    def same(ks: Key*): Expect         = Expect(ks.toSet, ks.toSet)
    def fullOnly(ks: Key*): Expect     = Expect(Set.empty, ks.toSet)

  private enum Status:
    case Verified
    case Pending(milestone: String)

  private final class Issue(
      val id: String,
      val title: String,
      val ml: Expect,
      val ours: Expect,
      val status: Status,
      build: => (ClassSource, Seq[ClassSource])
  ):
    lazy val built: (ClassSource, Seq[ClassSource]) = build

  private val issues: Seq[Issue] = Seq(

    // ---- false positives in missinglink -------------------------------------------------------

    Issue("spotify#39", "static method defined in a superclass", Expect.none, Expect.none, Status.Verified, {
      val lib = compile(Map(
        "lib/Base.java"  -> "package lib; public class Base { public static void indent() {} }",
        "lib/Child.java" -> "package lib; public class Child extends Base {}"))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static void run() { lib.Child.indent(); } }"), Seq(lib))
      (source(app), Seq(source(lib)))
    }),

    Issue("spotify#39", "static method defined in the outer class, called from a subclass nested in it",
      Expect.none, Expect.none, Status.Verified, {
      val lib = compile(Map("lib/Parseable.java" ->
        """package lib;
          |public class Parseable {
          |  static void trace(String s) {}
          |  public static class Url extends Parseable {
          |    public void reader() { trace("x"); }
          |  }
          |}""".stripMargin))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static void run() { new lib.Parseable.Url().reader(); } }"), Seq(lib))
      (source(app), Seq(source(lib)))
    }),

    Issue("spotify#23", "a dependency redefines a class that an earlier entry already provides",
      Expect(Set.empty, Set(fnf("lib/Style", "lib/Style", "registry"))), Expect.none, Status.Verified, {
      val winner = compile(Map("lib/Style.java" -> "package lib; public class Style { public static Object get() { return null; } }"))
      val shadowed = compile(Map("lib/Style.java" ->
        "package lib; public class Style { static final Object registry = new Object(); public static Object get() { return registry; } }"))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static void run() { lib.Style.get(); } }"), Seq(winner))
      // classpath order: app, winner, shadowed. The shadowed copy is never loaded by the JVM.
      (source(app), Seq(source(winner), source(shadowed)))
    }),

    Issue("spotify#55", "MethodHandle.invokeExact is signature polymorphic", Expect.none, Expect.none, Status.Verified, {
      val app = compile(Map("app/Main.java" ->
        """package app;
          |public class Main {
          |  public static int run(java.lang.invoke.MethodHandle mh) throws Throwable { return (int) mh.invokeExact("x"); }
          |}""".stripMargin))
      (source(app), Nil)
    }),

    // ---- breaks missinglink finds and so do we ------------------------------------------------

    Issue("spotify#25", "supertype of a reachable dependency class disappears",
      Expect.fullOnly(cnf("lib/Sub", "lib/Base")), Expect.fullOnly(cnf("lib/Sub", "lib/Base")), Status.Verified, {
      val v1 = compile(Map(
        "lib/Base.java" -> "package lib; public class Base {}",
        "lib/Sub.java"  -> "package lib; public class Sub extends Base {}"))
      // The dependency as shipped after Base vanished: only Sub, still extending Base.
      val sub = compile(Map("lib/Sub.java" -> "package lib; public class Sub extends Base {}"), Seq(v1))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static Object run() { return new lib.Sub(); } }"), Seq(v1))
      (source(app), Seq(source(sub)))
    }),

    // ---- breaks missinglink misses and we catch -----------------------------------------------

    Issue("static-interface-method", "a static method that moved into an interface is not inherited by implementors",
      Expect.none, Expect.same(mnf("app/Main", "lib/C", "s")), Status.Verified, {
      val v1 = compile(Map(
        "lib/Base.java" -> "package lib; public class Base { public static void s() {} }",
        "lib/C.java"    -> "package lib; public class C extends Base {}"))
      val v2 = compile(Map(
        "lib/I.java" -> "package lib; public interface I { static void s() {} }",
        "lib/C.java" -> "package lib; public class C implements I {}"))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static void run() { lib.C.s(); } }"), Seq(v1))
      (source(app), Seq(source(v2)))
    }),

    Issue("missing-parent-interface", "a parent interface of a reachable dependency class disappears",
      Expect.none, Expect.fullOnly(cnf("lib/C", "lib/I")), Status.Verified, {
      val v1 = compile(Map(
        "lib/I.java" -> "package lib; public interface I {}",
        "lib/C.java" -> "package lib; public class C implements I {}"))
      val c  = compile(Map("lib/C.java" -> "package lib; public class C implements I {}"), Seq(v1))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static Object run() { return new lib.C(); } }"), Seq(v1))
      (source(app), Seq(source(c)))
    }),

    // ---- wanted, not built yet ----------------------------------------------------------------

    Issue("spotify#22", "interface gained a method its implementation (an older jar) lacks",
      Expect.none, Expect.fullOnly(Key("ABSTRACT_METHOD_NOT_IMPLEMENTED", "lib/FooMetrics", "lib/Metrics", "b")),
      Status.Verified, {
      val aV1  = compile(Map("lib/Metrics.java" -> "package lib; public interface Metrics { void a(); }"))
      val aV2  = compile(Map("lib/Metrics.java" -> "package lib; public interface Metrics { void a(); void b(); }"))
      val impl = compile(Map("lib/FooMetrics.java" ->
        "package lib; public class FooMetrics implements Metrics { public void a() {} }"), Seq(aV1))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static void run() { lib.Metrics m = new lib.FooMetrics(); m.b(); } }"),
        Seq(aV2, impl))
      (source(app), Seq(source(aV2), source(impl)))
    }),

    Issue("spotify#11", "a method became private: missinglink misses it, we report it",
      Expect.none, Expect.same(Key("INACCESSIBLE_MEMBER", "app/Main", "lib/Foo", "bar")), Status.Verified, {
      val v1 = compile(Map("lib/Foo.java" -> "package lib; public class Foo { public static void bar() {} }"))
      val v2 = compile(Map("lib/Foo.java" -> "package lib; public class Foo { private static void bar() {} }"))
      val app = compile(Map("app/Main.java" ->
        "package app; public class Main { public static void run() { lib.Foo.bar(); } }"), Seq(v1))
      (source(app), Seq(source(v2)))
    }),

    // missinglink skips a guarded call; we report it and label it Guarded (see GuardedCallsSuite).
    Issue("spotify#7", "call guarded by catch (NoClassDefFoundError): missinglink skips it, we label it Guarded",
      Expect.none, Expect.same(cnf("app/Main", "opt/Optional")), Status.Verified, {
      val opt = compile(Map("opt/Optional.java" -> "package opt; public class Optional { public static void call() {} }"))
      val app = compile(Map("app/Main.java" ->
        """package app;
          |public class Main {
          |  public static void run() {
          |    try { opt.Optional.call(); } catch (NoClassDefFoundError e) { }
          |  }
          |}""".stripMargin), Seq(opt))
      (source(app), Nil)
    }),

    Issue("guarded-by-linkage-error", "call guarded by catch (LinkageError): missinglink reports it, we label it Guarded",
      Expect.same(cnf("app/Main", "opt/Optional")), Expect.same(cnf("app/Main", "opt/Optional")), Status.Verified, {
      val opt = compile(Map("opt/Optional.java" -> "package opt; public class Optional { public static void call() {} }"))
      val app = compile(Map("app/Main.java" ->
        """package app;
          |public class Main {
          |  public static void run() {
          |    try { opt.Optional.call(); } catch (LinkageError e) { }
          |  }
          |}""".stripMargin), Seq(opt))
      (source(app), Nil)
    })
  )

  for issue <- issues; scan <- Seq(false, true) do
    val scope = if scan then "Full" else "App"
    val label = s"${issue.id}: ${issue.title} [$scope]"

    // What missinglink-core does today. Always verified, even when our side is pending.
    test(s"missinglink on $label") {
      val (app, libs) = issue.built
      val got = MissinglinkOracle.check(app, libs, scan)
      assertEquals(got, if scan then issue.ml.full else issue.ml.app)
    }

    val ours = s"ours on $label"
    val body = { () =>
      val (app, libs) = issue.built
      val syms  = new Symbols
      val index = ClassIndex((app +: libs :+ ClassSource.jdk()).toIndexedSeq, syms)
      val idx   = 1 to libs.length
      val sc    = Scopes.of(index, Vector(0), if scan then Vector(0) ++ idx else Vector(0), Vector(0) ++ idx)
      val got   = Checker.check(index, sc).flatMap(MissinglinkOracle.normalise(_, syms)).toSet
      assertEquals(got, if scan then issue.ours.full else issue.ours.app)
    }
    issue.status match
      case Status.Verified   => test(ours)(body())
      case Status.Pending(m) => test(s"$ours (pending $m)".ignore)(body())

  test("a multi-release jar's META-INF/versions entries are not treated as classes") {
    val dir = JavaFixture.compile(Map("lib/Foo.java" -> "package lib; public class Foo {}"))
    val jar = Files.createTempFile("mr", ".jar")
    val bytes = Files.readAllBytes(dir.resolve("lib/Foo.class"))
    val out = new java.util.jar.JarOutputStream(Files.newOutputStream(jar))
    try
      for entry <- Seq("lib/Foo.class", "META-INF/versions/21/lib/Foo.class") do
        out.putNextEntry(new java.util.zip.ZipEntry(entry)); out.write(bytes); out.closeEntry()
    finally out.close()
    val src = ClassSource.open(jar).toOption.get
    try assertEquals(src.classNames.toList, List("lib/Foo")) finally src.close()
  }
