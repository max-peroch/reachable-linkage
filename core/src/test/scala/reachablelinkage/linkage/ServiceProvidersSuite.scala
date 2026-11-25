package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * `ServiceLoader` providers are named by `META-INF/services` files, never by an instruction, so
 * reachability must start from them. Each scenario is an app built against library v1 and checked
 * and run with v2, where `lib.Gone.x()` is gone. A failing run must have been labelled Reachable.
 */
class ServiceProvidersSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent

  private val service  = "lib/Service.java" -> "package lib; public interface Service { void run(); }"
  private val broken   = "lib/Provider.java" -> "package lib; public class Provider implements Service { public void run() { Gone.x(); } }"
  private val listing  = "META-INF/services/lib.Service" -> "lib.Provider\n"
  private val loadOwn  = "for (lib.Service s : java.util.ServiceLoader.load(lib.Service.class, Main.class.getClassLoader())) s.run();"

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val resources: Map[String, String],
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean
  )

  private val scenarios = Seq(
    Scenario("a provider that only ServiceLoader loads", Map(service, broken), Map(listing), loadOwn,
      Seq(("lib.Provider", "run", Reachable)), failsAtRuntime = true),

    Scenario("a provider method that nothing calls", Map(service,
      "lib/Provider.java" -> "package lib; public class Provider implements Service { public void run() { } public void extra() { Gone.x(); } }"),
      Map(listing), loadOwn,
      Seq(("lib.Provider", "extra", Latent)), failsAtRuntime = false),

    // Nothing in the app names lib.Service, so nobody can ask for its providers.
    Scenario("a service the app never mentions is not loaded", Map(service, broken), Map(listing), "",
      Seq.empty, failsAtRuntime = false),

    Scenario("only the services the app mentions have their providers started", Map(service, broken,
      "lib/Other.java"         -> "package lib; public interface Other { void go(); }",
      "lib/OtherProvider.java" -> "package lib; public class OtherProvider implements Other { public void go() { Gone.x(); } }"),
      Map(listing, "META-INF/services/lib.Other" -> "lib.OtherProvider\n"), loadOwn,
      Seq(("lib.Provider", "run", Reachable)), failsAtRuntime = true),

    Scenario("a provider of a JDK service interface is loaded by the app", Map(
      "lib/Job.java" -> "package lib; public class Job implements Runnable { public void run() { Gone.x(); } }"),
      Map("META-INF/services/java.lang.Runnable" -> "lib.Job\n"),
      "for (Runnable r : java.util.ServiceLoader.load(Runnable.class, Main.class.getClassLoader())) r.run();",
      Seq(("lib.Job", "run", Reachable)), failsAtRuntime = true),

    // JDK code may load these whenever it likes, and the JDK is not walked, so they always start.
    Scenario("a provider of a JDK service interface is assumed to be loaded", Map(
      "lib/Job.java" -> "package lib; public class Job implements Runnable { public void run() { Gone.x(); } }"),
      Map("META-INF/services/java.lang.Runnable" -> "lib.Job\n"), "",
      Seq(("lib.Job", "run", Reachable)), failsAtRuntime = false),

    // Iterating the service reads the whole file, so a listed provider that is gone breaks it.
    Scenario("a provider that is listed but not on the classpath is reported", Map(service, broken),
      Map("META-INF/services/lib.Service" -> "lib.Nope\n"), loadOwn.replace("s.run();", "{ }"),
      Seq(("lib.Service", "", Reachable)), failsAtRuntime = true),

    Scenario("a missing provider of a service the app never mentions is not reported", Map(service, broken),
      Map("META-INF/services/lib.Service" -> "lib.Nope\n"), "",
      Seq.empty, failsAtRuntime = false),

    Scenario("a missing provider of a JDK service interface", Map.empty,
      Map("META-INF/services/java.lang.Runnable" -> "lib.Nope\n"),
      "for (Runnable r : java.util.ServiceLoader.load(Runnable.class, Main.class.getClassLoader())) { }",
      Seq(("java.lang.Runnable", "", Reachable)), failsAtRuntime = true),

    // Provider is rooted but its run() is never called (Latent); the missing one is still reported.
    Scenario("a provider that exists next to one that is missing", Map(service, broken),
      Map("META-INF/services/lib.Service" -> "lib.Provider\nlib.Nope\n"), loadOwn.replace("s.run();", "{ }"),
      Seq(("lib.Provider", "run", Latent), ("lib.Service", "", Reachable)), failsAtRuntime = true),

    Scenario("a service file with comments, blank lines and CRLF", Map(service, broken),
      Map("META-INF/services/lib.Service" -> "# providers\r\n\r\n  lib.Provider   # the broken one\r\n"), loadOwn,
      Seq(("lib.Provider", "run", Reachable)), failsAtRuntime = true)
  )

  private def analyse(app: Path, lib: Path, precision: Precision): Vector[Row] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val locator = Locator(index)
    Checker.findings(index, scope, precision).map: f =>
      (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label)
    .distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(s.lib, s.resources)
      val app = compile(Map("app/Main.java" ->
        s"package app; public class Main { public static void run() { ${s.body} } }"), Seq(v1))

      val rows = analyse(app, v2, Precision.Method)
      assertEquals(rows, s.expected.sortBy(_.toString).toVector)

      // Class precision starts from providers too, and Method precision only labels its conflicts.
      val classRows = analyse(app, v2, Precision.Class)
      assert(classRows.forall(_._3 == Reachable))
      assertEquals(classRows.map(r => (r._1, r._2)), rows.map(r => (r._1, r._2)))

      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(
          error.exists(e => e.isInstanceOf[LinkageError] || e.isInstanceOf[java.util.ServiceConfigurationError]),
          s"expected a linkage or service configuration error, got $error")
        assert(rows.exists(_._3 == Reachable), s"a real failure had no Reachable conflict: $rows")
        assert(analyse(app, v2, Precision.MethodRta).exists(_._3 == Reachable), "a real failure had no Reachable conflict under RTA")
    }
