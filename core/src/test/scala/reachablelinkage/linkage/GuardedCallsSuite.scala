package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/** A reference inside a `try` whose handler catches the error it would throw is Guarded: the code expects the dependency to be missing. "The error it would
  * throw" includes every superclass: `NoSuchMethodError` is stopped by `IncompatibleClassChangeError`, `LinkageError`, `Error` and `Throwable`, and by nothing
  * narrower or unrelated. Each scenario also runs the app, so a Guarded one must really survive and an unguarded one must really fail.
  */
class GuardedCallsSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent
  private val Guarded   = Label.Guarded

  private final class Scenario(
      val name: String,
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean,
      val lib: Map[String, String] = Map.empty
  )

  private val run                     = ("app.Main", "run")
  private def row(l: Label): Seq[Row] = Seq((run._1, run._2, l))

  private val scenarios = Seq(
    // ---- a vanished class throws NoClassDefFoundError ----
    Scenario(
      "NoClassDefFoundError is caught around a call into a vanished class",
      "try { lib.Vanished.y(); } catch (NoClassDefFoundError e) { }",
      row(Guarded),
      failsAtRuntime = false
    ),
    Scenario("LinkageError, a superclass, is caught", "try { lib.Vanished.y(); } catch (LinkageError e) { }", row(Guarded), failsAtRuntime = false),
    Scenario("Error is caught", "try { lib.Vanished.y(); } catch (Error e) { }", row(Guarded), failsAtRuntime = false),
    Scenario("Throwable is caught", "try { lib.Vanished.y(); } catch (Throwable e) { }", row(Guarded), failsAtRuntime = false),

    // ---- a removed method throws NoSuchMethodError ----
    Scenario(
      "NoSuchMethodError is caught around a removed method",
      "try { lib.Gone.x(); } catch (NoSuchMethodError e) { }",
      row(Guarded),
      failsAtRuntime = false
    ),
    Scenario(
      "IncompatibleClassChangeError, its superclass, is caught",
      "try { lib.Gone.x(); } catch (IncompatibleClassChangeError e) { }",
      row(Guarded),
      failsAtRuntime = false
    ),
    Scenario(
      "a multi-catch that includes the error",
      "try { lib.Gone.x(); } catch (IllegalStateException | NoSuchMethodError e) { }",
      row(Guarded),
      failsAtRuntime = false
    ),

    // ---- a removed field throws NoSuchFieldError ----
    Scenario(
      "NoSuchFieldError is caught around a removed field",
      "try { int v = lib.Gone.f; } catch (NoSuchFieldError e) { }",
      row(Guarded),
      failsAtRuntime = false
    ),

    // ---- handlers that catch and rethrow protect nothing: the error still escapes ----
    Scenario("catch Throwable and rethrow it", "try { lib.Gone.x(); } catch (Throwable t) { throw t; }", row(Reachable), failsAtRuntime = true),
    Scenario(
      "try-with-resources, whose generated handler rethrows",
      "try (java.io.StringReader r = new java.io.StringReader(\"\")) { lib.Gone.x(); }",
      row(Reachable),
      failsAtRuntime = true
    ),
    Scenario("log and rethrow", "try { lib.Gone.x(); } catch (Error e) { System.getProperty(\"x\"); throw e; }", row(Reachable), failsAtRuntime = true),
    // slf4j's MDC does this: the handler reads the message to tell the error it expects from any other.
    Scenario(
      "a handler that reads the message and swallows the error it expects is a guard",
      "try { lib.Gone.x(); } catch (Throwable t) { String m = t.getMessage(); if (m != null && m.contains(\"Gone\")) { System.getProperty(\"x\"); } else { throw t; } }",
      row(Guarded),
      failsAtRuntime = false
    ),
    Scenario(
      "so is one that tests the type of the error",
      "try { lib.Gone.x(); } catch (Throwable t) { if (t instanceof NoSuchMethodError) { System.getProperty(\"x\"); } else { throw t; } }",
      row(Guarded),
      failsAtRuntime = false
    ),
    Scenario(
      "addSuppressed on the error, as try-with-resources does, is not inspecting it",
      "try (java.io.StringReader r = new java.io.StringReader(\"\")) { lib.Gone.x(); }",
      row(Reachable),
      failsAtRuntime = true
    ),

    // ---- a handler that always throws something: the error escapes wrapped, so it is not a guard ----
    Scenario(
      "catch and wrap in another exception",
      "try { lib.Gone.x(); } catch (NoSuchMethodError e) { throw new IllegalStateException(\"gone\", e); }",
      row(Reachable),
      failsAtRuntime = true
    ),
    Scenario(
      "catch Throwable, log, and wrap",
      "try { lib.Gone.x(); } catch (Throwable t) { System.getProperty(\"x\"); throw new RuntimeException(t); }",
      row(Reachable),
      failsAtRuntime = true
    ),
    Scenario(
      "read the message into a new exception: reading it does not make the handler a guard when it always throws",
      "try { lib.Gone.x(); } catch (Throwable t) { throw new IllegalStateException(\"failed: \" + t.getMessage(), t); }",
      row(Reachable),
      failsAtRuntime = true
    ),
    Scenario(
      "wrap on one path and swallow on the other: it may swallow, so it guards",
      "try { lib.Gone.x(); } catch (Throwable t) { if (System.getProperty(\"strict\") != null) { throw new RuntimeException(t); } }",
      row(Guarded),
      failsAtRuntime = false
    ),
    Scenario(
      "a handler that returns normally after logging is a guard",
      "try { lib.Gone.x(); } catch (NoSuchMethodError e) { System.getProperty(\"x\"); return; }",
      row(Guarded),
      failsAtRuntime = false
    ),

    Scenario(
      "catch Throwable and swallow it is still a guard",
      "try { lib.Gone.x(); } catch (Throwable t) { System.getProperty(\"x\"); }",
      row(Guarded),
      failsAtRuntime = false
    ),

    // ---- handlers that do not protect ----
    Scenario(
      "NoClassDefFoundError does not protect a removed method",
      "try { lib.Gone.x(); } catch (NoClassDefFoundError e) { }",
      row(Reachable),
      failsAtRuntime = true
    ),
    Scenario(
      "NoSuchMethodError does not protect a removed field",
      "try { int v = lib.Gone.f; } catch (NoSuchMethodError e) { }",
      row(Reachable),
      failsAtRuntime = true
    ),
    Scenario("catching Exception does not protect against an Error", "try { lib.Gone.x(); } catch (Exception e) { }", row(Reachable), failsAtRuntime = true),
    Scenario("a try with only a finally does not protect", "try { lib.Gone.x(); } finally { }", row(Reachable), failsAtRuntime = true),

    // ---- the protected range is exactly the try body ----
    Scenario(
      "a call in the handler itself is not protected by it",
      "try { int a = 1; } catch (LinkageError e) { lib.Gone.x(); }",
      row(Reachable),
      failsAtRuntime = false
    ),
    Scenario(
      "a call after the try block is not protected",
      "try { int a = 1; } catch (LinkageError e) { } lib.Gone.x();",
      row(Reachable),
      failsAtRuntime = true
    ),
    Scenario(
      "an outer handler protects a call in a nested try",
      "try { try { lib.Gone.x(); } catch (IllegalStateException e) { } } catch (LinkageError e) { }",
      row(Guarded),
      failsAtRuntime = false
    ),

    // Unreached code breaks nothing, guarded or not: Latent wins.
    Scenario(
      "a guarded call in a method nothing calls is Latent",
      "lib.Util.used();",
      Seq(("lib.Util", "unused", Latent)),
      failsAtRuntime = false,
      lib = Map(
        "lib/Util.java" ->
          "package lib; public class Util { public static void used() {} public static void unused() { try { Gone.x(); } catch (LinkageError e) { } } }"
      )
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
      val (v1, v2) =
        GoneLibrary.build(s.lib + ("lib/Vanished.java" -> "package lib; public class Vanished { public static void y() {} }"), removed = Set("lib/Vanished"))
      val app = compile(
        Map(
          "app/Main.java" ->
            s"package app; public class Main { public static void run() { ${s.body} } }"
        ),
        Seq(v1)
      )

      assertEquals(analyse(app, v2, Precision.Method), s.expected.sortBy(_.toString).toVector)

      // Class precision has no reachability, so Latent never appears; the conflicts are the same.
      val classRows = analyse(app, v2, Precision.Class)
      assert(classRows.forall(_._3 != Latent))
      assertEquals(classRows.map(r => (r._1, r._2)), s.expected.map(r => (r._1, r._2)).sortBy(_.toString).toVector)

      // The JVM agrees: a Guarded call survives, and a failing run was predicted Reachable.
      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        // A handler may wrap the error, so look down the cause chain for the linkage error.
        def chain(t: Throwable | Null): LazyList[Throwable] = if t == null then LazyList.empty else t.nn #:: chain(t.nn.getCause)
        assert(error.exists(e => chain(e).exists(_.isInstanceOf[LinkageError])), s"expected a linkage error, got $error")
        assert(analyse(app, v2, Precision.Method).exists(_._3 == Reachable), "a real failure had no Reachable conflict")
        assert(analyse(app, v2, Precision.MethodRta).exists(_._3 == Reachable), "a real failure had no Reachable conflict under RTA")
    }
