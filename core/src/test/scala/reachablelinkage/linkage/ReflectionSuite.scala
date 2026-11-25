package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * Constant-name reflection: `Class.forName("lib.Impl")` loads and initialises the class and
 * `newInstance` runs its constructor, though no instruction names `lib.Impl`. Each scenario is run on
 * the JVM, and a real linkage failure must have been labelled Reachable.
 */
class ReflectionSuite extends munit.FunSuite:

  private type Row = (String, String, Label)

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean
  )

  private def impl(members: String) =
    "lib/Impl.java" -> s"package lib; public class Impl { $members }"

  private val construct =
    "try { Class.forName(\"lib.Impl\").getDeclaredConstructor().newInstance(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }"

  /** Builds an Impl through its class name, then runs `use` with `c` (the Class) and `o` (the instance). */
  private def call(use: String) =
    "try { Class<?> c = Class.forName(\"lib.Impl\"); Object o = c.getDeclaredConstructor().newInstance(); " + use +
      " } catch (java.lang.reflect.InvocationTargetException e) { if (e.getCause() instanceof Error) throw (Error) e.getCause(); throw new RuntimeException(e); }" +
      " catch (ReflectiveOperationException e) { throw new RuntimeException(e); }"

  private val scenarios = Seq(
    Scenario("a constructor reached by Class.forName and newInstance", Map(impl("public Impl() { Gone.x(); }")), construct,
      Seq(("lib.Impl", "<init>", Label.Reflective)), failsAtRuntime = true),

    // `getConstructor(types).newInstance(args)` picks the constructor by its arguments, so a constructor with parameters is reached too.
    Scenario("a constructor with arguments, picked by its parameter types", Map(impl("public Impl(String s) { Gone.x(); } public Impl() { }")),
      "try { Class.forName(\"lib.Impl\").getConstructor(String.class).newInstance(\"x\"); } catch (java.lang.reflect.InvocationTargetException e) { if (e.getCause() instanceof Error) throw (Error) e.getCause(); throw new RuntimeException(e); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }",
      Seq(("lib.Impl", "<init>", Label.Reflective)), failsAtRuntime = true),

    Scenario("a static initialiser run by Class.forName", Map(impl("static { Gone.x(); }")),
      "try { Class.forName(\"lib.Impl\"); } catch (ClassNotFoundException e) { throw new RuntimeException(e); }",
      Seq(("lib.Impl", "<clinit>", Label.Reflective)), failsAtRuntime = true),

    Scenario("a method of the reflectively loaded class that nothing calls is latent",
      Map(impl("public Impl() { } public void other() { Gone.x(); }")), construct,
      Seq(("lib.Impl", "other", Label.Latent)), failsAtRuntime = false),

    // App scope checks reached classes only, so a class nothing names is not examined at all.
    Scenario("a name nobody loads does not reach anything", Map(impl("public Impl() { Gone.x(); }")), "",
      Seq.empty, failsAtRuntime = false),

    Scenario("a method looked up by getMethod and invoked", Map(impl("public Impl() { } public void go() { Gone.x(); } public void other() { Gone.x(); }")),
      call("c.getMethod(\"go\").invoke(o);"),
      Seq(("lib.Impl", "go", Label.Reflective), ("lib.Impl", "other", Label.Latent)), failsAtRuntime = true),

    Scenario("a private method looked up by getDeclaredMethod", Map(impl("public Impl() { } private void hidden() { Gone.x(); }")),
      call("java.lang.reflect.Method m = c.getDeclaredMethod(\"hidden\"); m.setAccessible(true); m.invoke(o);"),
      Seq(("lib.Impl", "hidden", Label.Reflective)), failsAtRuntime = true),

    Scenario("a method looked up with parameter types", Map(impl("public Impl() { } public void go(String s, int n) { Gone.x(); } public void go() { Gone.x(); }")),
      call("c.getMethod(\"go\", String.class, int.class).invoke(o, \"a\", 1);"),
      Seq(("lib.Impl", "go", Label.Reflective)), failsAtRuntime = true),

    Scenario("a method found through a method handle", Map(impl("public Impl() { } public void go() { Gone.x(); } public void other() { Gone.x(); }")),
      "try { Object o = new lib.Impl(); java.lang.invoke.MethodHandles.lookup().findVirtual(lib.Impl.class, \"go\", java.lang.invoke.MethodType.methodType(void.class)).invoke(o); } catch (Error e) { throw e; } catch (Throwable t) { throw new RuntimeException(t); }",
      Seq(("lib.Impl", "go", Label.Reflective), ("lib.Impl", "other", Label.Latent)), failsAtRuntime = true),

    Scenario("a name that is not a class on the classpath is ignored", Map(impl("public Impl() { Gone.x(); }")),
      "String s = \"lib.Other\"; if (s.isEmpty()) throw new IllegalStateException();",
      Seq.empty, failsAtRuntime = false)
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
      val (v1, v2) = GoneLibrary.build(s.lib)
      val app = compile(Map("app/Main.java" ->
        s"package app; public class Main { public static void run() { ${s.body} } }"), Seq(v1))
      val rows = analyse(app, v2, Precision.Method)
      assertEquals(rows, s.expected.sortBy(_.toString).toVector)
      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(rows.exists(_._3 == Label.Reflective), s"a real failure had no Reflective conflict: $rows")
        assert(analyse(app, v2, Precision.MethodRta).exists(_._3 == Label.Reflective), "a real failure had no Reflective conflict under RTA")
    }
