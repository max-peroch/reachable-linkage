package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * spotify #22: an interface (or abstract class) gains an abstract method, and an
 * implementation built against the old API lacks it. The app is compiled against the new API, so
 * it can call the new method; run against the old implementation that is an AbstractMethodError.
 * It only breaks if the method is called, so Method precision labels it Reachable only then.
 * The scenarios that must stay clean matter as much: reporting a well-formed class would be noise.
 */
class AbstractMethodsSuite extends munit.FunSuite:

  private type Row = (String, String, Label, String)
  private val Reachable = Label.Reachable
  private val Latent    = Label.Latent
  private val Reflective = Label.Reflective
  private def counts(l: Label): Boolean = l == Reachable || l == Reflective
  private val kind      = "AbstractMethodNotImplemented"

  private def reflectiveCall(name: String): String =
    "lib.Api x = new lib.Impl(); try { lib.Api.class.getMethod(\"" + name + "\").invoke(x); } " +
      "catch (java.lang.reflect.InvocationTargetException e) { if (e.getCause() instanceof Error) throw (Error) e.getCause(); throw new RuntimeException(e); } " +
      "catch (ReflectiveOperationException e) { throw new RuntimeException(e); }"

  private val apiV1 = "lib/Api.java" -> "package lib; public interface Api { void a(); }"
  private val apiV2 = "lib/Api.java" -> "package lib; public interface Api { void a(); void b(); }"
  private val impl  = "lib/Impl.java" -> "package lib; public class Impl implements Api { public void a() {} }"

  private final class Scenario(
      val name: String,
      val v1: Map[String, String],
      val v2: Map[String, String],
      val implementation: Map[String, String],
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean
  )

  private val scenarios = Seq(
    Scenario("an interface gained a method an old implementation lacks, and it is called",
      Map(apiV1), Map(apiV2), Map(impl), "lib.Api x = new lib.Impl(); x.b();",
      Seq(("lib.Impl", "", Reachable, kind)), failsAtRuntime = true),

    // `Api.class.getMethod("b").invoke(x)`: the name is a constant, so a rule can see it is called; no instruction calls b().
    Scenario("the new method is called only through reflection",
      Map(apiV1), Map(apiV2), Map(impl),
      reflectiveCall("b"),
      Seq(("lib.Impl", "", Reflective, kind)), failsAtRuntime = true),

    Scenario("a different method is looked up by name, so the missing one stays latent",
      Map(apiV1), Map(apiV2), Map(impl),
      reflectiveCall("a"),
      Seq(("lib.Impl", "", Latent, kind)), failsAtRuntime = false),

    Scenario("the same, but the new method is never called",
      Map(apiV1), Map(apiV2), Map(impl), "lib.Api x = new lib.Impl(); x.a();",
      Seq(("lib.Impl", "", Latent, kind)), failsAtRuntime = false),

    Scenario("an abstract class gained an abstract method",
      Map("lib/Shape.java" -> "package lib; public abstract class Shape { public abstract int area(); }"),
      Map("lib/Shape.java" -> "package lib; public abstract class Shape { public abstract int area(); public abstract int width(); }"),
      Map("lib/Square.java" -> "package lib; public class Square extends lib.Shape { public int area() { return 1; } }"),
      "lib.Shape s = new lib.Square(); s.width();",
      Seq(("lib.Square", "", Reachable, kind)), failsAtRuntime = true),

    // ---- things that must stay clean ----
    Scenario("the new method has a default", Map(apiV1),
      Map("lib/Api.java" -> "package lib; public interface Api { void a(); default void b() {} }"), Map(impl),
      "lib.Api x = new lib.Impl(); x.b();", Seq.empty, failsAtRuntime = false),

    // Api gains an abstract b; Defaults (another interface the old class implements) already supplies a
    // default b. Method selection picks the one non-abstract method, so the call works (JVMS 5.4.3.3).
    Scenario("a default from another interface implements the new abstract method",
      Map(apiV1, "lib/Defaults.java" -> "package lib; public interface Defaults { default void b() {} }"),
      Map(apiV2, "lib/Defaults.java" -> "package lib; public interface Defaults { default void b() {} }"),
      Map("lib/Both.java" -> "package lib; public class Both implements Api, Defaults { public void a() {} }"),
      "lib.Api x = new lib.Both(); x.b();", Seq.empty, failsAtRuntime = false),

    Scenario("the new method is implemented by a superclass", Map(apiV1), Map(apiV2),
      Map("lib/Base.java" -> "package lib; public class Base { public void b() {} }",
          "lib/Impl2.java" -> "package lib; public class Impl2 extends Base implements Api { public void a() {} }"),
      "lib.Api x = new lib.Impl2(); x.b();", Seq.empty, failsAtRuntime = false),

    Scenario("an abstract class in between does not have to implement it", Map(apiV1), Map(apiV2),
      Map("lib/Partial.java" -> "package lib; public abstract class Partial implements Api { public void a() {} }",
          "lib/Full.java" -> "package lib; public class Full extends Partial { public void b() {} }"),
      "lib.Api x = new lib.Full(); x.b();", Seq.empty, failsAtRuntime = false),

    Scenario("a generic interface is implemented through a bridge method",
      Map("lib/Conv.java" -> "package lib; public interface Conv<T> { T conv(T t); }"),
      Map("lib/Conv.java" -> "package lib; public interface Conv<T> { T conv(T t); }"),
      Map("lib/StrConv.java" -> "package lib; public class StrConv implements Conv<String> { public String conv(String s) { return s; } }"),
      "lib.Conv<String> c = new lib.StrConv(); c.conv(\"x\");", Seq.empty, failsAtRuntime = false),

    Scenario("an Object method redeclared in an interface is implemented by Object",
      Map("lib/Eq.java" -> "package lib; public interface Eq { boolean equals(Object o); void m(); }"),
      Map("lib/Eq.java" -> "package lib; public interface Eq { boolean equals(Object o); void m(); }"),
      Map("lib/EqImpl.java" -> "package lib; public class EqImpl implements Eq { public void m() {} }"),
      "lib.Eq e = new lib.EqImpl(); e.m();", Seq.empty, failsAtRuntime = false)
  )

  private def analyse(app: Path, api: Path, implementation: Path, precision: Precision): Vector[Row] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(api), source(implementation), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1, 2), Vector(0, 1, 2))
    val locator = Locator(index)
    Checker.findings(index, scope, precision).map: f =>
      (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label, f.conflict.productPrefix)
    .distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val v1    = compile(s.v1)
      val v2    = compile(s.v2)
      val oldImpl = compile(s.implementation, Seq(v1))
      val app   = compile(Map("app/Main.java" ->
        s"package app; public class Main { public static void run() { ${s.body} } }"), Seq(v2, oldImpl))

      assertEquals(analyse(app, v2, oldImpl, Precision.Method), s.expected.sortBy(_.toString).toVector)

      // Class precision reports every unimplemented method, the same rows as Method. It has no call graph, but sees that no class
      // names a method nobody calls (or looks up by name): that one is Latent at both, anything else is Reachable.
      val classRows = analyse(app, v2, oldImpl, Precision.Class)
      assertEquals(classRows.map(_._3), s.expected.sortBy(_.toString).map(r => if r._3 == Latent then Latent else Reachable).toVector)
      assertEquals(classRows.map(r => (r._1, r._2, r._4)), s.expected.map(r => (r._1, r._2, r._4)).sortBy(_.toString).toVector)

      val error = RuntimeOracle.run(Seq(app, v2, oldImpl))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      if error.isDefined then
        assert(error.exists(_.isInstanceOf[AbstractMethodError]), s"expected an AbstractMethodError, got $error")
        assert(analyse(app, v2, oldImpl, Precision.Method).exists(r => counts(r._3)), "a real failure had no Reachable or Reflective conflict")
        assert(analyse(app, v2, oldImpl, Precision.MethodRta).exists(r => counts(r._3)), "a real failure had no Reachable or Reflective conflict under RTA")
    }

  // ---- a method the JDK declares ----

  /** A class that implements `Runnable` and then loses `run()`, as a class built against an older JDK API would lack a newer method. */
  private def runnableWithoutRun(): Path =
    val dir = compile(Map("lib/R.java" -> "package lib; public class R implements Runnable { public void run() { } }"))
    val file = dir.resolve("lib/R.class")
    val reader = new org.objectweb.asm.ClassReader(java.nio.file.Files.readAllBytes(file))
    val writer = new org.objectweb.asm.ClassWriter(0)
    reader.accept(new org.objectweb.asm.ClassVisitor(org.objectweb.asm.Opcodes.ASM9, writer):
      override def visitMethod(access: Int, name: String, desc: String, sig: String, ex: Array[String]) =
        super.visitMethod(access, if name == "run" then "stopped" else name, desc, sig, ex), 0)
    java.nio.file.Files.write(file, writer.toByteArray)
    dir

  test("a method the JDK declares is assumed called: it is not Latent at Class precision, and not at Method precision once the class is reached") {
    val lib = runnableWithoutRun()
    val app = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { new lib.R(); } }"), Seq(lib))
    assert(RuntimeOracle.run(Seq(app, lib)).isEmpty, "constructing it works; only calling run() would fail")
    for precision <- Seq(Precision.Class, Precision.Method, Precision.MethodRta) do
      assertEquals(analyse(app, lib, lib, precision).map(r => (r._1, r._3, r._4)), Vector(("lib.R", Reachable, kind)), precision.toString)
  }

