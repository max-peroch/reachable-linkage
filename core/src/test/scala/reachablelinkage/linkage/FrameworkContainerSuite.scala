package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * The shapes of a container that builds its classes by constructors with arguments, ignores a class whose conditions fail, follows annotations
 * of annotations and goes through member classes (Spring in miniature), each run on the JVM. `lib.Absent` is a class that vanishes in v2, so a
 * condition naming it fails, and one naming `lib.Other` holds. (`lib.Gone` stays, only its members vanish: it is what the broken calls hit.)
 */
class FrameworkContainerSuite extends munit.FunSuite:

  private type Row = (String, String, Label)

  private val annotations = Map(
    "lib/Stereo.java"  -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Stereo { }",
    "lib/Needs.java"   -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Needs { Class<?>[] value(); }",
    "lib/Imports.java" -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Imports { Class<?> value(); }",
    "lib/Conf.java"    -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) @Imports(Target.class) public @interface Conf { }",
    "lib/Other.java"   -> "package lib; public class Other { }")

  private val container = "lib/Container.java" ->
    """package lib;
      |public class Container {
      |  /** Builds a class through its first constructor, with null for every argument: the framework picks the constructor it can satisfy. */
      |  public static Object build(Class<?> type) {
      |    try {
      |      java.lang.reflect.Constructor<?> c = type.getDeclaredConstructors()[0];
      |      c.setAccessible(true);
      |      return c.newInstance(new Object[c.getParameterCount()]);
      |    } catch (java.lang.reflect.InvocationTargetException e) {
      |      if (e.getCause() instanceof Error) throw (Error) e.getCause();
      |      throw new RuntimeException(e);
      |    } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }
      |  }
      |  /** Builds the class named after `type` plus a suffix (jboss-logging's `Foo_$logger`). */
      |  public static Object generate(Class<?> type) {
      |    try { return Class.forName(type.getName() + "_$logger").getDeclaredConstructor().newInstance(); }
      |    catch (java.lang.reflect.InvocationTargetException e) {
      |      if (e.getCause() instanceof Error) throw (Error) e.getCause();
      |      throw new RuntimeException(e);
      |    } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }
      |  }
      |  /** Builds the member classes that carry the stereotype. */
      |  public static void buildNested(Class<?> outer) {
      |    for (Class<?> n : outer.getDeclaredClasses()) if (n.isAnnotationPresent(Stereo.class)) build(n);
      |  }
      |  /** Builds the class named inside each annotation's own `@Imports`. */
      |  public static void buildImports(Class<?> holder) {
      |    for (java.lang.annotation.Annotation a : holder.getAnnotations()) {
      |      Imports i = a.annotationType().getAnnotation(Imports.class);
      |      if (i != null) build(i.value());
      |    }
      |  }
      |}
      |""".stripMargin

  private val stereotypes = Keep.classNames("lib/Stereo")
  private val rules       = Seq(Keep.Constructors(stereotypes), Keep.RequiresClasses(Keep.classNames("lib/Needs")), Keep.ClassValues(Keep.classNames("lib/Imports")))

  private final class Scenario(val name: String, val lib: Map[String, String], val body: String, val keep: Seq[Keep], val expected: Seq[Row],
      val failsAtRuntime: Boolean, val resources: Map[String, String] = Map.empty, val removed: Set[String] = Set.empty)

  private val svc = "lib/Svc.java" -> "package lib; @Stereo public class Svc { public Svc(String s) { Gone.x(); } }"

  private val scenarios = Seq(
    Scenario("without a rule a constructor with arguments only the framework calls is missed", annotations + container + svc,
      "lib.Container.build(lib.Svc.class);", Seq.empty, Seq(("lib.Svc", "<init>", Label.Latent)), failsAtRuntime = true),

    Scenario("a stereotype class is built through a constructor that takes arguments", annotations + container + svc,
      "lib.Container.build(lib.Svc.class);", rules, Seq(("lib.Svc", "<init>", Label.Reflective)), failsAtRuntime = true),

    // The class is named in a file, so only a rule roots it; its condition decides whether the framework looks at it.
    Scenario("a class whose condition names an absent class is ignored by the framework",
      annotations + container + ("lib/Svc2.java" -> "package lib; @Stereo @Needs(Absent.class) public class Svc2 { public Svc2(String s) { Gone.x(); } }") +
        ("lib/Absent.java" -> "package lib; public class Absent { }"),
      "", rules :+ Keep.ClassesNamedIn(Keep.fileNames("lib.conf")), Seq.empty, failsAtRuntime = false, resources = Map("lib.conf" -> "service = lib.Svc2"),
      removed = Set("lib/Absent")),

    Scenario("a class whose condition names a class that is there is built",
      annotations + container + ("lib/Svc2.java" -> "package lib; @Stereo @Needs(Other.class) public class Svc2 { public Svc2(String s) { Gone.x(); } }"),
      "", rules :+ Keep.ClassesNamedIn(Keep.fileNames("lib.conf")), Seq(("lib.Svc2", "<init>", Label.Reflective)), failsAtRuntime = false,
      resources = Map("lib.conf" -> "service = lib.Svc2")),

    Scenario("a class named by an annotation of one of its annotations is built",
      annotations + container + ("lib/Target.java" -> "package lib; public class Target { public Target() { Gone.x(); } }") +
        ("lib/Holder.java" -> "package lib; @Conf public class Holder { }"),
      "lib.Container.buildImports(lib.Holder.class);", rules, Seq(("lib.Target", "<init>", Label.Reflective)), failsAtRuntime = true),

    Scenario("without the rule that class is never reached", annotations + container + ("lib/Target.java" -> "package lib; public class Target { public Target() { Gone.x(); } }") +
        ("lib/Holder.java" -> "package lib; @Conf public class Holder { }"),
      "lib.Container.buildImports(lib.Holder.class);", Seq.empty, Seq.empty, failsAtRuntime = true),

    // The name is `type.getName() + "_$logger"`, computed at run time: only a rule that says what the suffix means can see the class.
    Scenario("a class named after another plus a suffix is built by a framework handed the first",
      annotations + container + ("lib/Api.java" -> "package lib; public interface Api { }") +
        ("lib/Api_$logger.java" -> "package lib; public class Api_$logger implements Api { public Api_$logger() { Gone.x(); } }"),
      "lib.Container.generate(lib.Api.class);", rules :+ Keep.GeneratedClasses(Keep.suffixes("_$logger")), Seq(("lib.Api_$logger", "<init>", Label.Reflective)), failsAtRuntime = true),

    Scenario("without the rule the generated class is never reached", annotations + container + ("lib/Api.java" -> "package lib; public interface Api { }") +
        ("lib/Api_$logger.java" -> "package lib; public class Api_$logger implements Api { public Api_$logger() { Gone.x(); } }"),
      "lib.Container.generate(lib.Api.class);", rules, Seq.empty, failsAtRuntime = true),

    Scenario("a member class of a stereotype class is built too",
      annotations + container + ("lib/Outer.java" -> "package lib; @Stereo public class Outer { @Stereo public static class Inner { public Inner(String s) { Gone.x(); } } }"),
      "lib.Container.buildNested(lib.Outer.class);", rules, Seq(("lib.Outer$Inner", "<init>", Label.Reflective)), failsAtRuntime = true)
  )

  private def analyse(app: Path, lib: Path, keep: Seq[Keep], precision: Precision): Vector[Row] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val locator = Locator(index)
    Checker.findings(index, scope, CheckOptions(precision, keep)).map: f =>
      (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label)
    .distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(s.lib, s.resources, s.removed)
      val app = compile(Map("app/Main.java" -> s"package app; public class Main { public static void run() { ${s.body} } }"), Seq(v1))
      val expected = s.expected.sortBy(_.toString).toVector
      assertEquals(analyse(app, v2, s.keep, Precision.Method), expected)
      assertEquals(analyse(app, v2, s.keep, Precision.MethodRta), expected, "under RTA")
      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
    }
