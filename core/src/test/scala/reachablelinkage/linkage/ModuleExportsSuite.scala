package reachablelinkage.linkage

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.*
import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{ChildRun, Scopes}
import reachablelinkage.symbols.Symbols

/** A public JDK class in a package its module does not export (`sun.security.x509` in `java.base`) cannot be named from the classpath: the JVM throws
  * `IllegalAccessError`, unless it is started with `--add-exports`. Each scenario runs in a child JVM with and without the flag, and the check must agree with
  * both.
  */
class ModuleExportsSuite extends munit.FunSuite:

  private val x509  = "java.base/sun.security.x509=ALL-UNNAMED"
  private val misc  = "java.base/jdk.internal.misc=ALL-UNNAMED"
  private val javac = Seq("--add-exports", x509, "--add-exports", misc)

  private def app(body: String): Path =
    compile(Map("app/Main.java" -> s"package app; public class Main { public static void run() { $body } }"), Nil, javac)

  private def flags(exports: String*): Seq[String] = exports.flatMap(e => Seq("--add-exports", e))

  private def rows(app: Path, precision: Precision, exported: Set[String] = Set.empty): Vector[(String, Label)] =
    val syms  = new Symbols
    val index = ClassIndex(Vector(source(app), ClassSource.jdk()), syms)
    val scope = Scopes.of(index, Vector(0), Vector(0), Vector(0))
    Checker
      .findings(index, scope, CheckOptions(precision, exportedPackages = exported))
      .collect:
        case f if f.conflict.isInstanceOf[Conflict.NotExported] => (f.conflict.show(syms), f.label)
      .distinct

  test("an unexported package is a conflict, and the JVM agrees: it fails without --add-exports and works with it") {
    val a       = app("Object o = sun.security.x509.X500Name.class;")
    val failure = ChildRun.run(Seq(a), jvmArgs = Nil)
    assert(failure.exists(_.exists(_._1 == "java.lang.IllegalAccessError")), s"expected IllegalAccessError, got $failure")
    assertEquals(ChildRun.run(Seq(a), jvmArgs = flags(x509)), None, "the control: with the flag it works")
    for precision <- Seq(Precision.Class, Precision.Method, Precision.MethodRta) do
      val found = rows(a, precision)
      assertEquals(found.map(_._2), Vector(Label.Reachable), precision.toString)
      assert(found.head._1.contains("sun.security.x509.X500Name"), found.toString)
  }

  test("naming the package in the request's exported packages says the JVM flag is there, so nothing is reported") {
    assertEquals(rows(app("Object o = sun.security.x509.X500Name.class;"), Precision.Method, Set("sun.security.x509")), Vector.empty)
    assertEquals(
      rows(app("Object o = sun.security.x509.X500Name.class;"), Precision.Method, Set("sun.security")).size,
      1,
      "a parent package is not the package"
    )
  }

  test("a package exported only to some modules is still not exported to the classpath") {
    val a = app("Object o = jdk.internal.misc.Unsafe.class;")
    assert(ChildRun.run(Seq(a), jvmArgs = Nil).exists(_.exists(_._1 == "java.lang.IllegalAccessError")))
    assertEquals(ChildRun.run(Seq(a), jvmArgs = flags(misc)), None)
    assertEquals(rows(a, Precision.Method).size, 1)
  }

  test("packages the JDK does export, including sun.misc from jdk.unsupported, are not reported") {
    val a = app("Object a = java.util.ArrayList.class; Object b = sun.misc.Unsafe.class; Object c = javax.xml.parsers.DocumentBuilderFactory.class;")
    assertEquals(ChildRun.run(Seq(a), jvmArgs = Nil), None, "all three work on this JVM without any flag")
    assertEquals(rows(a, Precision.Class), Vector.empty)
  }

  test("a handler for IllegalAccessError guards the access, and the program really survives") {
    val a = app("try { Object o = sun.security.x509.X500Name.class; } catch (IllegalAccessError e) { }")
    assertEquals(ChildRun.run(Seq(a), jvmArgs = Nil), None)
    assertEquals(rows(a, Precision.Method).map(_._2), Vector(Label.Guarded))
  }

  test("code nothing reachable runs is Latent at Method precision") {
    val a = compile(
      Map(
        "app/Main.java" ->
          "package app; public class Main { public static void run() { } public static void unused() { Object o = sun.security.x509.X500Name.class; } }"
      ),
      Nil,
      javac
    )
    assertEquals(rows(a, Precision.Method).map(_._2), Vector(Label.Reachable), "every method of an application class is a root")
    val lib = compile(
      Map(
        "lib/Util.java" -> "package lib; public class Util { public static void used() { } public static void unused() { Object o = sun.security.x509.X500Name.class; } }"
      ),
      Nil,
      javac
    )
    val main   = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.used(); } }"), Seq(lib))
    val syms   = new Symbols
    val index  = ClassIndex(Vector(source(main), source(lib), ClassSource.jdk()), syms)
    val scope  = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val labels = Checker.findings(index, scope, Precision.Method).collect { case f if f.conflict.isInstanceOf[Conflict.NotExported] => f.label }
    assertEquals(labels.distinct, Vector(Label.Latent))
  }

  test("through the facade: the category, and the exported packages of the request") {
    val a                                      = app("Object o = sun.security.x509.X500Name.class;")
    def check(exported: java.util.Set[String]) =
      Linkage.check(LinkageRequest.builder().withProjectClasses(java.util.List.of(a)).withExportedPackages(exported).build())
    val found = check(java.util.Set.of())
    assertEquals(found.conflicts.asScala.map(c => (c.category, c.label)).toList, List((ConflictCategory.NOT_EXPORTED, ConflictLabel.REACHABLE)))
    assert(found.conflicts.get(0).reason.contains("does not export"), found.conflicts.get(0).reason)
    assertEquals(check(java.util.Set.of("sun.security.x509")).conflicts.size, 0)
  }
