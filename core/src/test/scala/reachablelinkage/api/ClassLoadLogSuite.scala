package reachablelinkage.api

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.{GoneLibrary, Jars, RunLog}

class ClassLoadLogSuite extends munit.FunSuite:

  private def file(lines: String*): Path =
    val f = Files.createTempFile("class-load", ".log"); Files.write(f, lines.asJava); f

  private def classes(lines: String*): List[String] = ClassLoadLog.fromLines(lines.toList)

  test("each log format the JVM writes names the loaded class") {
    assertEquals(classes("[0.084s][info][class,load] app.Main source: file:/x/app/"), List("app.Main"))
    assertEquals(classes("app.Main source: file:/x/app/"), List("app.Main"), "no decorations")
    assertEquals(classes("[0.085s] app.Main source: file:/x/app/"), List("app.Main"), "uptime only, no tags")
    assertEquals(classes("[0.014s][info][class,load] java.lang.Object source: shared objects file"), List("java.lang.Object"))
    assertEquals(classes("[Loaded app.Main from file:/x/app/]"), List("app.Main"), "Java 8 -verbose:class")
    assertEquals(classes("[0.1s][info][class,load] app.Outer$Inner source: jar:file:/x/a.jar!/"), List("app.Outer$Inner"))
  }

  test("hidden classes, other log output and blank lines name nothing") {
    assertEquals(
      classes(
        "[0.1s][info][class,load] app.Main$$Lambda/0x0000000800c03000 source: app.Main",
        "[0.1s][info][class,load] java.lang.invoke.LambdaForm$MH/0x000001 source: __JVM_LookupDefineClass__",
        "[0.1s][info][gc] Using G1",
        "[Loaded app.Main$$Lambda$14/1234567 from app.Main]",
        "",
        "Hello from the application",
        "source: nothing"
      ),
      Nil
    )
  }

  test("a class is listed once, in the order it was first loaded") {
    assertEquals(classes("a.B source: x", "c.D source: x", "a.B source: x"), List("a.B", "c.D"))
  }

  test("a real log, made by a real JVM, names the program's classes and the JDK's") {
    val (v1, _) = GoneLibrary.build(Map("lib/Util.java" -> "package lib; public class Util { public static void hi() { } }"))
    val app     = compile(Map("app/Launch.java" -> "package app; public class Launch { public static void main(String[] a) { lib.Util.hi(); } }"), Seq(v1))
    val log     = RunLog.of(Seq(app, v1), "app.Launch")
    val parsed  = ClassLoadLog.parse(log).fold(e => fail(e.message), _.classes.asScala.toSet)
    assert(Set("app.Launch", "lib.Util", "java.lang.Object").subsetOf(parsed), parsed.take(5).toString)
    assert(parsed.size > 100, "a JVM loads hundreds of classes before main")
  }

  test("a file with no class-load line is rejected, and one that is missing is unreadable") {
    assertEquals(ClassLoadLog.parse(file("hello", "world")).left.map(_.getClass), Left(classOf[ClassLoadLogError.NoClassesLogged]))
    assert(ClassLoadLog.parse(Files.createTempDirectory("logs").resolve("nope.log")).left.exists(_.isInstanceOf[ClassLoadLogError.Unreadable]))
    assert(ClassLoadLog.parse(file("hello")).left.toOption.get.message.contains("-Xlog:class+load"))
  }

  // ---- through the facade ----

  private val lib = Map(
    "lib/Svc.java"  -> "package lib; public interface Svc { void run(); }",
    "lib/Impl.java" -> "package lib; public class Impl implements Svc { public void run() { Gone.x(); } }"
  )

  private def check(loaded: Set[String]): LinkageResult =
    val (v1, v2) = GoneLibrary.build(lib)
    val app      = compile(
      Map(
        "app/Main.java" -> "package app; public class Main { public static void run() { try { ((lib.Svc) Class.forName(String.join(\"\", \"lib.\", \"Im\", \"pl\")).getDeclaredConstructor().newInstance()).run(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); } } }"
      ),
      Seq(v1)
    )
    Linkage.check(
      LinkageRequest(
        java.util.List.of(app),
        java.util.List.of(ClasspathEntry("lib", Jars.of(v2))),
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.METHOD,
        java.util.Optional.empty[Path](),
        KeepRule.standard(),
        loaded.asJava
      )
    )

  test("a class the log names is checked, though the application never mentions it") {
    val without = check(Set.empty)
    assertEquals(without.conflicts.size, 0)
    assertEquals(without.warnings.size, 0)
    val withLog = check(Set("lib.Impl"))
    assertEquals(withLog.conflicts.asScala.map(c => (c.dependency.fromClass, c.label)).toList, List(("lib.Impl", ConflictLabel.REFLECTIVE)))
    assertEquals(withLog.warnings.size, 0)
  }

  test("a log that names nothing on the classpath is a warning, not an error") {
    val result = check(Set("java.lang.Object", "some.other.App"))
    assertEquals(result.conflicts.size, 0)
    assertEquals(result.warnings.size, 1)
    assert(result.warnings.get(0).contains("class-load log"), result.warnings)
  }

  test("a log with a byte that is not UTF-8 (a path in the platform encoding) is still read") {
    val f = Files.createTempFile("class-load", ".log")
    Files.write(f, "a.B source: file:/x/".getBytes("UTF-8") ++ Array(0xe9.toByte) ++ "/y\nc.D source: x\n".getBytes("UTF-8"))
    assertEquals(ClassLoadLog.parse(f).map(_.classes.asScala.toList), Right(List("a.B", "c.D")))
  }

  test("the classes of a log cannot be changed by whoever holds it") {
    val log = ClassLoadLog.parse(file("a.B source: x")).fold(e => fail(e.message), identity)
    intercept[UnsupportedOperationException](log.classes.add("evil.Injected"))
  }
