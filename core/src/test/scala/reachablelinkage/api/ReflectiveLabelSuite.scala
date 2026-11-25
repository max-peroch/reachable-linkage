package reachablelinkage.api

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.{GoneLibrary, Jars}

/**
 * `REFLECTIVE`: reachable only because a reflection rule, a keep rule or the class-load log made code a root. The
 * application's own call path never leads to it. It must still fail by default, since the JVM really breaks.
 */
class ReflectiveLabelSuite extends munit.FunSuite:

  private val wired = "lib/Wired.java" -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Wired { }"
  private val container = "lib/Container.java" ->
    "package lib; public class Container { public static Object start(Class<?> t) { try { return t.getDeclaredConstructor().newInstance(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); } } }"
  private val svc = "lib/Svc.java" -> "package lib; public class Svc { public Svc() { } @Wired public void go() { Gone.x(); } public void other() { Gone.x(); } }"
  private val rules = KeepRule.parse(java.nio.file.Files.writeString(java.nio.file.Files.createTempFile("rules", ".txt"), "keep-members lib.Wired\n"))
    .map(_.asJava).fold(e => throw new AssertionError(e.message), identity)

  private def check(lib: Map[String, String], body: String, keep: java.util.List[KeepRule] = rules, precision: CheckPrecision = CheckPrecision.METHOD): (LinkageResult, Path, Path) =
    val (v1, v2) = GoneLibrary.build(lib)
    val app = compile(Map("app/Main.java" -> s"package app; public class Main { public static void run() { $body } }"), Seq(v1))
    (Linkage.check(LinkageRequest(java.util.List.of(app), java.util.List.of(ClasspathEntry("lib", Jars.of(v2))), CheckScope.App,
      java.util.Set.of(), java.util.List.of(), precision, java.util.Optional.empty[Path](), keep)), app, v2)

  private def labels(r: LinkageResult): List[(String, ConflictLabel)] =
    r.conflicts.asScala.map(c => (c.dependency.fromMethodName, c.label)).toList.sortBy(_.toString)

  test("a method only a keep rule reaches is REFLECTIVE, one the code calls is REACHABLE, one nothing reaches is LATENT") {
    val lib = Map(wired, container, svc)
    val (r, _, _) = check(lib, "lib.Container.start(lib.Svc.class);")
    assertEquals(labels(r), List(("go", ConflictLabel.REFLECTIVE), ("other", ConflictLabel.LATENT)))
    // The same break, called directly: now the application's own code reaches it.
    val (direct, _, _) = check(lib, "lib.Container.start(lib.Svc.class); new lib.Svc().go();")
    assertEquals(labels(direct), List(("go", ConflictLabel.REACHABLE), ("other", ConflictLabel.LATENT)))
  }

  test("a method reachable both through a rule and a direct call is REACHABLE: the direct path wins, whatever the order") {
    val lib = Map(wired, container, svc)
    for body <- Seq("new lib.Svc().go(); lib.Container.start(lib.Svc.class);", "lib.Container.start(lib.Svc.class); new lib.Svc().go();") do
      assertEquals(labels(check(lib, body)._1).find(_._1 == "go").map(_._2), Some(ConflictLabel.REACHABLE), body)
  }

  test("what a reflective method calls is reflective too, and what a direct method calls stays direct") {
    val lib = Map(wired, container,
      "lib/Svc.java" -> "package lib; public class Svc { public Svc() { } @Wired public void go() { helper(); } void helper() { Gone.x(); } }")
    assertEquals(labels(check(lib, "lib.Container.start(lib.Svc.class);")._1), List(("helper", ConflictLabel.REFLECTIVE)))
    assertEquals(labels(check(lib, "lib.Container.start(lib.Svc.class); new lib.Svc().go();")._1), List(("helper", ConflictLabel.REACHABLE)))
  }

  test("a constant class name is REFLECTIVE as well, and Class precision, which has no call graph, says REACHABLE") {
    val lib = Map("lib/Impl.java" -> "package lib; public class Impl { public Impl() { Gone.x(); } }")
    val body = "try { Class.forName(\"lib.Impl\").getDeclaredConstructor().newInstance(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }"
    assertEquals(labels(check(lib, body, precision = CheckPrecision.METHOD)._1), List(("<init>", ConflictLabel.REFLECTIVE)))
    assertEquals(labels(check(lib, body, precision = CheckPrecision.METHOD_RTA)._1), List(("<init>", ConflictLabel.REFLECTIVE)))
    assertEquals(labels(check(lib, body, precision = CheckPrecision.CLASS)._1), List(("<init>", ConflictLabel.REACHABLE)))
  }

  test("a handler around the broken call still wins over REFLECTIVE") {
    val guarded = Map("lib/Impl.java" -> "package lib; public class Impl { public Impl() { try { Gone.x(); } catch (NoSuchMethodError e) { } } }")
    val body = "try { Class.forName(\"lib.Impl\").getDeclaredConstructor().newInstance(); } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }"
    assertEquals(labels(check(guarded, body)._1), List(("<init>", ConflictLabel.GUARDED)))
  }

  test("FailOn.REACHABLE (the default) fails on REFLECTIVE, FailOn.DIRECT does not, FailOn.ANY counts all") {
    val lib = Map(wired, container, svc)
    val (r, _, _) = check(lib, "lib.Container.start(lib.Svc.class);")
    assertEquals(r.failing().asScala.map(_.label).toList, List(ConflictLabel.REFLECTIVE))
    assertEquals(r.failing(FailOn.REACHABLE).size, 1)
    assertEquals(r.failing(FailOn.DIRECT).size, 0)
    assertEquals(r.failing(FailOn.ANY).size, 2)
    val (direct, _, _) = check(lib, "lib.Container.start(lib.Svc.class); new lib.Svc().go();")
    assertEquals(direct.failing(FailOn.DIRECT).size, 1)
  }

  test("the report puts REFLECTIVE between REACHABLE and PROBED") {
    assert(ConflictLabel.REACHABLE.severity() > ConflictLabel.REFLECTIVE.severity())
    assert(ConflictLabel.REFLECTIVE.severity() > ConflictLabel.PROBED.severity())
    val (r, _, _) = check(Map(wired, container, svc), "lib.Container.start(lib.Svc.class);")
    val text = Report.render(r.conflicts, ReportOptions()).mkString("\n")
    assert(text.contains("REFLECTIVE"), text)
  }
