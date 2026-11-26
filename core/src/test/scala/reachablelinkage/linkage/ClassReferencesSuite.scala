package reachablelinkage.linkage

import reachablelinkage.fixture.OracleSuite
import scala.jdk.CollectionConverters.*
import reachablelinkage.api.{CheckScope, ClasspathEntry, Linkage, LinkageRequest}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.MissinglinkOracle
import reachablelinkage.fixture.MissinglinkOracle.Key

/** Class references that are not method calls, field accesses or `new`: loading a class literal, a catch handler's exception type, and `multianewarray`. Each
  * loads the class when executed, so a class that disappeared from a dependency is a `NoClassDefFoundError`.
  */
class ClassReferencesSuite extends OracleSuite:

  private val v1 = Map(
    "lib/Gone.java"   -> "package lib; public class Gone {}",
    "lib/GoneEx.java" -> "package lib; public class GoneEx extends RuntimeException {}",
    "lib/Fn.java"     -> "package lib; public interface Fn { void run(); }"
  )

  private final class Case(val name: String, val body: String, val missing: String)

  private val cases = Seq(
    Case("class literal", "return lib.Gone.class;", "lib/Gone"),
    Case("array class literal", "return lib.Gone[].class;", "lib/Gone"),
    Case("catch handler", "try { return System.nanoTime(); } catch (lib.GoneEx e) { return null; }", "lib/GoneEx"),
    Case("multianewarray", "lib.Gone[][] a = new lib.Gone[1][1]; return a;", "lib/Gone"),
    // The call site of a lambda names the functional interface it makes (its descriptor's return type) and the types of what it captures.
    Case("lambda to a functional interface", "lib.Fn f = () -> { }; return f;", "lib/Fn"),
    // The lambda's own signature is a method type the bootstrap method is given: `Gone` is nowhere else in `run`.
    Case("lambda over a type that vanished", "java.util.function.Function<lib.Gone, String> f = g -> \"x\"; return f;", "lib/Gone")
  )

  private def build(c: Case) =
    val libV1 = compile(v1)
    val libV2 = compile(Map("lib/Other.java" -> "package lib; public class Other {}"))
    val app   = compile(
      Map(
        "app/Main.java" ->
          ("package app;\npublic class Main {\n  public static Object run() {\n    " + c.body + "\n  }\n}")
      ),
      Seq(libV1)
    )
    (app, libV2)

  for c <- cases do
    test(s"${c.name}: a class that vanished is reported, at the right method") {
      val (app, libV2) = build(c)
      val r            = Linkage.check(
        LinkageRequest(java.util.List.of(app), java.util.List.of(ClasspathEntry("lib", libV2)), CheckScope.App, java.util.Set.of(), java.util.List.of())
      )
      val found = r.conflicts.asScala.map(x => (x.dependency.fromClass, x.dependency.fromMethodName, x.dependency.targetClass)).toSet
      assertEquals(found, Set(("app.Main", "run", c.missing.replace('/', '.'))))
    }

    test(s"${c.name}: what missinglink-core does") {
      val (app, libV2) = build(c)
      val got          = MissinglinkOracle.check(source(app), Seq(source(libV2)), scanLibs = false)
      assertEquals(got, Set.empty[Key], "missinglink-core does not look at this kind of reference")
    }

  // ---- what the JVM really does at a lambda whose interface is gone ----

  private def runOnJvm(body: String): (Option[Throwable], List[reachablelinkage.api.ConflictLabel]) =
    val c      = Case("lambda", body, "lib/Fn")
    val libV1  = compile(v1)
    val libV2  = compile(Map("lib/Other.java" -> "package lib; public class Other {}"))
    val app    = compile(Map("app/Main.java" -> ("package app;\npublic class Main {\n  public static void run() {\n    " + c.body + "\n  }\n}")), Seq(libV1))
    val labels = Linkage
      .check(LinkageRequest(java.util.List.of(app), java.util.List.of(ClasspathEntry("lib", libV2)), CheckScope.App, java.util.Set.of(), java.util.List.of()))
      .conflicts
      .asScala
      .map(_.label)
      .toList
    (reachablelinkage.fixture.RuntimeOracle.run(Seq(app, libV2)), labels)

  test("the JVM throws NoClassDefFoundError at a lambda whose functional interface is gone, and we report it Reachable") {
    val (error, labels) = runOnJvm("lib.Fn f = () -> { };")
    assert(error.exists(_.isInstanceOf[NoClassDefFoundError]), s"expected NoClassDefFoundError, got $error")
    assertEquals(labels, List(reachablelinkage.api.ConflictLabel.REACHABLE))
  }

  test("a catch for NoClassDefFoundError really stops it, so the site is Guarded; one for another error does not") {
    val (survived, guarded) = runOnJvm("try { lib.Fn f = () -> { }; } catch (NoClassDefFoundError e) { }")
    assertEquals(survived, None)
    assertEquals(guarded, List(reachablelinkage.api.ConflictLabel.GUARDED))
    val (failed, unguarded) = runOnJvm("try { lib.Fn f = () -> { }; } catch (NoSuchMethodError e) { }")
    assert(failed.exists(_.isInstanceOf[NoClassDefFoundError]), s"got $failed")
    assertEquals(unguarded, List(reachablelinkage.api.ConflictLabel.REACHABLE))
  }
