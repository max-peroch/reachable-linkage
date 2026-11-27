package reachablelinkage.api

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.GoneLibrary
import reachablelinkage.fixture.JavaFixture.compile

/** A conflict that fails can say how the application reaches it: the methods on a shortest path of the call graph, from the application's own to the one that
  * holds the broken reference.
  */
class RouteSuite extends munit.FunSuite:

  private val lib = Map(
    "lib/B.java" -> "package lib; public class B { public static void mid() { A.go(); } }",
    "lib/A.java" -> "package lib; public class A { public static void go() { Gone.x(); } public static void unused() { Gone.x(); } }"
  )

  private def check(routes: Boolean): List[LinkageConflict] =
    val (v1, v2) = GoneLibrary.build(lib)
    val app      = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.B.mid(); } }"), Seq(v1))
    val request  = LinkageRequest
      .builder()
      .withProjectClasses(java.util.List.of(app))
      .withClasspath(java.util.List.of(ClasspathEntry("lib", v2)))
      .withScope(CheckScope.App)
      .withRoutes(routes)
      .build()
    Linkage.check(request).conflicts.asScala.toList

  test("a conflict that fails says the methods that lead to it from the application, in order") {
    val failing = check(routes = true).filter(_.label == ConflictLabel.REACHABLE)
    assertEquals(failing.size, 1)
    assertEquals(failing.head.route.asScala.toList, List("app.Main.run", "lib.B.mid", "lib.A.go"))
  }

  test("a conflict nothing reaches has no route") {
    val latent = check(routes = true).filter(_.label == ConflictLabel.LATENT)
    assertEquals(latent.size, 1)
    assertEquals(latent.head.route.size, 0)
  }

  test("no route is given unless it is asked for") {
    assert(check(routes = false).forall(_.route.isEmpty))
  }

  test("the bridge takes `routes` and answers each conflict with its `route`") {
    val (v1, v2) = GoneLibrary.build(lib)
    val app      = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.B.mid(); } }"), Seq(v1))
    def asked(routes: AnyRef): java.util.Map[String, AnyRef] =
      val request = new java.util.HashMap[String, AnyRef]()
      request.put("projectClasses", java.util.List.of(app.toString))
      request.put("classpath", java.util.List.of(java.util.List.of("lib", v2.toString)))
      request.put("routes", routes)
      LinkageBridge.check(request)
    val response  = asked(java.lang.Boolean.TRUE)
    val conflicts = response.get("conflicts").asInstanceOf[java.util.List[java.util.Map[String, AnyRef]]].asScala.toList
    val failing   = conflicts.filter(_.get("label") == "REACHABLE")
    assertEquals(failing.map(_.get("route").asInstanceOf[java.util.List[String]].asScala.toList), List(List("app.Main.run", "lib.B.mid", "lib.A.go")))
    val refused = asked("yes")
    assert(refused.containsKey("error"), refused.toString)
    assert(!refused.containsKey("conflicts"), refused.toString)
  }
