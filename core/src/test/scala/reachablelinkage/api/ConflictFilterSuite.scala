package reachablelinkage.api

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.{GoneLibrary, Jars}

/** The package filters, with the semantics of sbt-missinglink's four settings. */
class ConflictFilterSuite extends munit.FunSuite:

  private def conflict(from: String, target: String, label: ConflictLabel = ConflictLabel.REACHABLE): LinkageConflict =
    LinkageConflict(
      ConflictCategory.METHOD_SIGNATURE_NOT_FOUND,
      Dependency(from, "run", "()V", 1, target, "go", "()V"),
      "project",
      "lib",
      "Method not found",
      label
    )

  private def selector(name: String, subpackages: Boolean = true): PackageSelector =
    PackageSelector.of(name, subpackages).fold(e => fail(e.message), identity)

  private def filter(
      ignoreSource: List[PackageSelector] = Nil,
      targetSource: List[PackageSelector] = Nil,
      ignoreDestination: List[PackageSelector] = Nil,
      targetDestination: List[PackageSelector] = Nil
  ): ConflictFilter =
    ConflictFilter.of(ignoreSource, targetSource, ignoreDestination, targetDestination).fold(e => fail(e.message), identity)

  private def apply(f: ConflictFilter, cs: LinkageConflict*): (List[(String, String)], List[FilterEffect]) =
    val out = f(cs.asJava)
    (out.conflicts.asScala.map(c => (c.dependency.fromClass, c.dependency.targetClass)).toList, out.effects.asScala.toList)

  private val a = conflict("app.Main", "lib.api.Api")
  private val b = conflict("app.Main", "lib.impl.Impl")
  private val c = conflict("other.Thing", "lib.api.sub.Deep")

  test("no filter keeps everything and reports nothing") {
    assertEquals(
      apply(ConflictFilter.none, a, b, c),
      (List(("app.Main", "lib.api.Api"), ("app.Main", "lib.impl.Impl"), ("other.Thing", "lib.api.sub.Deep")), Nil)
    )
  }

  test("ignoring a package drops its classes and, by default, its subpackages") {
    val (kept, effects) = apply(filter(ignoreDestination = List(selector("lib.api"))), a, b, c)
    assertEquals(kept, List(("app.Main", "lib.impl.Impl")))
    assertEquals(effects, List(FilterEffect(FilterKind.IGNORE_DESTINATION_PACKAGES, 2)))
  }

  test("ignoring without subpackages drops only the package itself") {
    val (kept, _) = apply(filter(ignoreDestination = List(selector("lib.api", subpackages = false))), a, b, c)
    assertEquals(kept.map(_._2), List("lib.impl.Impl", "lib.api.sub.Deep"))
  }

  test("a package name matches on a package boundary: com.acme is not com.acmeco") {
    val x = conflict("app.Main", "com.acmeco.Thing")
    val y = conflict("app.Main", "com.acme.Thing")
    assertEquals(apply(filter(ignoreDestination = List(selector("com.acme"))), x, y)._1, List(("app.Main", "com.acmeco.Thing")))
    assertEquals(apply(filter(targetDestination = List(selector("com.acme"))), x, y)._1, List(("app.Main", "com.acme.Thing")))
  }

  test("the source side looks at the class that holds the reference, the destination at the one it points at") {
    assertEquals(apply(filter(ignoreSource = List(selector("app"))), a, c)._1.map(_._1), List("other.Thing"))
    assertEquals(apply(filter(ignoreDestination = List(selector("app"))), a, c)._1.size, 2, "no destination is in app")
  }

  test("targeting keeps only the conflicts in the listed packages") {
    val (kept, effects) = apply(filter(targetSource = List(selector("other"))), a, b, c)
    assertEquals(kept, List(("other.Thing", "lib.api.sub.Deep")))
    assertEquals(effects, List(FilterEffect(FilterKind.TARGET_SOURCE_PACKAGES, 2)))
    assertEquals(apply(filter(targetDestination = List(selector("lib.api", subpackages = false))), a, b, c)._1.map(_._2), List("lib.api.Api"))
  }

  test("several packages in one list: an ignore drops a match of any, a target keeps a match of any") {
    assertEquals(apply(filter(ignoreDestination = List(selector("lib.api"), selector("lib.impl"))), a, b, c)._1, Nil)
    assertEquals(
      apply(filter(targetDestination = List(selector("lib.impl"), selector("lib.api.sub"))), a, b, c)._1.map(_._2),
      List("lib.impl.Impl", "lib.api.sub.Deep")
    )
  }

  test("the four filters apply in order, each only when set, and each says what it removed") {
    val f               = filter(ignoreSource = List(selector("other")), targetDestination = List(selector("lib.api")))
    val (kept, effects) = apply(f, a, b, c)
    assertEquals(kept, List(("app.Main", "lib.api.Api")))
    assertEquals(effects, List(FilterEffect(FilterKind.IGNORE_SOURCE_PACKAGES, 1), FilterEffect(FilterKind.TARGET_DESTINATION_PACKAGES, 1)))
  }

  test("an ignore list and a target list on the same side are rejected, on either side") {
    assertEquals(
      ConflictFilter.of(ignoreSource = List(selector("a")), targetSource = List(selector("b"))).left.toOption,
      Some(ConflictFilterError.IgnoreAndTargetBoth(FilterSide.SOURCE))
    )
    assertEquals(
      ConflictFilter.of(ignoreDestination = List(selector("a")), targetDestination = List(selector("b"))).left.toOption,
      Some(ConflictFilterError.IgnoreAndTargetBoth(FilterSide.DESTINATION))
    )
    assert(ConflictFilter.of(ignoreSource = List(selector("a")), targetDestination = List(selector("b"))).isRight, "different sides are fine")
    assert(ConflictFilterError.IgnoreAndTargetBoth(FilterSide.SOURCE).message.contains("ignoreSourcePackages"))
  }

  test("a package name must be a dotted name") {
    for bad <- List("", "a..b", ".a", "a.", "1a", "a-b", "a b") do
      assertEquals(PackageSelector.of(bad), Left(ConflictFilterError.InvalidPackageName(bad)), s"'$bad'")
    for good <- List("a", "com.acme", "com.acme.v2", "a.b$c") do assert(PackageSelector.of(good).isRight, good)
  }

  test("a class in the default package has no package to match, and does not throw") {
    val d = conflict("Main", "Gone")
    assertEquals(apply(filter(ignoreSource = List(selector("app"))), d)._1, List(("Main", "Gone")), "ignoring app does not drop it")
    assertEquals(apply(filter(targetSource = List(selector("app"))), d)._1, Nil, "targeting app does not keep it")
  }

  test("an inner class is in its outer class's package, and the order of what is kept does not change") {
    val inner = conflict("app.Main$Inner", "lib.api.Api$Nested")
    assertEquals(
      apply(filter(ignoreDestination = List(selector("lib.api"))), inner, b)._1,
      List(("app.Main", "lib.impl.Impl")),
      "the nested class is in lib.api, so it is ignored"
    )
    assertEquals(apply(filter(ignoreSource = List(selector("app"))), inner, c)._1.map(_._1), List("other.Thing"), "app.Main$Inner is in app")
    assertEquals(apply(filter(targetDestination = List(selector("lib.api"))), b, inner, a)._1.map(_._2), List("lib.api.Api$Nested", "lib.api.Api"))
  }

  // ---- through the facade, with real jars ----

  private val lib = Map(
    "lib/api/Api.java"   -> "package lib.api; public class Api { public static void x() { } }",
    "lib/impl/Impl.java" -> "package lib.impl; public class Impl { public static void y() { } }"
  )

  private def check(filter: ConflictFilter): LinkageResult =
    val (v1, _) = GoneLibrary.build(lib)
    // v2 of the library lacks both methods: rebuild it without them
    val gone = compile(
      Map(
        "lib/api/Api.java"   -> "package lib.api; public class Api { }",
        "lib/impl/Impl.java" -> "package lib.impl; public class Impl { }",
        "lib/Gone.java"      -> "package lib; public class Gone { }"
      )
    )
    val app = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.api.Api.x(); lib.impl.Impl.y(); } }"), Seq(v1))
    Linkage.check(
      LinkageRequest(
        java.util.List.of(app),
        java.util.List.of(ClasspathEntry("lib", Jars.of(gone))),
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.METHOD,
        java.util.Optional.empty[Path](),
        KeepRule.standard(),
        java.util.Set.of(),
        filter
      )
    )

  test("a request's filter removes conflicts from the result and says how many") {
    val unfiltered = check(ConflictFilter.none)
    assertEquals(unfiltered.conflicts.size, 2)
    assertEquals(unfiltered.filtered.size, 0)
    val filtered = check(filter(ignoreDestination = List(selector("lib.api"))))
    assertEquals(filtered.conflicts.asScala.map(_.dependency.targetClass).toList, List("lib.impl.Impl"))
    assertEquals(filtered.filtered.asScala.toList, List(FilterEffect(FilterKind.IGNORE_DESTINATION_PACKAGES, 1)))
    assertEquals(filtered.conflictsBeforeFilters, 2)
    assertEquals(check(filter(targetSource = List(selector("app")))).conflicts.size, 2)
    assertEquals(check(filter(targetSource = List(selector("elsewhere")))).conflicts.size, 0)
  }

  test("the builder carries the filter, and a request has none by default") {
    assertEquals(LinkageRequest.builder().build().conflictFilter, ConflictFilter.none)
    val f = filter(ignoreSource = List(selector("app")))
    assertEquals(LinkageRequest.builder().withConflictFilter(f).build().conflictFilter, f)
  }
