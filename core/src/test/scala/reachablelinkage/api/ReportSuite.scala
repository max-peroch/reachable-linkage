package reachablelinkage.api

import scala.jdk.CollectionConverters.*

class ReportSuite extends munit.FunSuite:

  private def conflict(
      from: String,
      method: String,
      target: String,
      member: String = "",
      label: ConflictLabel = ConflictLabel.REACHABLE,
      category: ConflictCategory = ConflictCategory.CLASS_NOT_FOUND,
      line: Int = 10,
      usedBy: String = "app.jar",
      existsIn: String = "<unknown>"
  ): LinkageConflict =
    LinkageConflict(category, Dependency(from, method, "()V", line, target, member, ""), usedBy, existsIn, s"Class not found: $target", label)

  private def render(options: ReportOptions, cs: LinkageConflict*): List[String] = Report.render(cs.asJava, options)

  test("no conflicts give an empty report") {
    assertEquals(render(ReportOptions()), Nil)
  }

  test("groups that tie on label, size, target and member are ordered by category, not by whatever order a hash gives") {
    val all   = ConflictCategory.values().toList
    val lines = render(ReportOptions(), all.reverse.map(c => conflict("a.A", "m", "x.Gone", member = "go", category = c))*)
    val order = lines.filter(_.startsWith("[")).map(_.split(" ")(1))
    assertEquals(order, all.map(_.name))
  }

  test("a summary counts conflicts and groups by label, worst first") {
    val lines = render(
      ReportOptions(),
      conflict("a.A", "m", "x.Gone"),
      conflict("a.B", "m", "x.Gone", label = ConflictLabel.LATENT),
      conflict("a.C", "m", "y.Other", label = ConflictLabel.GUARDED)
    )
    assertEquals(lines.head, "3 conflicts in 2 groups: REACHABLE 1, GUARDED 1, LATENT 1")
  }

  test("one group per target, however many places use it, with the count") {
    val lines = render(ReportOptions(), conflict("a.A", "m", "x.Gone"), conflict("a.B", "m", "x.Gone"), conflict("a.C", "m", "x.Gone"))
    assertEquals(lines.count(_.startsWith("[")), 1)
    assert(lines.exists(l => l.startsWith("[REACHABLE] CLASS_NOT_FOUND x.Gone") && l.endsWith("3 uses")), lines.toString)
    assertEquals(lines.count(_.startsWith("    a.")), 3)
  }

  test("a group with mixed labels shows its worst, and the mix") {
    val lines = render(ReportOptions(), conflict("a.A", "m", "x.Gone", label = ConflictLabel.LATENT), conflict("a.B", "m", "x.Gone"))
    assert(lines(1).startsWith("[REACHABLE]") && lines(1).contains("(REACHABLE 1, LATENT 1)"), lines(1))
    assert(lines(2).contains("a.B#m"), "the more likely place comes first: " + lines.mkString("\n"))
  }

  test("groups come worst first, then the most places, then by name") {
    val lines = render(
      ReportOptions(),
      conflict("a.A", "m", "l.Latent", label = ConflictLabel.LATENT),
      conflict("a.A", "m", "r.OnePlace"),
      conflict("a.A", "m", "r.TwoPlaces"),
      conflict("a.B", "m", "r.TwoPlaces")
    )
    assertEquals(lines.filter(_.startsWith("[")).map(_.split(" ")(2).stripSuffix(":")), List("r.TwoPlaces", "r.OnePlace", "l.Latent"))
  }

  test("the number of groups and of places per group is bounded, and what is left out is counted") {
    val many  = (1 to 10).map(i => conflict(s"a.C$i", "m", "x.Gone"))
    val one   = (1 to 5).map(i => conflict("a.A", "m", s"t.T$i"))
    val lines = render(ReportOptions(maxGroups = 3, maxSitesPerGroup = 2), (many ++ one)*)
    assertEquals(lines.count(_.startsWith("[")), 3)
    assert(lines.exists(_.contains("... and 8 more places")), lines.mkString("\n"))
    assert(lines.last == "... and 3 more groups (3 conflicts)", lines.last)
    // summary, the big group (header, two places, a rest line), two single-place groups (header and place each), the trailer
    assertEquals(lines.size, 1 + (1 + 2 + 1) + 2 + 2 + 1)
  }

  test("a method that breaks at several lines is one place") {
    val lines = render(ReportOptions(), conflict("a.A", "m", "x.Gone", line = 5), conflict("a.A", "m", "x.Gone", line = 9))
    assertEquals(lines.count(_.startsWith("    a.A")), 1)
    assert(lines(1).endsWith("2 uses"), "both are counted as uses")
  }

  test("Scala's generated names are shown as source names, unless asked not to") {
    val c     = conflict("app.Foo$", "$anonfun$bar$1", "lib.Gone$", member = "$plus", line = 4)
    val shown = render(ReportOptions(), c)
    assert(shown.exists(_.contains("lib.Gone$") == false) && shown.exists(_.contains("object lib.Gone")), shown.mkString("\n"))
    assert(shown.exists(_.contains("object app.Foo#anonymous function in bar:4")), shown.mkString("\n"))
    val raw = render(ReportOptions(demangle = false), c)
    assert(raw.exists(_.contains("app.Foo$#$anonfun$bar$1:4")), raw.mkString("\n"))
  }

  test("a reference from a Scala 3 inline expansion names the library it was inlined from") {
    val c = conflict("app.Main", "run", "lib.Util$", member = "inline$helper", existsIn = "lib-1.0.jar", category = ConflictCategory.METHOD_SIGNATURE_NOT_FOUND)
    assert(render(ReportOptions(), c).exists(_.contains("[inlined from lib-1.0.jar]")), render(ReportOptions(), c).mkString("\n"))
  }

  test("a conflict that is not at an instruction (a missing superclass) has no method or line") {
    val lines = render(ReportOptions(), conflict("a.Sub", "", "x.Super", line = 0))
    assert(lines(2).trim.startsWith("a.Sub  [") && !lines(2).contains("#") && !lines(2).contains(":"), lines(2))
  }

  test("the same conflicts give the same report") {
    val cs = (1 to 20).map(i => conflict(s"a.C$i", "m", s"x.T${i % 4}"))
    assertEquals(render(ReportOptions(), cs*), render(ReportOptions(), cs.reverse*))
  }

  test("two kinds of break on the same target are two groups") {
    val lines = render(ReportOptions(), conflict("a.A", "m", "x.T"), conflict("a.B", "m", "x.T", category = ConflictCategory.INACCESSIBLE_CLASS))
    assertEquals(lines.count(_.startsWith("[")), 2)
    assert(lines.head.contains("in 2 groups"), lines.head)
  }
