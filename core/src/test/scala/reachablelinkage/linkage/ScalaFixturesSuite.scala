package reachablelinkage.linkage

import reachablelinkage.fixture.Scopes
import reachablelinkage.fixture.MissinglinkOracle.Key
import reachablelinkage.fixture.ScalaFixture.{compile, source}
import reachablelinkage.fixture.ScalaVersion.*

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.{MissinglinkOracle, OracleSuite, ScalaFixture, ScalaVersion}
import reachablelinkage.symbols.Symbols

/**
 * Scala-specific encodings, built with real scalac for 2.12, 2.13 and Scala 3 (LTS and current).
 * Each fixture builds a library v1, an app against it, and a v2 that breaks the app; both
 * missinglink-core and our checker must report the same break.
 */
class ScalaFixturesSuite extends OracleSuite:

  private def mnf(from: String, target: String, m: String) = Key("METHOD_SIGNATURE_NOT_FOUND", from, target, m)

  private final class Fixture(
      val name: String,
      val versions: Set[ScalaVersion],
      val expected: ScalaVersion => Set[Key],
      build: ScalaVersion => (ClassSource, Seq[ClassSource])
  ):
    private val cache = new java.util.concurrent.ConcurrentHashMap[ScalaVersion, (ClassSource, Seq[ClassSource])]()
    def built(v: ScalaVersion): (ClassSource, Seq[ClassSource]) = cache.computeIfAbsent(v, build(_))

  private val all: Set[ScalaVersion]    = ScalaVersion.values.toSet
  private val scala3: Set[ScalaVersion] = all.filter(_.isScala3)

  /** v1 library, app compiled against v1, v2 library that the app is checked against. */
  private def libraryBreak(lib1: String, lib2: String, app: String)(v: ScalaVersion): (ClassSource, Seq[ClassSource]) =
    val v1   = compile(v, Map("lib/Lib.scala" -> lib1))
    val v2   = compile(v, Map("lib/Lib.scala" -> lib2))
    val appD = compile(v, Map("app/Main.scala" -> app), Seq(v1))
    (source(appD), Seq(source(v2)))

  private val fixtures: Seq[Fixture] = Seq(

    Fixture("companion object method removed", all, _ => Set(mnf("app/Main$", "lib/Util$", "f")),
      libraryBreak(
        "package lib\nobject Util { def f(): Int = 1 }",
        "package lib\nobject Util { }",
        "package app\nobject Main { def run(): Int = lib.Util.f() }")),

    Fixture("trait method removed: the mixed-in forwarder in the app breaks", all,
      {
        // The mixin forwarder calls the static accessor `hello$` on every version. 2.x also calls the
        // trait initialiser `$init$` from the constructor, which an empty trait no longer has.
        case V3Lts | V3Next => Set(mnf("app/Impl", "lib/Greeter", "hello$"))
        case _ => Set(mnf("app/Impl", "lib/Greeter", "hello$"), mnf("app/Impl", "lib/Greeter", "$init$"))
      },
      libraryBreak(
        "package lib\ntrait Greeter { def hello(): String = \"hi\" }",
        "package lib\ntrait Greeter { }",
        "package app\nclass Impl extends lib.Greeter\nobject Main { def run(): String = new Impl().hello() }")),

    Fixture("lambda body calls a method that was removed", all, _ => Set(mnf("app/Main$", "lib/Lib$", "x")),
      libraryBreak(
        "package lib\nobject Lib { def run(f: () => Int): Int = f(); def x(): Int = 1 }",
        "package lib\nobject Lib { def run(f: () => Int): Int = f() }",
        "package app\nobject Main { def go(): Int = lib.Lib.run(() => lib.Lib.x()) }")),

    Fixture("case class gained a field: the call into apply/constructor breaks", all,
      {
        // scalac 2.x rewrites `P(1)` to `new P(1)`; Scala 3 keeps the call to the companion's apply.
        case V3Lts | V3Next => Set(mnf("app/Main$", "lib/P$", "apply"))
        case _              => Set(mnf("app/Main$", "lib/P", "<init>"))
      },
      libraryBreak(
        "package lib\ncase class P(a: Int)",
        "package lib\ncase class P(a: Int, b: Int)",
        "package app\nobject Main { def run(): lib.P = lib.P(1) }")),

    Fixture("Scala 3 top-level definition removed", scala3, _ => Set(mnf("app/Main$", "lib/Lib$package$", "helper")),
      libraryBreak(
        "package lib\ndef helper(): Int = 1",
        "package lib\ndef other(): Int = 1",
        "package app\nobject Main { def run(): Int = lib.helper() }")),

    // Value classes compile their methods into static `name$extension` methods on the companion.
    Fixture("value class method removed: the $extension call breaks", all,
      _ => Set(mnf("app/Main$", "lib/Meters$", "double$extension")),
      libraryBreak(
        "package lib\nclass Meters(val v: Int) extends AnyVal { def double: Int = v * 2 }",
        "package lib\nclass Meters(val v: Int) extends AnyVal { }",
        "package app\nobject Main { def run(): Int = new lib.Meters(2).double }")),

    // A clean build must stay clean: serializable lambdas ($deserializeLambda$, LambdaDeserialize),
    // lazy vals, traits, case classes, implicit value classes and for-comprehensions all compile to
    // encodings that must not look like missing members.
    Fixture("kitchen sink app against the Scala library: no conflicts", all, _ => Set.empty, v => {
      val app = compile(v, Map("app/Main.scala" ->
        """package app
          |import scala.collection.mutable
          |trait Shape { def area: Double; def describe: String = s"shape of area $area" }
          |final case class Circle(r: Double) extends Shape { def area: Double = math.Pi * r * r }
          |final case class Rect(w: Double, h: Double) extends Shape { def area: Double = w * h }
          |object Shape { def sum(xs: Seq[Shape]): Double = xs.map(_.area).sum }
          |class Counter { private var n = 0; lazy val expensive: Int = { n += 1; n * 42 }; def next(): Int = { n += 1; n } }
          |object Main {
          |  implicit class RichString(val s: String) extends AnyVal { def shout: String = s.toUpperCase + "!" }
          |  val f: Int => Int = x => x + 1
          |  def run(args: Array[String]): String = {
          |    val shapes = List(Circle(1.0), Rect(2.0, 3.0))
          |    val seen = mutable.Map.empty[String, Int]
          |    for { s <- shapes; k = s.describe } seen(k) = seen.getOrElse(k, 0) + 1
          |    val c = new Counter
          |    val first = shapes.collect { case Circle(r) => r }.headOption.map(r => f(r.toInt)).getOrElse(0)
          |    first.toString + c.expensive + "x".shout + Shape.sum(shapes) + args.mkString(",") + seen.size
          |  }
          |}""".stripMargin))
      (source(app), Nil)
    }),

    // The inline body is expanded into the app, so the break shows up in the app, not in the library.
    Fixture("Scala 3 inline expansion calls a member the library dropped", scala3,
      _ => Set(mnf("app/Main$", "lib/Lib$", "helper")),
      libraryBreak(
        "package lib\nobject Lib { inline def twice(x: Int): Int = helper(x) * 2; def helper(x: Int): Int = x }",
        "package lib\nobject Lib { inline def twice(x: Int): Int = x * 2 }",
        "package app\nobject Main { def run(): Int = lib.Lib.twice(3) }"))
  )

  for f <- fixtures; v <- ScalaVersion.values if f.versions.contains(v); scan <- Seq(false, true) do
    val label = s"${f.name} [Scala $v, ${if scan then "Full" else "App"}]"

    test(s"missinglink: $label") {
      val (app, libs) = f.built(v)
      assertEquals(MissinglinkOracle.check(app, libs, scan, ScalaFixture.librarySources(v)), f.expected(v))
    }

    test(s"ours: $label") {
      val (app, libs) = f.built(v)
      val provided    = ScalaFixture.librarySources(v)
      val syms        = new Symbols
      val index       = ClassIndex((app +: libs ++: provided :+ ClassSource.jdk()).toIndexedSeq, syms)
      val libIdx      = 1 to libs.length
      val provIdx     = (libs.length + 1) to (libs.length + provided.length)
      val scope = Scopes.of(index, Vector(0), if scan then Vector(0) ++ libIdx else Vector(0), Vector(0) ++ libIdx ++ provIdx)
      val got = Checker.check(index, scope).flatMap(MissinglinkOracle.normalise(_, syms)).toSet
      assertEquals(got, f.expected(v))
    }
