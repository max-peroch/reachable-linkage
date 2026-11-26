package reachablelinkage.api

import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import reachablelinkage.fixture.{GoneLibrary, Jars}
import reachablelinkage.fixture.JavaFixture.compile

class LinkageApiSuite extends munit.FunSuite:

  private def request(app: Path, libs: Seq[(String, Path)], scope: CheckScope, excluded: Set[String] = Set.empty): LinkageRequest =
    LinkageRequest(
      java.util.List.of(app),
      libs.map((n, p) => ClasspathEntry(n, p)).asJava,
      scope,
      excluded.asJava,
      java.util.List.of()
    )

  private val libV1 = Map("lib/Foo.java" -> "package lib; public class Foo { public static void bar() {} public static int X = 1; }")

  test("a broken call is reported with class, method, line and the entries involved") {
    val v1  = compile(libV1)
    val v2  = compile(Map("lib/Foo.java" -> "package lib; public class Foo { }"))
    val app = compile(
      Map(
        "app/Main.java" ->
          """package app;
        |public class Main {
        |  public static void run() {
        |    int unrelated = 1;
        |    lib.Foo.bar();
        |  }
        |}""".stripMargin
      ),
      Seq(v1)
    )
    val result = Linkage.check(request(app, Seq("org:lib:2" -> v2), CheckScope.App))
    assertEquals(result.warnings.size, 0)
    val c = result.conflicts.asScala.toList
    assertEquals(c.size, 1)
    assertEquals(c.head.category, ConflictCategory.METHOD_SIGNATURE_NOT_FOUND)
    assertEquals(c.head.dependency.fromClass, "app.Main")
    assertEquals(c.head.dependency.fromMethodName, "run")
    assertEquals(c.head.dependency.fromLineNumber, 5)
    assertEquals(c.head.dependency.targetClass, "lib.Foo")
    assertEquals(c.head.dependency.targetMemberName, "bar")
    assertEquals(c.head.usedBy, "project")
    assertEquals(c.head.existsIn, "org:lib:2")
    assertEquals(c.head.reason, "Method not found: lib.Foo.bar()")
  }

  test("a boot classpath entry that cannot be opened is a warning, like any other entry") {
    val app      = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { new Object(); } }"))
    val missing  = java.nio.file.Paths.get("/no/such/rt.jar")
    val result   = Linkage.check(LinkageRequest.builder().withProjectClasses(java.util.List.of(app)).withBootClasspath(java.util.List.of(missing)).build())
    val warnings = result.warnings.asScala.toList
    assert(warnings.exists(_.contains("rt.jar")), warnings.toString)
    assertEquals(result.conflicts.size, 0, "it falls back to the running JDK, which has Object")
  }

  test("a field read in a static initialiser is located in <clinit>") {
    val v2  = compile(Map("lib/Foo.java" -> "package lib; public class Foo { }"))
    val app = compile(
      Map(
        "app/Main.java" ->
          """package app;
        |public class Main {
        |  static int x = lib.Foo.X;
        |}""".stripMargin
      ),
      Seq(compile(libV1))
    )
    val c = Linkage.check(request(app, Seq("lib" -> v2), CheckScope.App)).conflicts.asScala.toList
    assertEquals(c.map(_.category), List(ConflictCategory.FIELD_NOT_FOUND))
    assertEquals(c.head.dependency.fromMethodName, "<clinit>")
    assertEquals(c.head.dependency.fromLineNumber, 3)
  }

  test("a missing superclass is reported at class level, without a method or line") {
    val app = compile(
      Map("app/Main.java" -> "package app; public class Main extends lib.Gone {}"),
      Seq(compile(Map("lib/Gone.java" -> "package lib; public class Gone {}")))
    )
    val c = Linkage.check(request(app, Nil, CheckScope.App)).conflicts.asScala.filter(_.dependency.fromLineNumber == 0).toList
    assert(c.exists(x => x.category == ConflictCategory.CLASS_NOT_FOUND && x.dependency.targetClass == "lib.Gone"))
    assert(c.forall(_.existsIn == "<unknown>"))
  }

  // app -> liba.A -> libb.B, where libb lost the method; liba.Dead is never reached.
  private def transitive() =
    val b1 = compile(Map("libb/B.java" -> "package libb; public class B { public static void gone() {} }"))
    val b2 = compile(Map("libb/B.java" -> "package libb; public class B { }"))
    val a  = compile(
      Map(
        "liba/A.java"    -> "package liba; public class A { public static void go() { libb.B.gone(); } }",
        "liba/Dead.java" -> "package liba; public class Dead { public static void boom() { libb.B.gone(); } }"
      ),
      Seq(b1)
    )
    val app = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { liba.A.go(); } }"), Seq(a))
    (app, Seq("liba" -> a, "libb" -> b2))

  test("App scope checks the dependencies the app reaches, and attributes them to their entry") {
    val (app, libs) = transitive()
    val c           = Linkage.check(request(app, libs, CheckScope.App)).conflicts.asScala.toList
    assertEquals(c.map(x => (x.dependency.fromClass, x.usedBy, x.existsIn)), List(("liba.A", "liba", "libb")))
  }

  test("Full scope also checks classes nothing reaches") {
    val (app, libs) = transitive()
    val c           = Linkage.check(request(app, libs, CheckScope.Full)).conflicts.asScala.map(_.dependency.fromClass).toSet
    assertEquals(c, Set("liba.A", "liba.Dead"))
  }

  test("an excluded entry still resolves references but is not checked") {
    val (app, libs) = transitive()
    val c           = Linkage.check(request(app, libs, CheckScope.App, excluded = Set("liba"))).conflicts.asScala.toList
    assertEquals(c, Nil)
  }

  test("an entry that cannot be opened is a warning, not a failure") {
    val (app, libs) = transitive()
    val r           = Linkage.check(request(app, libs :+ ("ghost" -> java.nio.file.Paths.get("/no/such/file.jar")), CheckScope.App))
    assertEquals(r.warnings.size, 1)
    assertEquals(r.conflicts.size, 1)
  }

  // lib.Util.unused() calls a removed method but nothing the app reaches calls it.
  private def deadCode() =
    val gone1 = Map("lib/Gone.java" -> "package lib; public class Gone { public static void x() {} }")
    val util  = Map(
      "lib/Util.java" ->
        "package lib; public class Util { public static void used() {} public static void unused() { Gone.x(); } }"
    )
    val v1 = compile(gone1 ++ util)
    val v2 = compile(
      Map("lib/Gone.java" -> "package lib; public class Gone { }") ++ Map(
        "lib/Util.java" ->
          "package lib; public class Util { public static void used() {} public static void unused() { } }"
      )
    )
    // Reuse v1's Util bytecode (which calls Gone.x) against a Gone without x.
    val broken = java.nio.file.Files.createTempDirectory(v2.getParent, "broken")
    java.nio.file.Files.createDirectories(broken.resolve("lib"))
    java.nio.file.Files.copy(v1.resolve("lib/Util.class"), broken.resolve("lib/Util.class"))
    java.nio.file.Files.copy(v2.resolve("lib/Gone.class"), broken.resolve("lib/Gone.class"))
    val app = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.used(); } }"), Seq(v1))
    (app, broken)

  private def withPrecision(app: Path, lib: Path, p: CheckPrecision): LinkageResult =
    Linkage.check(
      LinkageRequest(java.util.List.of(app), java.util.List.of(ClasspathEntry("lib", lib)), CheckScope.App, java.util.Set.of(), java.util.List.of(), p)
    )

  test("Class precision labels every conflict REACHABLE") {
    val (app, lib) = deadCode()
    val c          = withPrecision(app, lib, CheckPrecision.CLASS).conflicts.asScala.toList
    assertEquals(c.map(x => (x.dependency.fromMethodName, x.label)), List(("unused", ConflictLabel.REACHABLE)))
  }

  test("Method precision labels a conflict in code nothing reaches LATENT, and failOn decides what fails") {
    val (app, lib) = deadCode()
    val r          = withPrecision(app, lib, CheckPrecision.METHOD)
    assertEquals(r.conflicts.asScala.map(x => (x.dependency.fromMethodName, x.label)).toList, List(("unused", ConflictLabel.LATENT)))
    assertEquals(r.failing(FailOn.REACHABLE).size, 0)
    assertEquals(r.failing(FailOn.ANY).size, 1)
  }

  test("Method precision keeps a conflict the app can reach REACHABLE") {
    val (app, libs) = transitive()
    val r           = Linkage.check(
      LinkageRequest(
        java.util.List.of(app),
        libs.map((n, p) => ClasspathEntry(n, p)).asJava,
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.METHOD
      )
    )
    assertEquals(r.conflicts.asScala.map(_.label).toList, List(ConflictLabel.REACHABLE))
    assertEquals(r.failing(FailOn.REACHABLE).size, 1)
  }

  private def missingProvider() =
    val (v1, v2) =
      GoneLibrary.build(Map("lib/Service.java" -> "package lib; public interface Service { }"), Map("META-INF/services/lib.Service" -> "lib.Nope\n"))
    val app = compile(
      Map(
        "app/Main.java" ->
          "package app; public class Main { public static Object run() { return lib.Service.class; } }"
      ),
      Seq(v1)
    )
    (app, v2)

  test("a provider listed in a service file but missing is reported against the entry that ships the file") {
    val (app, lib) = missingProvider()
    val c          = Linkage.check(request(app, Seq("org:lib:2" -> lib), CheckScope.App)).conflicts.asScala.toList
    assertEquals(c.size, 1)
    assertEquals(c.head.category, ConflictCategory.MISSING_PROVIDER)
    assertEquals(c.head.dependency.fromClass, "lib.Service")
    assertEquals(c.head.dependency.targetClass, "lib.Nope")
    assertEquals(c.head.dependency.fromLineNumber, 0)
    assertEquals(c.head.usedBy, "org:lib:2")
    assertEquals(c.head.existsIn, "<unknown>")
    assertEquals(c.head.label, ConflictLabel.REACHABLE)
    assertEquals(c.head.reason, "Provider lib.Nope is listed in META-INF/services/lib.Service but is not on the classpath")
  }

  test("a missing provider in an excluded entry is not reported") {
    val (app, lib) = missingProvider()
    val c          = Linkage.check(request(app, Seq("org:lib:2" -> lib), CheckScope.App, excluded = Set("org:lib:2"))).conflicts.asScala.toList
    assertEquals(c, Nil)
  }

  test("a probed class is labelled PROBED: reported, but only FailOn.ANY counts it") {
    val (v1, v2) = GoneLibrary.build(
      Map(
        "lib/Opt.java" -> "package lib; public class Opt { public static boolean has() { try { Class.forName(\"lib.Vanished\"); return true; } catch (ClassNotFoundException e) { return false; } } }",
        "lib/Vanished.java" -> "package lib; public class Vanished { public static void y() {} }"
      ),
      removed = Set("lib/Vanished")
    )
    val app = compile(
      Map(
        "app/Main.java" ->
          "package app; public class Main { public static void run() { if (lib.Opt.has()) { lib.Vanished.y(); } } }"
      ),
      Seq(v1)
    )
    val r = withPrecision(app, v2, CheckPrecision.METHOD)
    assertEquals(r.conflicts.asScala.map(c => (c.category, c.label)).toList, List((ConflictCategory.CLASS_NOT_FOUND, ConflictLabel.PROBED)))
    assertEquals(r.failing(FailOn.REACHABLE).size, 0)
    assertEquals(r.failing(FailOn.ANY).size, 1)
  }

  test("a callee of a guarded call is GUARDED at Method precision, and REACHABLE at Class precision") {
    val (v1, v2) = GoneLibrary.build(
      Map(
        "lib/Util.java"     -> "package lib; public class Util { public static void risky() { Vanished.y(); } }",
        "lib/Vanished.java" -> "package lib; public class Vanished { public static void y() {} }"
      ),
      removed = Set("lib/Vanished")
    )
    val app = compile(
      Map(
        "app/Main.java" ->
          "package app; public class Main { public static void run() { try { lib.Util.risky(); } catch (NoClassDefFoundError e) { } } }"
      ),
      Seq(v1)
    )
    assertEquals(withPrecision(app, v2, CheckPrecision.METHOD).conflicts.asScala.map(_.label).toList, List(ConflictLabel.GUARDED))
    assertEquals(withPrecision(app, v2, CheckPrecision.CLASS).conflicts.asScala.map(_.label).toList, List(ConflictLabel.REACHABLE))
  }

  test("a cache directory keeps the JDK listing between runs and does not change the answer") {
    val (app, libs)                          = transitive()
    val dir                                  = java.nio.file.Files.createTempDirectory("facade-cache")
    def run(cache: java.util.Optional[Path]) = Linkage.check(
      LinkageRequest(
        java.util.List.of(app),
        libs.map((n, p) => ClasspathEntry(n, p)).asJava,
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.CLASS,
        cache
      )
    )
    val without = run(java.util.Optional.empty())
    val cold    = run(java.util.Optional.of(dir))
    val warm    = run(java.util.Optional.of(dir))
    assertEquals(cold.conflicts, without.conflicts)
    assertEquals(warm.conflicts, without.conflicts)
    val files = java.nio.file.Files.list(dir)
    try assertEquals(files.iterator.asScala.map(_.getFileName.toString.endsWith(".listing")).toList, List(true))
    finally files.close()
  }

  test("a cache directory also keeps a summary per classpath jar, and the answer does not change") {
    val (app, libs)                          = transitive()
    val jars                                 = libs.map((n, p) => (n, Jars.of(p)))
    val dir                                  = java.nio.file.Files.createTempDirectory("facade-jar-cache")
    def run(cache: java.util.Optional[Path]) = Linkage.check(
      LinkageRequest(
        java.util.List.of(app),
        jars.map((n, p) => ClasspathEntry(n, p)).asJava,
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.CLASS,
        cache
      )
    )
    val without = run(java.util.Optional.empty())
    val cold    = run(java.util.Optional.of(dir))
    val warm    = run(java.util.Optional.of(dir))
    assertEquals(without.conflicts.size, 1)
    assertEquals(cold.conflicts, without.conflicts)
    assertEquals(warm.conflicts, without.conflicts)
    val files = java.nio.file.Files.list(dir)
    try
      val names = files.iterator.asScala.map(_.getFileName.toString).toList
      assertEquals(names.count(_.endsWith(".summary")), jars.size, names.toString)
      assertEquals(names.count(_.endsWith(".listing")), 1)
    finally files.close()
  }

  test("an entry that cannot be opened is a warning, and the check still runs") {
    val v1     = compile(libV1)
    val app    = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Foo.bar(); } }"), Seq(v1))
    val gone   = java.nio.file.Files.createTempDirectory("missing").resolve("gone.jar")
    val result = Linkage.check(request(app, Seq("lib" -> Jars.of(v1), "gone" -> gone), CheckScope.App))
    assertEquals(result.conflicts.size, 0)
    assertEquals(result.warnings.size, 1)
    assert(result.warnings.get(0).contains("not found"), result.warnings)
  }

  // ---- the default precision is METHOD ----

  private def utilLibs() =
    GoneLibrary.build(Map("lib/Util.java" -> "package lib; public class Util { public static void used() { } public static void unused() { Gone.x(); } }"))

  test("a request that does not name a precision uses METHOD, and the five-argument form too") {
    val (v1, v2) = utilLibs()
    val app      = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.used(); } }"), Seq(v1))
    val libs     = java.util.List.of(ClasspathEntry("lib", Jars.of(v2)))
    val five     = LinkageRequest(java.util.List.of(app), libs, CheckScope.App, java.util.Set.of(), java.util.List.of())
    assertEquals(five.precision, CheckPrecision.METHOD)
    val six = new LinkageRequest(java.util.List.of(app), libs, CheckScope.App, java.util.Set.of(), java.util.List.of(), CheckPrecision.CLASS)
    assertEquals(six.precision, CheckPrecision.CLASS, "naming a precision still wins")
  }

  test("by default a conflict nothing reachable runs is LATENT and does not fail the build; CLASS reports it as reachable") {
    val (v1, v2)  = utilLibs()
    val app       = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.used(); } }"), Seq(v1))
    val libs      = java.util.List.of(ClasspathEntry("lib", Jars.of(v2)))
    val byDefault = Linkage.check(LinkageRequest(java.util.List.of(app), libs, CheckScope.App, java.util.Set.of(), java.util.List.of()))
    assertEquals(byDefault.conflicts.asScala.map(_.label).toList, List(ConflictLabel.LATENT))
    assertEquals(byDefault.failing().size, 0, "the default fails on REACHABLE only")
    assertEquals(byDefault.failing(FailOn.ANY).size, 1)
    val classLevel = Linkage.check(LinkageRequest(java.util.List.of(app), libs, CheckScope.App, java.util.Set.of(), java.util.List.of(), CheckPrecision.CLASS))
    assertEquals(classLevel.conflicts.asScala.map(_.label).toList, List(ConflictLabel.REACHABLE))
    assertEquals(classLevel.failing().size, 1)
  }

  test("failing() is failing(REACHABLE)") {
    val (v1, v2)       = utilLibs()
    val callsBroken    = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.unused(); } }"), Seq(v1))
    val libs           = java.util.List.of(ClasspathEntry("lib", Jars.of(v2)))
    def run(app: Path) = Linkage.check(LinkageRequest(java.util.List.of(app), libs, CheckScope.App, java.util.Set.of(), java.util.List.of()))
    val broken         = run(callsBroken)
    assertEquals(broken.failing(), broken.failing(FailOn.REACHABLE))
    assertEquals(broken.failing().size, 1)
  }

  // ---- the builder ----

  test("a builder with nothing set is a request with every default") {
    val built = LinkageRequest.builder().build()
    assertEquals(built.precision, CheckPrecision.METHOD)
    assertEquals(built.scope, CheckScope.App)
    assertEquals(built.keepRules, KeepRule.standard())
    assertEquals(built.loadedClasses, java.util.Set.of[String]())
    assertEquals(built.cacheDirectory, java.util.Optional.empty[Path]())
    assertEquals(built.projectClasses, java.util.List.of[Path]())
  }

  test("each setter changes its field and nothing else, and leaves the builder it was called on alone") {
    val base  = LinkageRequest.builder()
    val dir   = java.nio.file.Files.createTempDirectory("cache")
    val rules = java.util.List.of(KeepRule.of(KeepRuleKind.KEEP_MEMBERS, List("a.B")).fold(p => fail(p.message), identity))
    assertEquals(base.withPrecision(CheckPrecision.METHOD_RTA).build(), LinkageRequest.builder().build().copy(precision = CheckPrecision.METHOD_RTA))
    assertEquals(base.withScope(CheckScope.Full).build().scope, CheckScope.Full)
    assertEquals(base.withCacheDirectory(dir).build().cacheDirectory, java.util.Optional.of(dir))
    assertEquals(base.withKeepRules(rules).build().keepRules, rules)
    assertEquals(base.withLoadedClasses(java.util.Set.of("x.Y")).build().loadedClasses, java.util.Set.of("x.Y"))
    assertEquals(base.withExcludedEntries(java.util.Set.of("e")).build().excludedEntries, java.util.Set.of("e"))
    assertEquals(base.build().precision, CheckPrecision.METHOD, "the original builder is unchanged")
    assertEquals(base.build(), LinkageRequest.builder().build())
  }

  test("a request from the builder runs the same check as one from the constructor") {
    val (v1, v2) =
      GoneLibrary.build(Map("lib/Util.java" -> "package lib; public class Util { public static void used() { } public static void unused() { Gone.x(); } }"))
    val app            = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.unused(); } }"), Seq(v1))
    val libs           = java.util.List.of(ClasspathEntry("lib", Jars.of(v2)))
    val viaBuilder     = LinkageRequest.builder().withProjectClasses(java.util.List.of(app)).withClasspath(libs).build()
    val viaConstructor = LinkageRequest(java.util.List.of(app), libs, CheckScope.App, java.util.Set.of(), java.util.List.of())
    assertEquals(viaBuilder, viaConstructor)
    assertEquals(Linkage.check(viaBuilder).conflicts, Linkage.check(viaConstructor).conflicts)
  }
