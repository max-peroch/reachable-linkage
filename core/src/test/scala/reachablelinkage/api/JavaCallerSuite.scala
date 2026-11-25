package reachablelinkage.api

import java.net.URLClassLoader
import java.nio.file.{Path, Paths}
import reachablelinkage.fixture.{GoneLibrary, JavaFixture, ScalaFixture, ScalaVersion}

/**
 * The facade exists so Java and Scala 2.12 build plugins (sbt-missinglink, a Gradle plugin) can call
 * it. This compiles a plain Java caller with javac and runs it, to prove the API works from Java:
 * enums switch, accessors have missinglink's names, and no Scala type leaks into signatures it uses.
 */
class JavaCallerSuite extends munit.FunSuite:

  private def location(c: Class[?]): Path = Paths.get(c.getProtectionDomain.getCodeSource.getLocation.toURI)

  test("a Java caller can build a request, run the check and read every field of the result") {
    val libV1 = JavaFixture.compile(Map("lib/Foo.java" -> "package lib; public class Foo { public static void bar() {} }"))
    val libV2 = JavaFixture.compile(Map("lib/Foo.java" -> "package lib; public class Foo { }"))
    val app   = JavaFixture.compile(Map("app/Main.java" ->
      "package app; public class Main { public static void run() { lib.Foo.bar(); } }"), Seq(libV1))

    val ours  = Seq(location(classOf[Linkage.type]), location(classOf[org.objectweb.asm.ClassReader])) ++
      ScalaFixture.library(ScalaVersion.V3Next)
    val caller = JavaFixture.compile(Map("plug/Use.java" ->
      """package plug;
        |import java.nio.file.Path;
        |import java.util.List;
        |import java.util.Set;
        |import reachablelinkage.api.*;
        |
        |public class Use {
        |  public static String run(Path app, Path lib) {
        |    LinkageRequest req = new LinkageRequest(
        |        List.of(app), List.of(new ClasspathEntry("org:lib:2", lib)), CheckScope.App, Set.of(), List.of());
        |    LinkageResult res = Linkage.check(req);
        |    StringBuilder sb = new StringBuilder();
        |    for (LinkageConflict c : res.conflicts()) {
        |      switch (c.category()) {
        |        case METHOD_SIGNATURE_NOT_FOUND: sb.append("method;"); break;
        |        case CLASS_NOT_FOUND:             sb.append("class;"); break;
        |        default:                         sb.append("other;");
        |      }
        |      Dependency d = c.dependency();
        |      sb.append(d.fromClass()).append('#').append(d.fromMethodName()).append(':').append(d.fromLineNumber())
        |        .append("->").append(d.targetClass()).append('.').append(d.targetMemberName())
        |        .append(" usedBy=").append(c.usedBy()).append(" existsIn=").append(c.existsIn())
        |        .append(" ").append(c.reason());
        |    }
        |    return sb.toString();
        |  }
        |}""".stripMargin), ours)

    val loader = new URLClassLoader((caller +: ours).map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
    try
      val out = loader.loadClass("plug.Use").getMethod("run", classOf[Path], classOf[Path]).invoke(null, app, libV2)
      assertEquals(
        out,
        "method;app.Main#run:1->lib.Foo.bar usedBy=project existsIn=org:lib:2 Method not found: lib.Foo.bar()"
      )
    finally loader.close()
  }

  test("a Java caller can use the builder, set the precision and read the default failing conflicts") {
    val (lib1, lib2) = GoneLibrary.build(Map("lib/Util.java" -> "package lib; public class Util { public static void used() {} public static void unused() { Gone.x(); } }"))
    val app = JavaFixture.compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.used(); } }"), Seq(lib1))
    val ours = Seq(location(classOf[Linkage.type]), location(classOf[org.objectweb.asm.ClassReader])) ++ ScalaFixture.library(ScalaVersion.V3Next)
    val caller = JavaFixture.compile(Map("plug/Build.java" ->
      """package plug;
        |import java.nio.file.Path;
        |import java.util.List;
        |import reachablelinkage.api.*;
        |
        |public class Build {
        |  public static String run(Path app, Path lib) {
        |    LinkageRequestBuilder base = LinkageRequest.builder()
        |        .withProjectClasses(List.of(app))
        |        .withClasspath(List.of(new ClasspathEntry("org:lib:2", lib)));
        |    LinkageResult byDefault = Linkage.check(base.build());
        |    LinkageResult classLevel = Linkage.check(base.withPrecision(CheckPrecision.CLASS).build());
        |    return byDefault.conflicts().get(0).label() + "/" + byDefault.failing().size()
        |        + " " + classLevel.conflicts().get(0).label() + "/" + classLevel.failing().size();
        |  }
        |}""".stripMargin), ours)
    val loader = new URLClassLoader((caller +: ours).map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
    try
      val out = loader.loadClass("plug.Build").getMethod("run", classOf[Path], classOf[Path]).invoke(null, app, lib2)
      assertEquals(out, "LATENT/0 REACHABLE/1")
    finally loader.close()
  }

