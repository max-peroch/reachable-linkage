package reachablelinkage.api

import java.nio.file.{Files, Path, Paths}

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.GoneLibrary

/** The core promises to load and run on Java 8 and later (it is compiled with `-release:8`, which checks the APIs it uses, but only a run shows that nothing
  * touches the module system, which Java 8 does not have, or a JDK class file the reader cannot parse). The checker runs in a child JVM of the Java at
  * `CHECK_JAVA_HOME` (or `-Dcheck.java.home=`), from a Java 8 caller, on a library whose method vanished: it must find exactly that break, which also shows the
  * JDK's own classes were read (from `rt.jar` on 8, the module image later). If `CHECK_JAVA_RELEASE` is set (`8`, `11`, ...), the child must be that release.
  *
  * Without a Java home the test is skipped, as the rest of the suite runs on the JVM that runs sbt.
  */
class OtherJavaSuite extends munit.FunSuite:

  private val javaHome: Option[Path] =
    sys.env
      .get("CHECK_JAVA_HOME")
      .orElse(sys.props.get("check.java.home"))
      .map(Paths.get(_))
      .filter(home => Files.isRegularFile(home.resolve("bin").resolve("java")))

  // The release `java.specification.version` reports: `1.8` for Java 8, the number from 9 on.
  private def specificationVersion(release: String): String = if release == "8" then "1.8" else release

  // A Java 8 caller: it uses nothing newer than Java 8 and prints one line per conflict.
  private val caller = Map(
    "rl/Check8.java" ->
      """package rl;
      |import java.nio.file.*;
      |import java.util.*;
      |import reachablelinkage.api.*;
      |public class Check8 {
      |  public static void main(String[] args) {
      |    LinkageRequest request = LinkageRequest.builder()
      |        .withProjectClasses(Collections.singletonList(Paths.get(args[0])))
      |        .withClasspath(Arrays.asList(new ClasspathEntry("app", Paths.get(args[0])), new ClasspathEntry("lib", Paths.get(args[1]))))
      |        .withScope(CheckScope.App)
      |        .withPrecision(CheckPrecision.METHOD)
      |        .build();
      |    LinkageResult result = Linkage.check(request);
      |    System.out.println("RELEASE " + System.getProperty("java.specification.version"));
      |    System.out.println("WARNINGS " + result.warnings().size());
      |    for (LinkageConflict c : result.conflicts()) System.out.println("CONFLICT " + c.category() + " " + c.label() + " " + c.dependency().targetClass());
      |  }
      |}""".stripMargin
  )

  /** Where the checker and what it needs were loaded from: its classes, the two Scala libraries and ASM. */
  private def checkerClasspath: Seq[Path] =
    Seq[Class[?]](Linkage.getClass, classOf[scala.Product], Class.forName("scala.runtime.LazyVals$"), classOf[org.objectweb.asm.ClassReader])
      .map(c => Paths.get(c.getProtectionDomain.getCodeSource.getLocation.toURI))
      .distinct

  test("the checker runs on the other Java and finds the one break that is there") {
    assume(javaHome.isDefined, "set CHECK_JAVA_HOME to a Java home to run this test")
    val (v1, v2) = GoneLibrary.build(Map.empty)
    val app      = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Gone.x(); } }"), Seq(v1))
    val classes  = checkerClasspath
    val check    = compile(caller, classpath = classes, extraArgs = Seq("--release", "8"))

    val javaBin = javaHome.get.resolve("bin").resolve("java").toString
    val process = new ProcessBuilder(javaBin, "-cp", (check +: classes).mkString(java.io.File.pathSeparator), "rl.Check8", app.toString, v2.toString)
      .redirectErrorStream(true)
      .start()
    val output = new String(process.getInputStream.readAllBytes()).linesIterator.toList
    process.waitFor()

    sys.env
      .get("CHECK_JAVA_RELEASE")
      .foreach(release => assert(output.contains(s"RELEASE ${specificationVersion(release)}"), s"not run on Java $release: $output"))
    assert(output.contains("WARNINGS 0"), output.toString)
    // Exactly the broken call: nothing about the JDK (which would show if its classes were not found).
    assertEquals(output.filter(_.startsWith("CONFLICT")), List("CONFLICT METHOD_SIGNATURE_NOT_FOUND REACHABLE lib.Gone"))
  }
