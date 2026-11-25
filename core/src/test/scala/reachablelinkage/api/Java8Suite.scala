package reachablelinkage.api

import java.nio.file.{Files, Path, Paths}

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.GoneLibrary

/**
 * The core promises to load and run on Java 8 (it is compiled with `-release:8`, which checks the APIs it uses, but only a run shows that nothing
 * touches the module system there, which Java 8 does not have). The checker runs in a child JVM of Java 8, from a Java 8 caller, on a library
 * whose method vanished: it must find exactly that break, which also shows the JDK's own classes were read from `rt.jar`.
 *
 * Needs a Java 8: set `JAVA_8_HOME` (or `-Djava8.home=`). Without one the test is skipped, as the rest of the suite runs on newer JVMs.
 */
class Java8Suite extends munit.FunSuite:

  private val java8Home: Option[Path] =
    sys.env.get("JAVA_8_HOME").orElse(sys.props.get("java8.home")).map(Paths.get(_)).filter(home => Files.isRegularFile(home.resolve("bin").resolve("java")))

  // A Java 8 caller: it uses nothing newer than Java 8 and prints one line per conflict.
  private val caller = Map("rl/Check8.java" ->
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
      |}""".stripMargin)

  /** Where the checker and what it needs were loaded from: its classes, the two Scala libraries and ASM. */
  private def checkerClasspath: Seq[Path] =
    Seq[Class[?]](Linkage.getClass, classOf[scala.Product], Class.forName("scala.runtime.LazyVals$"), classOf[org.objectweb.asm.ClassReader])
      .map(c => Paths.get(c.getProtectionDomain.getCodeSource.getLocation.toURI))
      .distinct

  test("the checker runs on Java 8 and finds the one break that is there") {
    assume(java8Home.isDefined, "set JAVA_8_HOME to a Java 8 to run this test")
    val (v1, v2) = GoneLibrary.build(Map.empty)
    val app      = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Gone.x(); } }"), Seq(v1))
    val classes  = checkerClasspath
    val check    = compile(caller, classpath = classes, extraArgs = Seq("--release", "8"))

    val javaBin = java8Home.get.resolve("bin").resolve("java").toString
    val process = new ProcessBuilder(javaBin, "-cp", (check +: classes).mkString(java.io.File.pathSeparator), "rl.Check8", app.toString, v2.toString).redirectErrorStream(true).start()
    val output  = new String(process.getInputStream.readAllBytes()).linesIterator.toList
    process.waitFor()

    assert(output.contains("RELEASE 1.8"), s"not run on Java 8: $output")
    assert(output.contains("WARNINGS 0"), output.toString)
    // Exactly the broken call: nothing about the JDK (which would show if its classes were not found).
    assertEquals(output.filter(_.startsWith("CONFLICT")), List("CONFLICT METHOD_SIGNATURE_NOT_FOUND REACHABLE lib.Gone"))
  }
