package reachablelinkage.api

import java.nio.file.{Files, Path}
import javax.tools.ToolProvider
import scala.collection.JavaConverters._

/** The Scala 2.12 build of the core, called as an sbt 1 plugin calls it: through the Java facade, on a library whose method vanished. The Scala 3 suites cannot
  * run on 2.12, so this is what shows that the 2.12 artifact finds the same break.
  */
class CrossBuildSuite extends munit.FunSuite {

  private def compile(sources: Map[String, String], classpath: List[Path] = Nil): Path = {
    val work  = Files.createTempDirectory("cross-build")
    val out   = Files.createDirectory(work.resolve("out"))
    val files = sources.map { case (name, text) =>
      val file = work.resolve(name)
      Files.createDirectories(file.getParent)
      Files.write(file, text.getBytes("UTF-8"))
      file.toString
    }.toList
    val javac = ToolProvider.getSystemJavaCompiler
    val cp    = classpath.map(_.toString).mkString(java.io.File.pathSeparator)
    val args  = List("-d", out.toString, "-classpath", cp, "-nowarn") ++ files
    assertEquals(javac.run(null, null, null, args: _*), 0, "javac failed")
    out
  }

  test("the 2.12 build finds a method that is gone, and calls it REACHABLE") {
    val v1      = compile(Map("lib/Gone.java" -> "package lib; public class Gone { public static void x() {} }"))
    val v2      = compile(Map("lib/Gone.java" -> "package lib; public class Gone { }"))
    val app     = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Gone.x(); } }"), List(v1))
    val request = LinkageRequest(
      java.util.Collections.singletonList(app),
      java.util.Arrays.asList(ClasspathEntry("app", app), ClasspathEntry("lib", v2)),
      CheckScope.App,
      java.util.Collections.emptySet[String](),
      java.util.Collections.emptyList()
    )
    val result = Linkage.check(request)
    assertEquals(result.warnings.size, 0)
    val conflicts = result.conflicts.asScala.toList
    assertEquals(
      conflicts.map(c => (c.category, c.label, c.dependency.targetClass)),
      List((ConflictCategory.METHOD_SIGNATURE_NOT_FOUND, ConflictLabel.REACHABLE, "lib.Gone"))
    )
  }

  test("the same app against the library that still has the method has no conflict") {
    val v1      = compile(Map("lib/Gone.java" -> "package lib; public class Gone { public static void x() {} }"))
    val app     = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Gone.x(); } }"), List(v1))
    val request = LinkageRequest(
      java.util.Collections.singletonList(app),
      java.util.Arrays.asList(ClasspathEntry("app", app), ClasspathEntry("lib", v1)),
      CheckScope.App,
      java.util.Collections.emptySet[String](),
      java.util.Collections.emptyList()
    )
    assertEquals(Linkage.check(request).conflicts.size, 0)
  }
}
