package reachablelinkage.classpath

import java.io.FileOutputStream
import java.nio.file.{Files, Path}
import java.util.jar.{Attributes, JarEntry, JarOutputStream, Manifest}
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.*
import reachablelinkage.classfile.ClassParser
import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.RuntimeOracle
import reachablelinkage.symbols.Symbols

/** A multi-release jar holds a class once for each Java release that needs a different version of it, and a JVM uses the one for its own release. The check
  * must see the classes the application will actually load.
  */
class MultiReleaseSuite extends munit.FunSuite:

  private def util(body: String): Path = compile(Map("lib/Util.java" -> s"package lib; public class Util { $body }"))
  private val extra                    = compile(Map("lib/Extra.java" -> "package lib; public class Extra { }"))

  /** A jar of the classes under `base` and, for each release, under `META-INF/versions/<release>/`. */
  private def jar(base: Path, versions: Map[Int, Path], multiRelease: String = "true"): Path =
    val file     = Files.createTempFile("mr", ".jar")
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    if multiRelease.nonEmpty then manifest.getMainAttributes.putValue("Multi-Release", multiRelease)
    val out                                   = new JarOutputStream(new FileOutputStream(file.toFile), manifest)
    def add(root: Path, prefix: String): Unit =
      val s = Files.walk(root)
      try
        s.iterator.asScala
          .filter(Files.isRegularFile(_))
          .foreach: p =>
            out.putNextEntry(new JarEntry(prefix + root.relativize(p).toString.replace('\\', '/')))
            out.write(Files.readAllBytes(p)); out.closeEntry()
      finally s.close()
    try { add(base, ""); versions.toList.sortBy(_._1).foreach((r, dir) => add(dir, s"META-INF/versions/$r/")) }
    finally out.close()
    file

  private def methods(source: ClassSource, name: String): List[String] =
    val syms = new Symbols
    source
      .read(name)
      .toList
      .flatMap: bytes =>
        ClassParser
          .parse(bytes, syms)
          .toOption
          .toList
          .flatMap(_.methods.toList.map(m => syms.name(m.name)))
          .filterNot(_ == "<init>")

  private def open(file: Path, release: Int, cache: Option[Path] = None): ClassSource =
    ClassSource.open(file, cache, release).fold(e => fail(e.message), identity)

  private val base = util("public static void old() { }")
  private val v11  = util("public static void eleven() { }")
  private val v17  = util("public static void seventeen() { }")

  test("below Java 9 the versioned entries are ignored") {
    val mr = jar(base, Map(11 -> v11))
    val s  = open(mr, 8)
    assertEquals(methods(s, "lib/Util"), List("old"))
  }

  test("a versions directory below 9 is not an overlay, whatever the release") {
    assertEquals(methods(open(jar(base, Map(8 -> v11, 7 -> v17)), 25), "lib/Util"), List("old"))
  }

  test("the highest versioned entry at or below the release is the class, else the base one") {
    val mr = jar(base, Map(11 -> v11, 17 -> v17))
    assertEquals(methods(open(mr, 9), "lib/Util"), List("old"), "nothing for 9 or 10")
    assertEquals(methods(open(mr, 11), "lib/Util"), List("eleven"))
    assertEquals(methods(open(mr, 15), "lib/Util"), List("eleven"), "17 is too new for 15")
    assertEquals(methods(open(mr, 17), "lib/Util"), List("seventeen"))
    assertEquals(methods(open(mr, 25), "lib/Util"), List("seventeen"))
  }

  test("a class that exists only under a version is there from that release, and not before") {
    val mr = jar(base, Map(11 -> extra))
    assert(!open(mr, 8).classNames.contains("lib/Extra"))
    assert(open(mr, 11).classNames.contains("lib/Extra"))
    assertEquals(open(mr, 17).classNames.toList.sorted, List("lib/Extra", "lib/Util"), "each class once, whatever its entries")
  }

  test("without Multi-Release: true in the manifest, the versioned entries are never used, as in the JVM") {
    assertEquals(methods(open(jar(base, Map(17 -> v17), multiRelease = ""), 25), "lib/Util"), List("old"))
    assertEquals(methods(open(jar(base, Map(17 -> v17), multiRelease = "false"), 25), "lib/Util"), List("old"))
    assertEquals(methods(open(jar(base, Map(17 -> v17), multiRelease = "TRUE"), 25), "lib/Util"), List("seventeen"), "the value is case-insensitive")
  }

  test("module-info and package-info, versioned or not, are not classes") {
    val extras = Files.createTempDirectory("extras")
    Files.createDirectories(extras.resolve("lib"))
    Files.write(extras.resolve("module-info.class"), Array[Byte](1, 2, 3))
    Files.write(extras.resolve("lib/package-info.class"), Array[Byte](1, 2, 3))
    assertEquals(open(jar(base, Map(11 -> extras)), 25).classNames.toList, List("lib/Util"))
    assertEquals(open(jar(extras, Map.empty), 25).classNames.toList, Nil)
  }

  test("a summary is made per release, so a cache shared by two releases never mixes them") {
    val mr           = jar(base, Map(17 -> v17))
    val cache        = Files.createTempDirectory("summaries")
    val fingerprints = List(8, 17).map(r => open(mr, r, Some(cache)).fingerprint)
    assertNotEquals(fingerprints.head, fingerprints.last)
    for release <- List(8, 17, 8, 17) do // twice each: a cold build, then a read of the saved summary
      val s    = open(mr, release, Some(cache))
      val syms = new Symbols
      val info = s.summarized("lib/Util", syms).nn
      assertEquals(
        info.methods.toList.map(m => syms.name(m.name)).filterNot(_ == "<init>"),
        List(if release == 8 then "old" else "seventeen"),
        s"release $release"
      )
    assertEquals(Files.list(cache).count, 2L, "one summary per release")
  }

  test("a jar that is not multi-release has the same fingerprint at every release (existing summaries stay valid)") {
    val plain = jar(base, Map.empty, multiRelease = "")
    assertEquals(open(plain, 8).fingerprint, open(plain, 25).fingerprint)
  }

  // ---- through the facade, on the JVM ----

  test("the check sees what the JVM will load: a method gone from the version for this release is a conflict, and not for an older one") {
    val mr                  = jar(base, Map(17 -> v17))
    val app                 = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Util.old(); } }"), Seq(base))
    def check(release: Int) =
      Linkage.check(
        LinkageRequest
          .builder()
          .withProjectClasses(java.util.List.of(app))
          .withClasspath(java.util.List.of(ClasspathEntry("mr.jar", mr)))
          .withJavaRelease(release)
          .build()
      )
    val newer = check(17)
    assertEquals(newer.conflicts.asScala.map(c => (c.category, c.label)).toList, List((ConflictCategory.METHOD_SIGNATURE_NOT_FOUND, ConflictLabel.REACHABLE)))
    assertEquals(check(11).conflicts.size, 0, "on Java 11 the base class is used")
    assertEquals(check(8).conflicts.size, 0)
    // The JVM this test runs on (Java 17 or later) really does fail, and the default release is its own.
    val error = RuntimeOracle.run(Seq(app, mr))
    assert(error.exists(_.isInstanceOf[NoSuchMethodError]), s"expected NoSuchMethodError, got $error")
    val byDefault =
      Linkage.check(LinkageRequest.builder().withProjectClasses(java.util.List.of(app)).withClasspath(java.util.List.of(ClasspathEntry("mr.jar", mr))).build())
    assertEquals(byDefault.conflicts.size, 1, "the default is the running JDK's release")
  }

  // ---- service files are not versioned ----

  test("a service file under META-INF/versions is ignored, by the JVM's ServiceLoader and by us: only the base file counts") {
    val classes =
      compile(Map("svc/Svc.java" -> "package svc; public interface Svc { }", "svc/Impl.java" -> "package svc; public class Impl implements Svc { }"))
    def jarWith(services: Map[String, String]): Path =
      val file     = Files.createTempFile("svc", ".jar")
      val manifest = new Manifest()
      manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      manifest.getMainAttributes.putValue("Multi-Release", "true")
      val out = new JarOutputStream(new FileOutputStream(file.toFile), manifest)
      try
        for name <- List("svc/Svc.class", "svc/Impl.class") do
          out.putNextEntry(new JarEntry(name)); out.write(Files.readAllBytes(classes.resolve(name))); out.closeEntry()
        services.foreach: (path, text) =>
          out.putNextEntry(new JarEntry(path)); out.write(text.getBytes("UTF-8")); out.closeEntry()
      finally out.close()
      file
    def providersTheJvmFinds(jar: Path): Int =
      val loader = new java.net.URLClassLoader(Array(jar.toUri.toURL), ClassLoader.getPlatformClassLoader)
      try java.util.ServiceLoader.load(loader.loadClass("svc.Svc"), loader).iterator.asScala.size
      finally loader.close()
    def serviceFilesWeSee(jar: Path): List[String] =
      val source = ClassSource.open(jar).fold(e => fail(e.message), identity)
      try source.serviceFiles.toList
      finally source.close()

    val base      = jarWith(Map("META-INF/services/svc.Svc" -> "svc.Impl\n"))
    val versioned = jarWith(Map(s"META-INF/versions/${Runtime.version.feature}/META-INF/services/svc.Svc" -> "svc.Impl\n"))
    assertEquals((providersTheJvmFinds(base), serviceFilesWeSee(base)), (1, List("META-INF/services/svc.Svc")))
    assertEquals((providersTheJvmFinds(versioned), serviceFilesWeSee(versioned)), (0, Nil))
  }
