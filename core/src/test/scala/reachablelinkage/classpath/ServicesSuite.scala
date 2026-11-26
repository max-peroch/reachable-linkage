package reachablelinkage.classpath

import java.nio.file.{Files, Path}
import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry

import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.symbols.Symbols

class ServicesSuite extends munit.FunSuite:

  test("a service file lists providers with ServiceLoader's rules") {
    val text = "# providers\n\n  a.b.One  \na.b.Two # the second\r\n   # only a comment\n\ta.b.Three\n"
    assertEquals(Services.parse(text), Vector("a.b.One", "a.b.Two", "a.b.Three"))
    assertEquals(Services.parse(""), Vector.empty[String])
  }

  private def writeJar(entries: (String, String)*): Path =
    val jar = Files.createTempFile("services", ".jar")
    val out = new JarOutputStream(Files.newOutputStream(jar))
    try
      entries.foreach: (name, content) =>
        out.putNextEntry(new ZipEntry(name)); out.write(content.getBytes("UTF-8")); out.closeEntry()
    finally out.close()
    jar

  test("a jar lists its service files, skips directory entries, and reads them") {
    val jar = writeJar("META-INF/services/" -> "", "META-INF/services/lib.Service" -> "lib.Provider\n", "META-INF/MANIFEST.MF" -> "x", "a/B.txt" -> "y")
    val src = ClassSource.open(jar).toOption.get
    try
      assertEquals(src.serviceFiles.toList, List("META-INF/services/lib.Service"))
      assertEquals(src.readResource("META-INF/services/lib.Service").map(new String(_, "UTF-8")), Some("lib.Provider\n"))
      assertEquals(src.readResource("nope"), None)
    finally src.close()
  }

  test("a directory lists its service files and reads them") {
    val dir = Files.createTempDirectory("services-dir")
    Files.createDirectories(dir.resolve("META-INF/services"))
    Files.writeString(dir.resolve("META-INF/services/lib.Service"), "lib.Provider\n")
    val src = ClassSource.open(dir).toOption.get
    assertEquals(src.serviceFiles.toList, List("META-INF/services/lib.Service"))
    assertEquals(src.readResource("META-INF/services/lib.Service").map(new String(_, "UTF-8")), Some("lib.Provider\n"))
    assertEquals(ClassSource.open(Files.createTempDirectory("empty")).toOption.get.serviceFiles.toList, Nil)
  }

  test("providers are read from the classpath; one that is not there is recorded as missing") {
    val dir = compile(
      Map("lib/Service.java" -> "package lib; public interface Service { }", "lib/Provider.java" -> "package lib; public class Provider implements Service { }")
    )
    Files.createDirectories(dir.resolve("META-INF/services"))
    Files.writeString(dir.resolve("META-INF/services/lib.Service"), "lib.Provider\nlib.Nope\n")
    val syms      = new Symbols
    val index     = ClassIndex(Vector(source(dir), ClassSource.jdk()), syms)
    val providers = index.services.providersOf(syms.classId("lib/Service")).map(syms.className).toList
    assertEquals(providers, List("lib/Provider"))
    assertEquals(index.services.missing.map(m => (syms.className(m.service), syms.className(m.provider), m.entry)), Vector(("lib/Service", "lib/Nope", 0)))
    assertEquals(index.services.interfaces.map(syms.className).toList, List("lib/Service"))
    assertEquals(index.services.providersOf(syms.classId("lib/Other")).length, 0)
  }
