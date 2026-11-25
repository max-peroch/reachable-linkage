package reachablelinkage.classpath

import java.io.FileOutputStream
import java.nio.file.{Files, Path}
import java.util.BitSet
import java.util.jar.{Attributes, JarEntry, JarOutputStream, Manifest}
import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.symbols.Symbols

/**
 * The classes a configuration file names are loaded by the framework that reads it. A word in a wanted file that is a class of the classpath
 * counts; nothing else does (a word that is not a class, a class of the JDK, a file with another name).
 */
class NamedInResourcesSuite extends munit.FunSuite:

  private val classes = compile(Map(
    "p/A.java" -> "package p; public class A { }",
    "p/B.java" -> "package p; public class B { public static class Inner { } }",
    "p/C.java" -> "package p; public class C { }"))

  /** The class files of `classes` plus the given resources, as a directory and as a jar. */
  private def directoryWith(files: Map[String, String]): Path =
    val dir = Files.createTempDirectory("res")
    val walk = Files.walk(classes)
    try walk.iterator.asScala.filter(Files.isRegularFile(_)).foreach: p =>
      val target = dir.resolve(classes.relativize(p).toString)
      Files.createDirectories(target.getParent); Files.copy(p, target)
    finally walk.close()
    files.foreach((path, text) => { val f = dir.resolve(path); Files.createDirectories(f.getParent); Files.writeString(f, text) })
    dir

  private def jarWith(files: Map[String, String]): Path =
    val dir  = directoryWith(Map.empty)
    val file = Files.createTempFile("res", ".jar")
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    val out = new JarOutputStream(new FileOutputStream(file.toFile), manifest)
    try
      val walk = Files.walk(dir)
      try walk.iterator.asScala.filter(Files.isRegularFile(_)).foreach: p =>
        out.putNextEntry(new JarEntry(dir.relativize(p).toString.replace('\\', '/'))); out.write(Files.readAllBytes(p)); out.closeEntry()
      finally walk.close()
      files.foreach((path, text) => { out.putNextEntry(new JarEntry(path)); out.write(text.getBytes("UTF-8")); out.closeEntry() })
    finally out.close()
    file

  private def open(p: Path): ClassSource = ClassSource.open(p).fold(e => fail(e.message), identity)

  /** The names of the classes the files name, by internal name. */
  private def namedAll(sources: Seq[ClassSource], wanted: Set[String], entries: Seq[Int]): Vector[String] =
    val syms  = new Symbols
    val index = ClassIndex((sources :+ ClassSource.jdk()).toVector, syms)
    val bits  = new BitSet; entries.foreach(bits.set(_))
    NamedInResources.classes(index, wanted, bits).map(c => syms.className(c))

  private def named(sources: Seq[ClassSource], wanted: Set[String], entries: Seq[Int]): Set[String] = namedAll(sources, wanted, entries).toSet

  private val conf = """akka { serializer = "p.A", other = "p.B$Inner", nope = "p.Missing", dispatcher = "akka.actor.default-dispatcher", jdk = "java.util.ArrayList" }"""

  for (label, make) <- List[(String, Map[String, String] => Path)](("a directory", directoryWith), ("a jar", jarWith)) do
    test(s"$label: resource names leave out classes and directories, and a jar leaves out multi-release overlays") {
      val source = open(make(Map("reference.conf" -> conf, "META-INF/versions/17/x.conf" -> "x", "META-INF/MANIFEST.MF.bak" -> "y")))
      val names  = source.resourceNames.toSet
      assert(names.contains("reference.conf"), names.toString)
      assert(!names.exists(_.endsWith(".class")), names.toString)
      // Only a jar has overlays (the JVM applies `META-INF/versions` to jars, not to a directory, where it is just a folder).
      assertEquals(names.contains("META-INF/versions/17/x.conf"), label == "a directory", names.toString)
      assert(!names.exists(_.endsWith("/")), names.toString)
    }

    test(s"$label: a class named in a wanted file is found, a word that is not a class, a missing class and a JDK class are not") {
      assertEquals(named(Seq(open(make(Map("reference.conf" -> conf)))), Set("reference.conf"), Seq(0)), Set("p/A", "p/B$Inner"))
    }

    test(s"$label: only a file of a wanted name counts, matched whole, anywhere in the path") {
      val files = Map("other.conf" -> "x = \"p.C\"", "a/b/reference.conf" -> "x = \"p.A\"", "a/xreference.conf" -> "x = \"p.B\"")
      assertEquals(named(Seq(open(make(files))), Set("reference.conf"), Seq(0)), Set("p/A"))
      assertEquals(named(Seq(open(make(files))), Set("a/b/reference.conf"), Seq(0)), Set("p/A"))
    }

  test("only the entries asked for are read, and a class is reported once however many files name it") {
    val one = open(jarWith(Map("reference.conf" -> "x = \"p.A\" y = \"p.A\"")))
    val two = open(jarWith(Map("reference.conf" -> "x = \"p.A\"")))
    assertEquals(namedAll(Seq(one, two), Set("reference.conf"), Seq(0, 1)), Vector("p/A"), "once, though three mentions in two files name it")
    // Entry 1 is left out: its file names nothing the first does not, but entry 0 alone is still found, and an entry that is not traversed adds nothing.
    val zero = open(jarWith(Map.empty))
    assertEquals(named(Seq(zero, two), Set("reference.conf"), Seq(0)), Set.empty[String])
  }
