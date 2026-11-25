package reachablelinkage.classpath

import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry
import scala.jdk.CollectionConverters.*

import reachablelinkage.classfile.{ClassInfo, ClassParser, Probe}
import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.symbols.{ClassId, Symbols}

/**
 * A jar summary is a cache of what `ClassParser` produces, so the one thing that must hold is that a
 * class decoded from it is exactly the class that parsing the jar would give, and that anything wrong
 * with the cache means parsing, never a wrong answer.
 */
class JarSummarySuite extends munit.FunSuite:

  private def freshDir(): Path = Files.createTempDirectory("jar-summary")

  private def summaryFiles(dir: Path): List[Path] =
    val s = Files.list(dir)
    try s.iterator.asScala.filter(_.getFileName.toString.endsWith(".summary")).toList finally s.close()

  private def jar(entries: Seq[(String, Array[Byte])]): Path =
    val file = Files.createTempFile("fixture", ".jar")
    val out  = new JarOutputStream(Files.newOutputStream(file))
    try entries.foreach: (name, bytes) =>
      out.putNextEntry(new ZipEntry(name)); out.write(bytes); out.closeEntry()
    finally out.close()
    file

  private def classesOf(dir: Path): Seq[(String, Array[Byte])] =
    val s = Files.walk(dir)
    try s.iterator.asScala.filter(_.toString.endsWith(".class")).map(p => dir.relativize(p).toString -> Files.readAllBytes(p)).toList
    finally s.close()

  /** A class's content with every id turned back into its name, so two runs' ids can be compared. */
  private def render(i: ClassInfo, s: Symbols): String =
    def cls(id: Int): String = if id < 0 then "-" else s.className(ClassId(id))
    val sb = new StringBuilder
    sb ++= s"${cls(i.id.value)} access=${i.access} super=${cls(i.superName.value)} ifaces=${i.interfaces.map(c => cls(c.value)).mkString(",")}\n"
    i.methods.foreach(m => sb ++= s"m ${s.name(m.name)}${s.desc(m.desc)} ${m.access}\n")
    i.fields.foreach(f => sb ++= s"f ${s.name(f.name)}:${s.desc(f.desc)} ${f.access}\n")
    i.methodRefs.grouped(4).foreach(r => sb ++= s"mr ${cls(r(0))}.${s.name(reachablelinkage.symbols.NameId(r(1)))}${s.desc(reachablelinkage.symbols.DescId(r(2)))} ${r(3)}\n")
    i.fieldRefs.grouped(3).foreach(r => sb ++= s"fr ${cls(r(0))}.${s.name(reachablelinkage.symbols.NameId(r(1)))}:${s.desc(reachablelinkage.symbols.DescId(r(2)))}\n")
    sb ++= s"cr ${i.classRefs.map(cls).mkString(",")}\n"
    sb ++= s"starts ${i.methodRefStart.mkString(",")} | ${i.fieldRefStart.mkString(",")} | ${i.classRefStart.mkString(",")}\n"
    sb ++= s"guards ${i.guards.mkString(",")}\nprobes ${i.encodedProbes.mkString(",")}\n"
    sb ++= s"newRefs ${i.newRefs.mkString(",")} writes ${i.fieldWrites.mkString(",")} generic ${i.signatureTypes.grouped(3).map(t => s"${t(0)}:${t(1)}:${cls(t(2))}").mkString(",")} literals ${i.literalArgs.grouped(3).map(t => s"${cls(t(0))}.${s.name(reachablelinkage.symbols.NameId(t(1)))}:${cls(t(2))}").mkString(",")}\n"
    sb ++= s"annotations ${i.annotations.grouped(4).map(a => s"${a(0)}:${a(1)}:${cls(a(2))}:${cls(a(3))}").mkString(",")}\n"
    sb.toString

  /** Every class of `file` through a summary must equal the same class parsed from the jar. */
  private def assertSameAsParsing(file: Path, cache: Path): Int =
    val cached = ClassSource.open(file, Some(cache)).toOption.get
    val plain  = ClassSource.open(file).toOption.get
    val a, b   = new Symbols
    var n      = 0
    try cached.classNames.filterNot(c => c.endsWith("module-info") || c.endsWith("package-info")).foreach: name =>
      val parsed  = ClassParser.parse(plain.read(name).get, a)
      val decoded = cached.summarized(name, b)
      parsed match
        case Right(p) =>
          assert(decoded != null, s"$name: parsed fine but the summary has no record")
          assertEquals(render(decoded.nn, b), render(p, a), name)
          n += 1
        case Left(_) => assertEquals(decoded, null, s"$name does not parse, so the summary must not have it")
    finally { cached.close(); plain.close() }
    n

  private val compilerJars: Seq[Path] =
    val p  = new java.util.Properties()
    val in = getClass.getResourceAsStream("/scala-compilers.properties")
    try p.load(in) finally in.close()
    p.getProperty("2.13").split(java.io.File.pathSeparator).toSeq.map(Paths.get(_))

  test("every class of the five Scala 2.13 compiler jars decodes to exactly what parsing gives") {
    val cache = freshDir()
    val total = compilerJars.map(assertSameAsParsing(_, cache)).sum
    assert(total > 8000, s"only $total classes were compared")
    assertEquals(summaryFiles(cache).size, compilerJars.size, "one summary per jar")
  }

  // Java compiled to cover what the codec carries: handlers (guards), constants that look like class
  // names (probes), method handles from lambdas, fields, statics, an interface and an enum.
  private val synthetic = Map(
    "lib/Opt.java" -> "package lib; public class Opt { public static boolean has() { try { Class.forName(\"lib.Missing\"); return true; } catch (ClassNotFoundException e) { return false; } } }",
    "lib/Guard.java" -> "package lib; public class Guard { public static void go() { try { Opt.has(); System.out.println(lib.Other.X); } catch (LinkageError e) { } finally { Opt.has(); } } }",
    "lib/Other.java" -> "package lib; public class Other { public static int X = 1; private int y; protected void p() {} }",
    "lib/Lambdas.java" -> "package lib; public class Lambdas { public static Runnable r() { return () -> Opt.has(); } public static java.util.function.Supplier<Other> s() { return Other::new; } }",
    "lib/Api.java" -> "package lib; public interface Api { void m(); default void d() { Opt.has(); } }",
    "lib/E.java" -> "package lib; public enum E { A, B { void f() {} }; void f() {} }")

  test("a synthetic jar with guards, probes, handles, fields and an enum decodes exactly") {
    val classes = classesOf(compile(synthetic))
    val file    = jar(classes)
    assertEquals(assertSameAsParsing(file, freshDir()), classes.size)
    val cached = ClassSource.open(file, Some(freshDir())).toOption.get
    val guard  = cached.summarized("lib/Guard", new Symbols).nn
    assert(guard.guards.nonEmpty, "the handler's guard must be in the summary")
    assert(cached.summarized("lib/Opt", new Symbols).nn.probes.contains(Probe.ClassName("lib/Missing")), "the probe must be in the summary")
    cached.close()
  }

  test("the first use builds the summary and a later instance loads it") {
    val cache = freshDir()
    val file  = jar(classesOf(compile(synthetic)))
    val first = ClassSource.open(file, Some(cache)).toOption.get
    first.summarized("lib/Opt", new Symbols)
    assertEquals(first.asInstanceOf[JarSource].summaryOutcome, SummaryOutcome.Built)
    val second = ClassSource.open(file, Some(cache)).toOption.get
    second.summarized("lib/Opt", new Symbols)
    assertEquals(second.asInstanceOf[JarSource].summaryOutcome, SummaryOutcome.Loaded)
    assertEquals(summaryFiles(cache).size, 1)
    first.close(); second.close()
  }

  test("the same classes at another path share one summary: it is named by content, not by place") {
    val cache = freshDir()
    val a     = jar(classesOf(compile(synthetic)))
    val b     = Files.createTempFile("copy", ".jar")
    Files.copy(a, b, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    val sa = ClassSource.open(a, Some(cache)).toOption.get; sa.summarized("lib/Opt", new Symbols)
    val sb = ClassSource.open(b, Some(cache)).toOption.get; sb.summarized("lib/Opt", new Symbols)
    assertEquals(sb.asInstanceOf[JarSource].summaryOutcome, SummaryOutcome.Loaded)
    assertEquals(summaryFiles(cache).size, 1)
    sa.close(); sb.close()
  }

  test("a jar whose classes changed does not use the old summary") {
    val cache = freshDir()
    val all   = classesOf(compile(synthetic))
    val path  = Files.createTempFile("changing", ".jar")
    def write(entries: Seq[(String, Array[Byte])]): Unit =
      val out = new JarOutputStream(Files.newOutputStream(path))
      try entries.foreach((n, b) => { out.putNextEntry(new ZipEntry(n)); out.write(b); out.closeEntry() }) finally out.close()
    write(all.filterNot(_._1.contains("Other")))
    val v1 = ClassSource.open(path, Some(cache)).toOption.get
    assertEquals(v1.summarized("lib/Other", new Symbols), null)
    v1.close()
    write(all)
    val v2 = ClassSource.open(path, Some(cache)).toOption.get
    assert(v2.summarized("lib/Other", new Symbols) != null, "the new class must be found, not the old summary's absence")
    assertEquals(v2.asInstanceOf[JarSource].summaryOutcome, SummaryOutcome.Built)
    assertEquals(summaryFiles(cache).size, 2, "the old summary stays, under its own name")
    v2.close()
  }

  test("the fingerprint depends on the entry names, and on the classes' contents, and on nothing else") {
    val classes = classesOf(compile(synthetic))
    def fp(entries: Seq[(String, Array[Byte])]): List[Byte] =
      val file = jar(entries)
      val j    = new java.util.jar.JarFile(file.toFile)
      try JarSummary.fingerprint(JarEntries.resolve(j, Runtime.version.feature)).toList finally j.close()
    val base = fp(classes)
    assertEquals(fp(classes), base, "the same jar gives the same fingerprint")
    assertEquals(fp(classes :+ ("META-INF/MANIFEST.MF" -> "x".getBytes)), base, "non-class entries are not part of it")
    val renamed = classes.updated(0, (classes.head._1.replace("lib/", "lib2/"), classes.head._2))
    assertNotEquals(fp(renamed), base, "a renamed entry changes it")
    val first = classes.head
    val changed = classes.updated(0, (first._1, first._2 :+ 0.toByte))
    assertNotEquals(fp(changed), base, "changed bytes change it")
  }

  private def corruptThenLoad(damage: Path => Unit): (SummaryOutcome, Boolean) =
    val cache = freshDir()
    val file  = jar(classesOf(compile(synthetic)))
    val s1    = ClassSource.open(file, Some(cache)).toOption.get; s1.summarized("lib/Opt", new Symbols); s1.close()
    damage(summaryFiles(cache).head)
    val s2    = ClassSource.open(file, Some(cache)).toOption.get
    val ok    = s2.summarized("lib/Opt", new Symbols) != null
    val out   = s2.asInstanceOf[JarSource].summaryOutcome
    s2.close()
    (out, ok)

  test("a corrupt summary is rebuilt, and classes still come out right") {
    assertEquals(corruptThenLoad(f => Files.writeString(f, "not a summary")), (SummaryOutcome.Built, true))
  }

  test("a truncated summary is rebuilt") {
    assertEquals(corruptThenLoad(f => Files.write(f, Files.readAllBytes(f).take(200))), (SummaryOutcome.Built, true))
  }

  test("a summary from another parser version is not used") {
    val patch: Path => Unit = f =>
      val bytes = Files.readAllBytes(f)
      bytes(11) = (bytes(11) + 1).toByte // the low byte of the parser version
      Files.write(f, bytes)
    assertEquals(corruptThenLoad(patch), (SummaryOutcome.Built, true))
  }

  test("a summary with a wrong fingerprint is not used") {
    val patch: Path => Unit = f =>
      val bytes = Files.readAllBytes(f)
      bytes(20) = (bytes(20) + 1).toByte
      Files.write(f, bytes)
    assertEquals(corruptThenLoad(patch), (SummaryOutcome.Built, true))
  }

  test("a cache directory that cannot be written still gives correct classes") {
    val blocker = Files.createTempFile("not-a-dir", ".txt")
    val file    = jar(classesOf(compile(synthetic)))
    assertEquals(assertSameAsParsing(file, blocker.resolve("cache")), classesOf(compile(synthetic)).size)
  }

  test("class directories are never summarized") {
    val cache = freshDir()
    val dir   = compile(synthetic)
    val src   = ClassSource.open(dir, Some(cache)).toOption.get
    assertEquals(src.summarized("lib/Opt", new Symbols), null)
    assertEquals(summaryFiles(cache), Nil)
  }

  test("a class that does not parse has no record, and the others still do") {
    val classes = classesOf(compile(synthetic)) :+ ("lib/Broken.class" -> Array[Byte](0xca.toByte, 0xfe.toByte, 0xba.toByte, 0xbe.toByte, 0, 0))
    val src = ClassSource.open(jar(classes), Some(freshDir())).toOption.get
    assertEquals(src.summarized("lib/Broken", new Symbols), null)
    assert(src.summarized("lib/Opt", new Symbols) != null)
    src.close()
  }

  // If this fails, ClassInfo changed. The summary codec must carry the new field, and
  // ClassParser.Version must be bumped so old summaries are not used.
  test("ClassInfo has the shape the summary codec knows") {
    val shape = classOf[ClassInfo].getDeclaredConstructors.map(_.getParameterTypes.map(_.getSimpleName).toList).toList
    assertEquals(
      shape.filter(_.length == 20),
      List(List("int", "int", "int", "int[]", "Member[]", "Member[]", "int[]", "int[]", "int[]", "int[]", "int[]", "int[]", "int[]", "String[]", "int[]", "int[]", "int[]", "int[]", "int[]", "Positions")),
      "update JarSummary.encode/decode and bump ClassParser.Version"
    )
  }
