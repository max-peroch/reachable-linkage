package reachablelinkage

import java.nio.file.Files
import java.util.Random
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.*
import reachablelinkage.classfile.{ClassInfo, ClassParser}
import reachablelinkage.classpath.ClassSource
import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.Jars
import reachablelinkage.symbols.Symbols

/**
 * Damaged input must never throw and never produce a wrong answer: a class file becomes a `Left`, a damaged summary is
 * ignored (the class is parsed again), and a text file becomes an error value. Seeded, so a failure repeats.
 */
class FuzzSuite extends munit.FunSuite:

  override def munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private def classBytes(name: String): Array[Byte] =
    val jdk = ClassSource.jdk()
    try jdk.read(name).getOrElse(fail(s"no $name")) finally jdk.close()

  private val samples = List("java/util/HashMap", "java/lang/String", "java/util/concurrent/ConcurrentHashMap", "java/lang/Enum").map(n => n -> classBytes(n))

  private def mutate(random: Random, original: Array[Byte]): Array[Byte] =
    random.nextInt(4) match
      case 0 => original.take(random.nextInt(original.length))                       // truncated
      case 1 => original.drop(random.nextInt(original.length))                        // the start missing
      case 2 => original ++ Array.fill(1 + random.nextInt(32))(random.nextInt().toByte) // garbage at the end
      case _ =>                                                                         // a few bytes changed
        val copy = original.clone()
        (0 to random.nextInt(6)).foreach(_ => copy(random.nextInt(copy.length)) = random.nextInt().toByte)
        copy

  /** What a ClassInfo holds, by name, so two parses with different symbol tables compare. */
  private def shape(info: ClassInfo, syms: Symbols): String =
    def cls(id: Int): String = if id < 0 then "-" else syms.className(reachablelinkage.symbols.ClassId(id))
    List(
      cls(info.id.value), info.access.toString, cls(info.superName.value), info.interfaces.map(i => cls(i.value)).mkString(","),
      info.methods.map(m => s"${syms.name(m.name)}${syms.desc(m.desc)}:${m.access}").mkString(","),
      info.fields.map(f => s"${syms.name(f.name)}:${syms.desc(f.desc)}:${f.access}").mkString(","),
      info.methodRefs.grouped(4).map(r => s"${cls(r(0))}.${syms.name(reachablelinkage.symbols.NameId(r(1)))}${r(3)}").mkString(","),
      info.classRefs.map(cls).mkString(","), info.methodRefStart.mkString(","), info.encodedProbes.mkString(","),
      info.annotations.mkString(","), info.newRefs.mkString(","), info.fieldWrites.mkString(","),
      info.signatureTypes.grouped(3).map(t => s"${t(0)}:${t(1)}:${cls(t(2))}").mkString(","),
      info.literalArgs.grouped(3).map(t => s"${cls(t(0))}.${t(1)}:${cls(t(2))}").mkString(",")).mkString("|")

  test("a class file that is cut, shifted, padded or has bytes changed is a Left or a consistent ClassInfo, never an exception") {
    val random = new Random(20241122L)
    var parsed, rejected = 0
    for (name, original) <- samples; _ <- 1 to 600 do
      val bytes = mutate(random, original)
      val syms  = new Symbols
      ClassParser.parse(bytes, syms) match
        case Left(_) => rejected += 1
        case Right(info) =>
          parsed += 1
          // Whatever it accepted must hang together: the per-method start tables bracket the reference arrays.
          assertEquals(info.methodRefStart.length, info.methods.length + 1, name)
          assertEquals(info.methodRefStart.last, info.methodRefs.length / 4, name)
          assertEquals(info.fieldRefStart.last, info.fieldRefs.length / 3, name)
          assertEquals(info.classRefStart.last, info.classRefs.length, name)
    assert(rejected > 100 && parsed > 0, s"the mutations should produce both outcomes: parsed $parsed, rejected $rejected")
  }

  test("every prefix of a class file is rejected or consistent") {
    val (_, bytes) = samples.head
    val syms = new Symbols
    (0 until bytes.length by 7).foreach: n =>
      ClassParser.parse(bytes.take(n), syms) match
        case Left(_)     => ()
        case Right(info) => assertEquals(info.methodRefStart.length, info.methods.length + 1, s"prefix $n")
  }

  test("a damaged summary is never believed: each class comes out as the jar's own, or is parsed again") {
    val lib  = compile((1 to 6).map(i => s"lib/C$i.java" -> s"package lib; public class C$i { public static int m$i(String s) { return s.length() + $i; } public int f$i; }").toMap)
    val jar  = Jars.of(lib)
    val cache = Files.createTempDirectory("summaries")
    val reference = new Symbols
    val plain = ClassSource.open(jar).fold(e => fail(e.message), identity)
    val expected = plain.classNames.toList.sorted.map(n => n -> shape(ClassParser.parse(plain.read(n).get, reference).toOption.get, reference)).toMap
    val first = ClassSource.open(jar, Some(cache)).fold(e => fail(e.message), identity)
    first.classNames.foreach(n => first.summarized(n, new Symbols))   // builds and saves the summary
    val file     = Files.list(cache).iterator.asScala.toList.head
    val original = Files.readAllBytes(file)
    val random   = new Random(7L)
    var wrong, fellBack = 0
    for _ <- 1 to 400 do
      Files.write(file, mutate(random, original))
      val source = ClassSource.open(jar, Some(cache)).fold(e => fail(e.message), identity)
      val syms   = new Symbols
      source.classNames.foreach: n =>
        source.summarized(n, syms) match
          case null => fellBack += 1
          case info => if shape(info, syms) != expected(n) then wrong += 1
      source.close()
    assertEquals(wrong, 0, s"damaged summaries produced $wrong wrong classes (fell back $fellBack times)")
  }

  test("text that is not a rules file or a class-load log is an error value, whatever it holds") {
    val random = new Random(99L)
    def junk(): String =
      val chars = "abc xyz.$/\t\n\r#=;:[]\\0123456789-_ é\u0000".toCharArray
      (0 until random.nextInt(300)).map(_ => chars(random.nextInt(chars.length))).mkString
    for _ <- 1 to 300 do
      val f = Files.createTempFile("junk", ".txt")
      Files.writeString(f, junk())
      KeepRule.parse(f)             // an Either, however it falls
      ClassLoadLog.parse(f)
      Files.write(f, Array.fill(random.nextInt(200))(random.nextInt().toByte))   // not even text
      KeepRule.parse(f); ClassLoadLog.parse(f)
  }

  test("a report of conflicts with empty, odd and long names does not throw") {
    val random = new Random(5L)
    def name(): String = List("", "$", "a.B", "app.Foo$$anon$1", "$anonfun$", "x.Y$", "lambda$", "a." + "b" * 300)(random.nextInt(8))
    val conflicts = (1 to 300).map: _ =>
      LinkageConflict(ConflictCategory.values()(random.nextInt(ConflictCategory.values().length)),
        Dependency(name(), name(), "()V", random.nextInt(50), name(), name(), ""), name(), name(), name(), ConflictLabel.values()(random.nextInt(ConflictLabel.values().length)))
    assert(Report.render(conflicts.asJava, ReportOptions(maxGroups = 5, maxSitesPerGroup = 1)).nonEmpty)
    assert(Report.render(conflicts.asJava, ReportOptions(demangle = false)).nonEmpty)
  }
