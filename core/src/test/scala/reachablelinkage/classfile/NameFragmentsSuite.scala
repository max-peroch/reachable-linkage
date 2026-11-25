package reachablelinkage.classfile

import java.nio.file.Files

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.symbols.Symbols

/** A class that builds the name of a class it loads holds the end of the name as a constant (`pkg + ".kqueue.KQueue"`). Such a fragment counts only in a class
  * that loads a class by name, and names, later, only classes that exist.
  */
class NameFragmentsSuite extends munit.FunSuite:

  test("a fragment starts with a dot and has at least two segments, the last one capitalised") {
    assertEquals(ProbeNames.fragment(".kqueue.KQueue"), "/kqueue/KQueue")
    assertEquals(ProbeNames.fragment(".uring.IoUringIoHandler"), "/uring/IoUringIoHandler")
    assertEquals(ProbeNames.fragment(".a.b.C$D"), "/a/b/C$D")
  }

  test("what is not a fragment") {
    for s <- List("kqueue.KQueue", ".KQueue", ".kqueue", ".kqueue.kqueue", "..a.B", ".a..B", ". a.B", ".a.B ", "", ".", ".a.") do
      assertEquals(ProbeNames.fragment(s), null, s"'$s'")
  }

  private val dir = compile(
    Map(
      "p/Loads.java" -> "package p; public class Loads { static Object f(String base) throws Exception { return Class.forName(base + \".kqueue.KQueue\"); } }",
      "p/LoadsCl.java" -> "package p; public class LoadsCl { static Object f(ClassLoader l, String base) throws Exception { return l.loadClass(base + \".epoll.Epoll\"); } }",
      "p/NoLoad.java"   -> "package p; public class NoLoad { static String f(String base) { return base + \".kqueue.KQueue\"; } }",
      "p/OnlyLoad.java" -> "package p; public class OnlyLoad { static Object f(String name) throws Exception { return Class.forName(name); } }"
    )
  )

  // The same loader compiled for Java 8, where `+` is a StringBuilder and the tail is an ldc: how netty's and armeria's own classes are built.
  private val dir8 = compile(
    Map(
      "p/Loads8.java" -> "package p; public class Loads8 { static Object f(String base) throws Exception { return Class.forName(base + \".kqueue.KQueue\"); } }"
    ),
    Nil,
    Seq("--release", "8")
  )

  /** The tails of class names the class kept (the other probes are not what these tests are about). */
  private def tails(probes: Iterable[Probe]): List[String] = probes.collect { case Probe.NameTail(suffix) => suffix }.toList

  private def probes(name: String): List[String] =
    val syms = new Symbols
    ClassParser.parse(Files.readAllBytes(dir.resolve(s"p/$name.class")), syms).fold(e => fail(e.message), info => tails(info.probes))

  test("a class that loads by name keeps its fragments, as name tails, whether the concatenation is an indy recipe or a StringBuilder") {
    assertEquals(probes("Loads"), List("/kqueue/KQueue"))
    assertEquals(probes("LoadsCl"), List("/epoll/Epoll"))
  }

  test("the same through a StringBuilder and an ldc, as Java 8 bytecode has it") {
    val syms  = new Symbols
    val found = ClassParser.parse(Files.readAllBytes(dir8.resolve("p/Loads8.class")), syms).fold(e => fail(e.message), info => tails(info.probes))
    assertEquals(found, List("/kqueue/KQueue"))
  }

  test("a fragment in a class that loads nothing by name is not kept, and a loader with no fragment has none") {
    assertEquals(probes("NoLoad"), Nil)
    assertEquals(probes("OnlyLoad"), Nil)
  }
