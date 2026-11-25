package reachablelinkage.classpath

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The on-disk JDK listing is an optimisation and must never change an answer: whatever is wrong with
 * it, a check still sees exactly the classes the running JDK has.
 */
class JdkListingSuite extends munit.FunSuite:

  private def freshDir(): Path = Files.createTempDirectory("jdk-listing")
  private def key: String      = JdkListing.imageKey().getOrElse(fail("this JDK has no lib/modules"))

  private def cacheFiles(dir: Path): List[Path] =
    val s = Files.list(dir)
    try s.iterator.asScala.toList finally s.close()

  private def names(src: ClassSource): Set[String] = src.classNames.toSet

  test("a cold load saves the listing, and a cached load sees the same classes as the JDK") {
    val dir  = freshDir()
    val live = ClassSource.jdk()
    val cold = ClassSource.jdk(Some(dir))
    assertEquals(cacheFiles(dir).map(_.getFileName.toString).map(_.endsWith(".listing")), List(true))
    val warm = ClassSource.jdk(Some(dir))
    try
      assertEquals(names(cold), names(live))
      assertEquals(names(warm), names(live))
      assert(names(live).size > 20000, "the JDK has tens of thousands of classes")
    finally { live.close(); cold.close(); warm.close() }
  }

  test("a second load really reads the file: a doctored file shows through") {
    val dir  = freshDir()
    val real = JdkListing.list()
    val file = dir.resolve(JdkListing.fileName(key))
    val doctored = JdkListing(real.modules, real.names.drop(1), real.moduleOf.drop(1))
    JdkListing.writeCached(file, key, doctored)
    val loaded = JdkListing.load(Some(dir))
    assertEquals(loaded.names.length, real.names.length - 1)
    assert(!loaded.names.contains(real.names.head), "the dropped class came from the file, not the JDK")
  }

  test("a corrupt file is ignored and replaced") {
    val dir  = freshDir()
    val file = dir.resolve(JdkListing.fileName(key))
    Files.writeString(file, "this is not a listing")
    val loaded = JdkListing.load(Some(dir))
    assertEquals(loaded.names.length, JdkListing.list().names.length)
    assertEquals(JdkListing.readCached(file, key).map(_.names.length), Some(loaded.names.length))
  }

  test("a truncated file is ignored") {
    val dir  = freshDir()
    val file = dir.resolve(JdkListing.fileName(key))
    JdkListing.writeCached(file, key, JdkListing.list())
    val bytes = Files.readAllBytes(file)
    Files.write(file, bytes.take(bytes.length / 2))
    assertEquals(JdkListing.readCached(file, key), None)
    assertEquals(JdkListing.load(Some(dir)).names.length, JdkListing.list().names.length)
  }

  test("a file made for another JDK image is not used") {
    val dir  = freshDir()
    val real = JdkListing.list()
    val file = dir.resolve(JdkListing.fileName(key))
    JdkListing.writeCached(file, "some|other|jdk", JdkListing(real.modules, real.names.take(10), real.moduleOf.take(10)))
    val loaded = JdkListing.load(Some(dir))
    assertEquals(loaded.names.length, real.names.length)
    assertEquals(JdkListing.readCached(file, key).map(_.names.length), Some(real.names.length), "rewritten for this JDK")
  }

  test("a cache directory that cannot be written is not an error") {
    val blocker = Files.createTempFile("not-a-dir", ".txt")
    val loaded  = JdkListing.load(Some(blocker.resolve("cache")))
    assertEquals(loaded.names.length, JdkListing.list().names.length)
  }

  test("classes of several modules can be read, and an unknown class is absent") {
    val jdk = ClassSource.jdk(Some(freshDir()))
    try
      for c <- Seq("java/lang/Object", "java/sql/Connection", "javax/swing/JFrame", "java/net/http/HttpClient") do
        val bytes = jdk.read(c).getOrElse(fail(s"$c is not readable"))
        assertEquals(bytes.take(4).toList, List(0xca, 0xfe, 0xba, 0xbe).map(_.toByte), c)
      assertEquals(jdk.read("no/such/Clazz"), None)
    finally jdk.close()
  }

  test("module-info files are not listed as classes") {
    assert(!names(ClassSource.jdk()).exists(_.contains("module-info")))
  }

  test("the listing is deterministic: same order every time") {
    assertEquals(JdkListing.list().names.toList, JdkListing.list().names.toList)
  }

  test("a class listed under a module this JDK does not have is unreadable, not an exception") {
    val dir  = freshDir()
    val real = JdkListing.list()
    val ghostModule = real.modules.length
    val doctored = JdkListing(real.modules :+ "no.such.module", real.names :+ "ghost/Phantom", real.moduleOf :+ ghostModule)
    JdkListing.writeCached(dir.resolve(JdkListing.fileName(key)), key, doctored)
    val jdk = ClassSource.jdk(Some(dir))
    try
      assert(jdk.classNames.contains("ghost/Phantom"), "the doctored listing is in use")
      assertEquals(jdk.read("ghost/Phantom"), None)
      assert(jdk.read("java/lang/Object").exists(_.nonEmpty), "real classes are still read")
    finally jdk.close()
  }

