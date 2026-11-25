package reachablelinkage.fixture

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.JavaFixture.compile

/**
 * A library built twice: v1 has `lib.Gone.x()` and `lib.Gone.f`, v2 is the same classes with both
 * removed (and any `removed` classes deleted), so code compiled against v1 breaks exactly where it
 * uses them.
 */
object GoneLibrary:
  private val goneV1 = "package lib; public class Gone { public static void x() {} public static int f = 1; }"
  private val goneV2 = "package lib; public class Gone { }"

  /**
   * @param lib       Java sources of the library (they may call `lib.Gone.x()`)
   * @param resources files written into both versions, by relative path (service files, say)
   * @param removed   internal names (`lib/Vanished`) of classes that exist in v1 but not in v2
   * @return the v1 and v2 class directories
   */
  def build(
      lib: Map[String, String],
      resources: Map[String, String] = Map.empty,
      removed: Set[String] = Set.empty,
      classpath: Seq[Path] = Nil
  ): (Path, Path) =
    val v1 = compile(lib + ("lib/Gone.java" -> goneV1), classpath)
    resources.foreach: (path, content) =>
      val file = v1.resolve(path)
      Files.createDirectories(file.getParent)
      Files.writeString(file, content)
    val v2     = FixtureDirs.fresh().resolve("v2")
    val stream = Files.walk(v1)
    try stream.iterator.asScala.foreach: p =>
      val target = v2.resolve(v1.relativize(p).toString)
      if Files.isDirectory(p) then Files.createDirectories(target) else Files.copy(p, target)
    finally stream.close()
    val gone = compile(Map("lib/Gone.java" -> goneV2))
    Files.copy(gone.resolve("lib/Gone.class"), v2.resolve("lib/Gone.class"), StandardCopyOption.REPLACE_EXISTING)
    removed.foreach(name => Files.delete(v2.resolve(name + ".class")))
    (v1, v2)
