package reachablelinkage.fixture

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.JavaFixture.compile

/** A library and its upgrade: v1 is compiled from `lib`; v2 is v1 with the classes compiled from `changed` swapped in. Code compiled against v1 and run against
  * v2 therefore sees exactly the change, with every other class still the bytecode that was built against the old one.
  */
object ChangedLibrary:
  /** @return the v1 and v2 class directories */
  def build(lib: Map[String, String], changed: Map[String, String]): (Path, Path) =
    val v1 = compile(lib)
    val v2 = FixtureDirs.fresh().resolve("v2")
    copyTree(v1, v2)
    copyTree(compile(changed), v2)
    (v1, v2)

  private def copyTree(from: Path, to: Path): Unit =
    val stream = Files.walk(from)
    try
      stream.iterator.asScala.foreach: p =>
        val target = to.resolve(from.relativize(p).toString)
        if Files.isDirectory(p) then Files.createDirectories(target)
        else Files.copy(p, target, StandardCopyOption.REPLACE_EXISTING)
    finally stream.close()
