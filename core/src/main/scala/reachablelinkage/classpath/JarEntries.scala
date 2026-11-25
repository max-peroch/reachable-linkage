package reachablelinkage.classpath

import java.io.IOException
import java.util.jar.{JarEntry, JarFile}
import scala.jdk.CollectionConverters.*
import scala.util.control.Exception.catching

/**
 * Which entry of a jar a class comes from. A plain jar has one entry per class. A multi-release jar (its manifest says
 * `Multi-Release: true`) may also hold `META-INF/versions/N/pkg/C.class`, and a JVM running Java R uses, for each class, the
 * entry with the highest N that is at most R (N is at least 9), else the base one; a class may exist only under a version.
 * The manifest rule is the JVM's: without the attribute the versioned entries are never used.
 */
private[classpath] object JarEntries:
  private val Versioned = """^META-INF/versions/(\d+)/(.+)\.class$""".r

  /** The class an entry holds: its internal name and the release it is for (0 for the base). None for anything else. */
  private def classIn(entryName: String): Option[(String, Int)] = entryName match
    case Versioned(release, name) => release.toIntOption.map(r => (name, r))
    case n if n.endsWith(".class") && !n.startsWith("META-INF/") => Some((n.dropRight(6), 0))
    case _ => None

  private def notAClass(name: String): Boolean = name.endsWith("module-info") || name.endsWith("package-info")

  def isMultiRelease(jar: JarFile): Boolean =
    catching(classOf[IOException]).opt(jar.getManifest).flatMap(Option(_))
      .exists(m => "true".equalsIgnoreCase(String.valueOf(m.getMainAttributes.getValue("Multi-Release"))))

  /** Each class of the jar, in the order it is first listed, with the entry a JVM running Java `release` would use. */
  def resolve(jar: JarFile, release: Int): Vector[(String, JarEntry)] =
    val multi = isMultiRelease(jar)
    val candidates = jar.entries.asScala.filterNot(_.isDirectory).flatMap: e =>
      classIn(e.getName).filter((name, r) => !notAClass(name) && (r == 0 || (multi && r >= 9 && r <= release))).map((name, r) => (name, r, e))
    .toVector
    val best = candidates.groupMapReduce(_._1)(c => (c._2, c._3))((a, b) => if b._1 > a._1 then b else a)
    candidates.map(_._1).distinct.map(name => (name, best(name)._2))
