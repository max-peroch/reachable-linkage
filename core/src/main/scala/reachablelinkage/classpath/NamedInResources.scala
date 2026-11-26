package reachablelinkage.classpath

import java.nio.charset.StandardCharsets
import java.util.BitSet
import java.util.regex.Pattern
import scala.annotation.tailrec

import reachablelinkage.symbols.ClassId

/** The classes named in configuration files. A framework that reads a class name from a file (`akka.actor.serializers { jackson = "a.b.C" }` in
  * `reference.conf`, a servlet in `web.xml`, an entity in `persistence.xml`) loads it by name, so no instruction anywhere names it. Any word in such a file
  * that is a class of the classpath counts: an over-approximation that never invents a class.
  */
private[reachablelinkage] object NamedInResources {

  /** A dotted name, as a class is written in a file (`$` for a nested class). */
  private val Name = Pattern.compile("[A-Za-z_$][A-Za-z0-9_$]*(?:\\.[A-Za-z_$][A-Za-z0-9_$]*)+")

  /** Does `path` name one of the files wanted: the file name itself, or a path that ends with it (`META-INF/persistence.xml`)? */
  private def wanted(path: String, files: Set[String]): Boolean =
    files.exists(f => path == f || path.endsWith("/" + f))

  /** The classes of the entries in `entries` (bits over source indices) that a wanted file of those entries names.
    * @param files
    *   file names, such as `reference.conf` or `META-INF/persistence.xml`
    */
  def classes(index: ClassIndex, files: Set[String], entries: BitSet): Vector[ClassId] = {
    val found                       = new BitSet
    val out                         = Vector.newBuilder[ClassId]
    @tailrec def each(i: Int): Unit =
      if (i >= 0) {
        val source = index.sources(i)
        source.resourceNames
          .filter(wanted(_, files))
          .foreach { path =>
            source
              .readResource(path)
              .foreach { bytes =>
                val matcher = Name.matcher(new String(bytes, StandardCharsets.UTF_8))
                while (matcher.find()) {
                  val c = index.syms.findClass(matcher.group.replace('.', '/'))
                  if (!c.isNone && index.exists(c) && entries.get(index.sourceIndex(c)) && !found.get(c.value)) {
                    found.set(c.value)
                    out += c
                  }
                }
              }
          }
        each(entries.nextSetBit(i + 1))
      }
    each(entries.nextSetBit(0))
    out.result()
  }
}
