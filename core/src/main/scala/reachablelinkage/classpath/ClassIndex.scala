package reachablelinkage.classpath

import java.util.Arrays
import scala.collection.mutable.ArrayBuffer

import reachablelinkage.classfile.{ClassFileError, ClassInfo, ClassParser, ParseDetail}
import reachablelinkage.symbols.{ClassId, Symbols}

/** Classpath-ordered index. Construction reads only tables of contents; a class is parsed the first time something asks for it, then kept. First entry on the
  * classpath wins, as in the JVM. Not thread-safe.
  */
final class ClassIndex(val sources: IndexedSeq[ClassSource], val syms: Symbols) {
  private var srcOf   = Array.fill(1024)(-1)
  private var parsed  = new Array[ClassInfo](1024)
  private val failed  = new java.util.HashMap[Int, ClassFileError]()
  private val parses  = new Array[Int](sources.length)
  private val loads   = new Array[Int](sources.length)
  private val members = sources.map(_ => ArrayBuffer.empty[ClassId])

  sources.iterator.zipWithIndex.foreach { case (src, i) =>
    src.classNames.foreach { name =>
      if (!name.endsWith("module-info") && !name.endsWith("package-info")) {
        val id = syms.classId(name)
        ensure(id.value)
        if (srcOf(id.value) < 0) {
          srcOf(id.value) = i
          members(i) += id
        }
      }
    }
  }

  private def ensure(i: Int): Unit =
    if (i >= srcOf.length) {
      val n   = math.max(i + 1, srcOf.length * 2)
      val old = srcOf.length
      srcOf = Arrays.copyOf(srcOf, n)
      Arrays.fill(srcOf, old, n, -1)
      parsed = Arrays.copyOf(parsed, n)
    }

  /** Classes provided by classpath entry `i`, excluding ones shadowed by earlier entries. */
  def classesIn(i: Int): scala.collection.IndexedSeq[ClassId] = members(i)

  private val bySuffix = new java.util.HashMap[String, Vector[ClassId]]

  /** The classes of the classpath whose internal name ends with `suffix` (`/kqueue/KQueue`), in classpath order; memoized. */
  def endingWith(suffix: String): Vector[ClassId] =
    bySuffix.computeIfAbsent(suffix, s => (0 until sources.length).iterator.flatMap(i => members(i)).filter(c => syms.className(c).endsWith(s)).toVector)

  private lazy val nestedByOuter: java.util.HashMap[String, Vector[ClassId]] = {
    val groups = new java.util.HashMap[String, scala.collection.mutable.Builder[ClassId, Vector[ClassId]]]
    (0 until sources.length).foreach { i =>
      members(i).foreach { c =>
        val name = syms.className(c)
        val cut  = name.lastIndexOf('$')
        if (cut > 0 && cut < name.length - 1) groups.computeIfAbsent(name.substring(0, cut), _ => Vector.newBuilder[ClassId]).+=(c)
      }
    }
    val out = new java.util.HashMap[String, Vector[ClassId]]
    groups.forEach((k, b) => out.put(k, b.result()))
    out
  }

  /** The classes nested directly in `outer` by name (`Outer\$Inner`): member classes, and anonymous and local ones too. Built once, on first use. */
  def nestedIn(outer: ClassId): Vector[ClassId] =
    nestedByOuter.getOrDefault(syms.className(outer), Vector.empty)

  /** Index of the classpath entry providing `id`, or -1 when absent. */
  def sourceIndex(id: ClassId): Int = if (id.value >= 0 && id.value < srcOf.length) srcOf(id.value) else -1

  /** Can the classpath name this class (see [[ClassSource.isExported]])? True if it is not known. */
  def isExported(id: ClassId): Boolean = !exists(id) || sources(srcOf(id.value)).isExported(syms.className(id))

  def exists(id: ClassId): Boolean = id.value >= 0 && id.value < srcOf.length && srcOf(id.value) >= 0

  /** The `ServiceLoader` providers declared on the classpath. Read once, on first use. */
  private[reachablelinkage] lazy val services: Services = Services.read(this)

  /** How many classes have been parsed from each classpath entry so far. */
  private[reachablelinkage] def parseCounts: IndexedSeq[Int] = parses.toIndexedSeq

  /** How many classes have been taken from a cached summary, per classpath entry. */
  private[reachablelinkage] def summaryLoads: IndexedSeq[Int] = loads.toIndexedSeq

  /** Why parsing failed, if it did. */
  def failure(id: ClassId): Option[ClassFileError] = Option(failed.get(id.value))

  /** Re-reads a class with line numbers recorded, for reporting. Not cached; null if unreadable. */
  private[reachablelinkage] def parseWithPositions(id: ClassId): ClassInfo =
    if (!exists(id)) null
    else sources(srcOf(id.value)).read(syms.className(id)).flatMap(ClassParser.parse(_, syms, ParseDetail.WithPositions).toOption).orNull

  /** The parsed class, or null when absent or unreadable. */
  private[reachablelinkage] def get(id: ClassId): ClassInfo =
    if (!exists(id)) null
    else {
      val hit = parsed(id.value)
      if (hit != null || failed.containsKey(id.value)) hit
      else {
        val src        = sources(srcOf(id.value))
        val summarized = src.summarized(syms.className(id), syms)
        if (summarized != null) {
          loads(srcOf(id.value)) += 1
          parsed(id.value) = summarized
          summarized
        } else
          src.read(syms.className(id)) match {
            case None        => failed.put(id.value, ClassFileError.Unavailable); null
            case Some(bytes) =>
              parses(srcOf(id.value)) += 1
              ClassParser.parse(bytes, syms) match {
                case Right(info) => parsed(id.value) = info; info
                case Left(err)   => failed.put(id.value, err); null
              }
          }
      }
    }
}
