package reachablelinkage.linkage

import java.util.BitSet
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, HashSet}

import reachablelinkage.classfile.{ClassInfo, Probe}
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.ClassId

/** The missing classes that reached code names in a string constant, and, when the library of a class it asks for is not on the classpath at all, the other
  * missing classes of that library. Code that is about to use an optional class tends to ask whether it is there first, by that name, and that asks about its
  * library.
  *
  * The library is the tree under any prefix of the package asked about (`net/sf/jsqlparser` for `net/sf/jsqlparser/parser`, or `org/apache/logging/log4j/core`
  * when only log4j-api is there). If no class of that tree is on the classpath, the library is absent as a whole and any other missing class of it is part of
  * the same optional dependency. A tree that is still partly there is a library whose version changed, which is a real break and is never hidden.
  */
private[linkage] object ProbedClasses:

  /** The ids of the missing classes among `findings` that are probed. */
  def of(index: ClassIndex, reachable: BitSet, findings: ArrayBuffer[Finding]): BitSet =
    val missing = missingClasses(findings)
    val probed  = new BitSet()
    if !missing.isEmpty then
      val askedPackages = HashSet.empty[String]
      eachReachedClass(index, reachable): info =>
        info.probes.foreach:
          case Probe.ClassName(name) => noteProbe(index, name, missing, probed, askedPackages)
          case _                     => ()
      markMissingClassesOfAbsentLibraries(index, askedPackages.toSet, missing, probed)
    probed

  private def missingClasses(findings: ArrayBuffer[Finding]): BitSet =
    val missing = new BitSet()
    findings.foreach(f =>
      f.conflict match
        case Conflict.MissingClass(m, _) => missing.set(m.value)
        case _                           => ()
    )
    missing

  /** A name that is asked for and is not there is a probe, whether or not anything also refers to the class; its package is a library asked about. */
  private def noteProbe(index: ClassIndex, name: String, missing: BitSet, probed: BitSet, askedPackages: HashSet[String]): Unit =
    val id = index.syms.findClass(name)
    if id.isNone || !index.exists(id) then
      askedPackages ++= packageOf(name)
      if !id.isNone && missing.get(id.value) then probed.set(id.value)

  private def markMissingClassesOfAbsentLibraries(index: ClassIndex, askedPackages: Set[String], missing: BitSet, probed: BitSet): Unit =
    if askedPackages.nonEmpty then
      val roots  = askedPackages.flatMap(libraryRootsOf)
      val absent = roots.diff(rootsWithAClass(index, roots))
      if absent.nonEmpty then eachSetBit(missing)(m => if isInAbsentLibrary(index.syms.className(ClassId(m)), absent) then probed.set(m))

  /** `a/b/c` is in the library trees `a`, `a/b` and `a/b/c`. */
  private def libraryRootsOf(pkg: String): List[String] =
    @tailrec def step(start: Int, acc: List[String]): List[String] =
      val next = pkg.indexOf('/', start)
      if next < 0 then pkg :: acc else step(next + 1, pkg.substring(0, next) :: acc)
    step(0, Nil)

  /** Which of the `roots` have at least one class of their tree on the classpath. */
  private def rootsWithAClass(index: ClassIndex, roots: Set[String]): Set[String] =
    val present = HashSet.empty[String]
    index.sources.foreach(source => source.classNames.foldLeft("")((last, name) => noteRootsOfPackage(name, last, roots, present)))
    present.toSet

  /** Notes the roots of `name`'s package, and returns that package; a name in the package just before (`last`) says nothing new and costs no allocation. */
  private def noteRootsOfPackage(name: String, last: String, roots: Set[String], present: HashSet[String]): String =
    val slash = name.lastIndexOf('/')
    if slash <= 0 || isInPackage(name, slash, last) then last
    else
      val pkg = name.substring(0, slash)
      libraryRootsOf(pkg).foreach(r => if roots.contains(r) then present += r)
      pkg

  private def isInPackage(name: String, slash: Int, pkg: String): Boolean = slash == pkg.length && name.startsWith(pkg)

  private def isInAbsentLibrary(name: String, absent: Set[String]): Boolean =
    packageOf(name).exists(pkg => libraryRootsOf(pkg).exists(absent.contains))

  private def packageOf(internalName: String): Option[String] =
    val slash = internalName.lastIndexOf('/')
    if slash > 0 then Some(internalName.substring(0, slash)) else None

  private def eachReachedClass(index: ClassIndex, reachable: BitSet)(f: ClassInfo => Unit): Unit =
    eachSetBit(reachable): c =>
      index.get(ClassId(c)) match
        case null => ()
        case info => f(info)

  private def eachSetBit(bits: BitSet)(f: Int => Unit): Unit =
    @tailrec def from(i: Int): Unit =
      if i >= 0 then
        f(i)
        from(bits.nextSetBit(i + 1))
    from(bits.nextSetBit(0))
