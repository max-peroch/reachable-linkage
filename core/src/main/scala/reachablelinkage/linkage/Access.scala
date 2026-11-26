package reachablelinkage.linkage

import java.util.BitSet

import reachablelinkage.classfile.ClassInfo
import reachablelinkage.classfile.AccessFlags._
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.ClassId

/** Who may use what (JVMS 5.4.4). A class is usable from its own package, or from anywhere if public. A public member is usable by everyone; a protected one
  * from its package or a subclass; a package-private one from its package; a private one only within its nest.
  *
  * Where the JVM is more precise, this is more permissive, so it never reports a call that works: the nest is approximated by the outermost class name, and the
  * receiver-type rule for protected access is ignored. Module encapsulation (a public class in an unexported JDK package) is not modelled.
  */
private[linkage] final class Access(index: ClassIndex, exportedPackages: Set[String] = Set.empty) {

  private val syms     = index.syms
  private val packages = new Slots[String]

  /** May `from` name `target`? True when `target` cannot be read: that is reported elsewhere. */
  def classAccessible(from: ClassId, target: ClassId): Boolean =
    from == target || {
      index.get(target) match {
        case null => true
        case info => info.access.has(Public) || samePackage(from, target)
      }
    }

  /** Does the class's module export its package to the classpath, or does a JVM flag say it is exported? */
  def classExported(target: ClassId): Boolean =
    index.isExported(target) || exportedPackages.contains(packageOf(target).replace('/', '.'))

  def methodAccessible(from: ClassInfo, declaring: ClassId, member: Int): Boolean =
    index.get(declaring) match {
      case null => true
      case d    => accessible(from, d, d.methods(member).access)
    }

  def fieldAccessible(from: ClassInfo, declaring: ClassId, member: Int): Boolean =
    index.get(declaring) match {
      case null => true
      case d    => accessible(from, d, d.fields(member).access)
    }

  private def accessible(from: ClassInfo, declaring: ClassInfo, flags: Int): Boolean =
    if (from.id == declaring.id || flags.has(Public)) true
    else if (flags.has(Private)) sameNest(from.id, declaring.id)
    else if (flags.has(Protected)) samePackage(from.id, declaring.id) || isSubclass(from, declaring.id)
    else samePackage(from.id, declaring.id)

  private def packageOf(c: ClassId): String =
    packages.get(c.value) match {
      case null =>
        val name = syms.className(c)
        val pkg  = name.substring(0, math.max(name.lastIndexOf('/'), 0))
        packages.put(c.value, pkg)
        pkg
      case cached => cached
    }

  private[linkage] def samePackage(a: ClassId, b: ClassId): Boolean = a == b || packageOf(a) == packageOf(b)

  /** `Outer$Inner$1` and `Outer` are nestmates; so is anything sharing the part before the first `$`. */
  private def outerOf(c: ClassId): String = {
    val name   = syms.className(c)
    val simple = name.substring(name.lastIndexOf('/') + 1)
    val dollar = simple.indexOf('$')
    if (dollar > 0) simple.substring(0, dollar) else simple
  }

  private def sameNest(a: ClassId, b: ClassId): Boolean =
    a == b || (samePackage(a, b) && outerOf(a) == outerOf(b))

  private def isSubclass(from: ClassInfo, target: ClassId): Boolean = {
    val seen                        = new BitSet()
    def visit(id: ClassId): Boolean =
      !id.isNone && !seen.get(id.value) && {
        seen.set(id.value)
        id == target || {
          index.get(id) match {
            case null => false
            case info => visit(info.superName) || info.interfaces.exists(visit)
          }
        }
      }
    visit(from.superName) || from.interfaces.exists(visit)
  }
}
