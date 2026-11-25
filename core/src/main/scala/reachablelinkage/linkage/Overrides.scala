package reachablelinkage.linkage

import scala.annotation.tailrec

import reachablelinkage.classfile.{CallKind, ClassInfo, Keys}
import reachablelinkage.classfile.AccessFlags.*
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.ClassId

/** Methods a class overrides although a superclass now declares them `final`: the verifier rejects the class when it loads. The nearest superclass that
  * declares the method (instance, not private) decides, as in the JVM; a package-private `final` method is only overridden from its own package.
  */
private[linkage] final class Overrides(index: ClassIndex, access: Access):

  private val syms     = index.syms
  private val initName = syms.nameId("<init>")
  private val clinit   = syms.nameId("<clinit>")

  /** The final methods `info` overrides, as (declaring class, index of the method in `info`). Interfaces override nothing. */
  def finalOverridden(info: ClassInfo): Vector[(ClassId, Int)] =
    if info.isInterface then Vector.empty
    else
      val found                       = Vector.newBuilder[(ClassId, Int)]
      @tailrec def from(m: Int): Unit =
        if m < info.methods.length then
          val member = info.methods(m)
          if !member.access.has(Static | Private) && member.name != initName && member.name != clinit then
            nearestDeclaration(info.superName, Keys.of(member.name.value, member.desc.value)).foreach: (declaring, flags) =>
              if flags.has(Final) && (flags.has(Public | Protected) || access.samePackage(info.id, declaring)) then found += ((declaring, m))
          from(m + 1)
      from(0)
      found.result()

  /** The first superclass, from `start` up, that declares an instance method with this key, and its flags. */
  @tailrec private def nearestDeclaration(start: ClassId, key: Long): Option[(ClassId, Int)] =
    if start.isNone then None
    else
      index.get(start) match
        case null => None
        case up   =>
          val at = up.methodIndex(key, CallKind.Instance)
          if at >= 0 && !up.methods(at).access.has(Private) then Some((start, up.methods(at).access))
          else nearestDeclaration(up.superName, key)
