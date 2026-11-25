package reachablelinkage.linkage

import scala.annotation.tailrec
import java.util.BitSet

import reachablelinkage.classfile.{CallKind, ClassInfo, Keys, Member}
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.classfile.AccessFlags.*
import reachablelinkage.symbols.ClassId

/**
 * Finds the abstract methods a concrete class inherits but nothing implements. Method selection
 * (JVMS 5.4.6) takes the first declaration of the method along the class and its superclasses; if
 * that one is abstract, the call fails even if an interface has a default. Only when no class in
 * the chain declares it does an interface default count.
 */
private[linkage] final class Implementations(index: ClassIndex):

  /**
   * @return for each unimplemented abstract method, the ancestor that declares it and the method.
   *         Empty for an abstract class or interface, and when an ancestor is missing from the
   *         classpath (the missing class is reported on its own).
   */
  def unimplemented(c: ClassInfo): Vector[(ClassId, Member)] =
    if c.isInterface || c.isAbstract then Vector.empty
    else
      ancestors(c) match
        case null => Vector.empty
        case all =>
          val seen = new LongSet
          val out  = Vector.newBuilder[(ClassId, Member)]
          all.foreach: ancestor =>
            index.get(ancestor) match
              case null => ()
              case a =>
                a.abstractMethods.foreach: i =>
                  val m   = a.methods(i)
                  val key = Keys.of(m.name.value, m.desc.value)
                  if seen.add(key) && !implemented(c, key, all) then out += ((ancestor, m))
          out.result()

  /** Every superclass and superinterface, or null if any of them cannot be read. */
  private def ancestors(c: ClassInfo): Array[ClassId] =
    val seen     = new BitSet()
    val out      = scala.collection.mutable.ArrayBuffer.empty[ClassId]
    var complete = true
    def visit(id: ClassId): Unit =
      if !id.isNone && !seen.get(id.value) then
        seen.set(id.value)
        index.get(id) match
          case null => complete = false
          case info =>
            out += id
            visit(info.superName)
            info.interfaces.foreach(visit)
    visit(c.superName)
    c.interfaces.foreach(visit)
    if complete then out.toArray else null

  private def implemented(c: ClassInfo, key: Long, all: Array[ClassId]): Boolean =
    // The first declaration up the class chain decides: abstract fails, anything else is the implementation.
    @tailrec def up(current: ClassInfo | Null): Boolean =
      if current == null then hasDefault(key, all)
      else
        val info = current.nn
        val at   = info.methodIndex(key, CallKind.Instance)
        if at >= 0 && !info.methods(at).access.has(Private) then !info.methods(at).access.has(Abstract)
        else up(if info.superName.isNone then null else index.get(info.superName))
    up(c)

  /** Does some interface among the ancestors supply a default for the method? */
  private def hasDefault(key: Long, all: Array[ClassId]): Boolean =
    all.exists: a =>
      index.get(a) match
        case null => false
        case info =>
          info.isInterface && {
            val at = info.methodIndex(key, CallKind.Instance)
            at >= 0 && !info.methods(at).access.has(Abstract)
          }
