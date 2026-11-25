package reachablelinkage.linkage

import java.util.BitSet
import scala.annotation.tailrec

import reachablelinkage.classfile.Keys
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.{ClassId, DescId, NameId}

/**
 * The methods that the checked classes name in a call, and the method names they look up by a constant (`getMethod("run")`). It is what a
 * check without a call graph can still say about an abstract method nothing implements: if no class names it, nothing calls it.
 * Built once, over the classes the walk reached.
 */
private[linkage] final class NamedMethods private (called: LongSet, looked: BitSet):

  /** Does some reached class contain a call to a method with this name and descriptor (on any owner)? */
  def isCalled(name: NameId, desc: DescId): Boolean = called.contains(Keys.of(name.value, desc.value))

  /** Does some reached class look up a method of this name by a constant? */
  def isLookedUp(name: NameId): Boolean = name.value >= 0 && looked.get(name.value)

private[linkage] object NamedMethods:
  def of(index: ClassIndex, reached: BitSet): NamedMethods =
    val called = new LongSet
    val looked = new BitSet
    @tailrec def classes(r: Int): Unit =
      if r >= 0 then
        index.get(ClassId(r)) match
          case null => ()
          case info =>
            @tailrec def each(i: Int): Unit =
              if i < info.methodRefs.length then
                called.add(Keys.of(info.methodRefs(i + 1), info.methodRefs(i + 2)))
                each(i + 4)
            each(0)
            info.reflectedMethodNames.foreach(n => looked.set(index.syms.nameId(n).value))
        classes(reached.nextSetBit(r + 1))
    classes(reached.nextSetBit(0))
    NamedMethods(called, looked)
