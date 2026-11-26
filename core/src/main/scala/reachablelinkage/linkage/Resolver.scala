package reachablelinkage.linkage

import scala.annotation.tailrec
import reachablelinkage.classfile.{CallKind, ClassInfo, Keys}
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.{ClassId, DescId, NameId}

/** JVMS 5.4.3 member lookup at Class precision: the member must exist somewhere in the owner's superclass and superinterface closure. Results are memoized per
  * (owner, member, kind).
  */
private[linkage] final class Resolver(index: ClassIndex) {
  import Resolver._

  private val memo = new ResolutionMemo
  // MethodHandle.invoke and VarHandle accessors accept any descriptor (signature polymorphism).
  private val methodHandle = index.syms.classId("java/lang/invoke/MethodHandle")
  private val varHandle    = index.syms.classId("java/lang/invoke/VarHandle")

  def method(owner: ClassId, name: NameId, desc: DescId, call: CallKind): Resolution =
    if (owner == methodHandle || owner == varHandle) Resolution.Found(owner, Resolution.NoMember)
    else {
      val kind = call match {
        case CallKind.Static   => MemberKind.StaticMethod
        case CallKind.Instance => MemberKind.InstanceMethod
      }
      lookup(owner, Keys.of(name.value, desc.value), kind)
    }

  def field(owner: ClassId, name: NameId, desc: DescId): Resolution =
    lookup(owner, Keys.of(name.value, desc.value), MemberKind.Field)

  private def lookup(owner: ClassId, key: Long, kind: MemberKind): Resolution =
    memo.get(owner.value, key, kind.ordinal) match {
      case null =>
        val r = search(owner, key, kind, root = true, new java.util.HashSet[Int]())
        memo.put(owner.value, key, kind.ordinal, r)
        r
      case hit => hit
    }

  /** @param root
    *   true for the class named by the reference. Static interface methods are not inherited (JVMS 5.4.3.3), so they only count when the interface is the root.
    */
  private def search(id: ClassId, key: Long, kind: MemberKind, root: Boolean, seen: java.util.HashSet[Int]): Resolution =
    if (!seen.add(id.value)) Resolution.NotFound
    else
      index.get(id) match {
        case null => Resolution.Incomplete(id)
        case info =>
          val inheritedInterface = !root && info.isInterface
          val here               = memberIn(info, key, kind)
          if (here != Absent && !(kind == MemberKind.StaticMethod && inheritedInterface)) Resolution.Found(id, here)
          else if (
            kind != MemberKind.Field && !(kind == MemberKind.InstanceMethod && inheritedInterface) &&
            memberIn(info, key, opposite(kind)) != Absent
          ) Resolution.KindMismatch
          else {
            // The superclass first, then each interface; stop as soon as one branch finds it.
            @tailrec def interfaces(i: Int, best: Resolution): Resolution =
              if (i >= info.interfaces.length || best.isFound) best
              else {
                val r = search(info.interfaces(i), key, kind, root = false, seen)
                interfaces(i + 1, if (rank(r) > rank(best)) r else best)
              }
            interfaces(0, if (info.superName.isNone) Resolution.NotFound else search(info.superName, key, kind, root = false, seen))
          }
      }
}

object Resolver {
  private val Absent: Int = -1

  /** Index of the member declared by `info` (into its `fields` or `methods`), or [[Absent]]. */
  private def memberIn(info: ClassInfo, key: Long, kind: MemberKind): Int = kind match {
    case MemberKind.Field          => info.fieldIndex(key)
    case MemberKind.StaticMethod   => info.methodIndex(key, CallKind.Static)
    case MemberKind.InstanceMethod => info.methodIndex(key, CallKind.Instance)
  }

  /** The method kind a call site of `kind` would wrongly have hit. */
  private def opposite(kind: MemberKind): MemberKind = kind match {
    case MemberKind.StaticMethod => MemberKind.InstanceMethod
    case _                       => MemberKind.StaticMethod
  }

  /** Which outcome wins when several superclass/interface branches disagree. */
  private def rank(r: Resolution): Int = r match {
    case Resolution.Found(_, _)   => 4
    case Resolution.KindMismatch  => 3
    case Resolution.Incomplete(_) => 2
    case Resolution.NotFound      => 1
  }
}
