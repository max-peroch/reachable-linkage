package reachablelinkage.linkage

import scala.annotation.tailrec
import java.util.BitSet

import reachablelinkage.classfile.{ClassInfo, Probe}
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.ClassId

/** Class-level reachability: the transitive closure of supertypes and referenced classes, plus the `ServiceLoader` providers nothing names: a provider of a
  * library interface starts once the interface is reached (loading it means naming it), and a provider of a JDK interface always starts (JDK code loads those,
  * and the JDK is not walked). A string constant that names a class on the classpath (`Class.forName("x.Y")`) counts as a reference to it.
  */
private[linkage] object Reachability {

  /** @param traverse
    *   indices of the classpath entries the walk may enter (a bit per entry)
    * @return
    *   a bit per class id that is reachable from `roots`
    */
  def classes(index: ClassIndex, rules: KeepRules, roots: Iterable[ClassId], traverse: BitSet): BitSet =
    classes(index, roots, traverse, new FrameworkTargets(index, rules, Generated.of(index, rules), traverse))

  /** As above, with the framework targets already prepared (the call graph asks the same questions). */
  def classes(index: ClassIndex, roots: Iterable[ClassId], traverse: BitSet, targets: FrameworkTargets): BitSet = {
    val walk = new Walk(index, traverse, targets)
    walk.start(roots)
    walk.drain()
    walk.seen
  }

  /** One walk: the classes seen so far and the ones still to read. Each method says one thing a reached class makes reachable. */
  private final class Walk(index: ClassIndex, traverse: BitSet, targets: FrameworkTargets) {
    val seen             = new BitSet()
    private val stack    = new IntList
    private val services = index.services

    private def isTraversed(id: ClassId): Boolean = {
      val source = index.sourceIndex(id)
      source >= 0 && traverse.get(source)
    }

    private def push(id: Int): Unit =
      if (id >= 0 && !seen.get(id) && isTraversed(ClassId(id))) {
        seen.set(id)
        stack.push(id)
        pushProvidersOf(ClassId(id))
      }

    private def pushClass(c: ClassId): Unit = push(c.value)

    private def pushProvidersOf(service: ClassId): Unit = services.providersOf(service).foreach(pushClass)

    /** The roots, and the providers of the JDK's own services (JDK code loads those and is not walked). */
    def start(roots: Iterable[ClassId]): Unit = {
      roots.foreach(pushClass)
      services.interfaces.foreach(service => if (index.exists(service) && !isTraversed(service)) pushProvidersOf(service))
    }

    @tailrec def drain(): Unit =
      if (!stack.isEmpty) {
        val current = ClassId(stack.pop())
        index.get(current) match {
          case null => ()
          case info => pushEverythingReachedFrom(current, info)
        }
        drain()
      }

    private def pushEverythingReachedFrom(current: ClassId, info: ClassInfo): Unit = {
      pushSupertypes(info)
      info.classRefs.foreach(push)
      pushFrameworkTargets(current, info)
      pushClassesNamedByConstants(info)
      pushMethodAndFieldOwners(info)
    }

    private def pushSupertypes(info: ClassInfo): Unit = {
      push(info.superName.value)
      info.interfaces.foreach(pushClass)
    }

    /** What the framework rules make a framework load from this class, unless a failing condition makes it ignore the class. */
    private def pushFrameworkTargets(current: ClassId, info: ClassInfo): Unit =
      targets.eachLoaded(current, info)(built = pushClass, member = pushClass)

    /** Constant-name reflection: a string naming a class on the classpath loads it. */
    private def pushClassesNamedByConstants(info: ClassInfo): Unit =
      info.probes.foreach {
        case Probe.ClassName(name)                          => pushNamedClass(index.syms.findClass(name))
        case Probe.NameTail(suffix)                         => index.endingWith(suffix).foreach(pushNamedClass)
        case Probe.MethodName(_) | Probe.LambdaInterface(_) => ()
      }

    /** A class a framework ignores unless its conditions hold (`RequiresClasses`) is not loaded by a name either. */
    private def pushNamedClass(id: ClassId): Unit =
      if (!id.isNone && isEnabledOrUnparsed(id)) pushClass(id)

    private def isEnabledOrUnparsed(id: ClassId): Boolean =
      index.get(id) match {
        case null => true
        case info => targets.considers(info)
      }

    private def pushMethodAndFieldOwners(info: ClassInfo): Unit = {
      pushEvery(info.methodRefs, 4)
      pushEvery(info.fieldRefs, 3)
    }

    /** Pushes the first int of each entry of `stride` ints. */
    private def pushEvery(entries: Array[Int], stride: Int): Unit = {
      @tailrec def from(i: Int): Unit =
        if (i < entries.length) {
          push(entries(i))
          from(i + stride)
        }
      from(0)
    }
  }
}
