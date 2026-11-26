package reachablelinkage.linkage

import java.util.BitSet

import reachablelinkage.classfile.ClassInfo
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.ClassId

/** The classes a framework loads from a class that was reached, and whether it considers the class at all: the one place the class walk and the call graph both
  * ask, so the two cannot disagree. The walk only marks the targets seen; the graph also builds them through their constructors.
  */
private[linkage] final class FrameworkTargets(index: ClassIndex, rules: KeepRules, generated: Generated, traverse: BitSet) {
  private val present: ClassId => Boolean = index.exists

  /** Annotation types of the traversed entries are read for their own annotations; the JDK's are not walked. */
  private val traversedInfo: ClassId => ClassInfo = id => if (isTraversed(id)) index.get(id) else null

  private def isTraversed(id: ClassId): Boolean = {
    val source = index.sourceIndex(id)
    source >= 0 && traverse.get(source)
  }

  /** Does the framework consider this class, or does a condition naming a class that is not on the classpath make it ignore it? */
  def considers(info: ClassInfo): Boolean = rules.enabled(info, -1, present)

  /** What the framework loads because of `current`, asking once whether it considers the class: `built` gets the classes it builds, `member` the member classes
    * it goes through. Nothing, for a class it ignores.
    */
  def eachLoaded(current: ClassId, info: ClassInfo)(built: ClassId => Unit, member: ClassId => Unit): Unit =
    if (considers(info)) {
      eachBuiltOfConsidered(current, info)(built)
      eachMemberClassOfConsidered(current, info)(member)
    }

  /** Calls `f` with each class the framework builds because of `current`: classes its annotations name (`@Import(Impl.class)`, `@ImplementedBy`), those named
    * on the annotation types they use, however deeply, and its generated companions (`Foo_$logger`). For a class that [[considers]] says is considered: the
    * caller has asked, and does not ask again.
    */
  def eachBuiltOfConsidered(current: ClassId, info: ClassInfo)(f: ClassId => Unit): Unit = {
    rules.referencedClasses(info)(f)
    rules.metaReferencedClasses(info, traversedInfo)(f)
    generated.companionsOf(current).foreach(f)
  }

  /** Calls `f` with each member class of `current` the framework goes through (it processes `Outer$Inner` configurations of a class it builds); for a
    * considered class.
    */
  def eachMemberClassOfConsidered(current: ClassId, info: ClassInfo)(f: ClassId => Unit): Unit =
    if (rules.needsConstructors(info)) index.nestedIn(current).foreach(f)
}
