package reachablelinkage.linkage

import scala.annotation.tailrec
import java.util.BitSet

import reachablelinkage.classfile.{ClassInfo, Keys}
import reachablelinkage.classfile.AnnotationEntries._
import reachablelinkage.linkage.RuleNames._
import reachablelinkage.symbols.{ClassId, Symbols}

/** The engine behind [[Keep]] rules: the annotation types and supertypes they name, as bits over class ids, and the questions the call graph and the class walk
  * ask of a class that was reached.
  */
private[linkage] final class KeepRules private (
    keepMembers: BitSet,
    valueAnnotations: BitSet,
    noArgMarkers: BitSet,
    constructorMarkers: BitSet,
    conditionMarkers: BitSet,
    overrideTypes: BitSet,
    beanCalls: LongSet,
    builtCalls: LongSet,
    constructedCalls: LongSet,
    val namedFiles: Set[String],
    val generatedSuffixes: Set[String]
) {

  /** Does a framework read the generic types of member `member` of `kind` (0 a method or constructor, 1 a field)? One that a `Members` rule fires for (an
    * injected constructor, a `@Provides` method), and every member of a class a marker rule makes Jackson-style. A framework loads the erased types of all
    * members when it lists them, but only asks for the generic types of the members it uses, so a type argument of an unrelated method is never loaded.
    */
  def readsGenericTypes(info: ClassInfo): (Int, Int) => Boolean =
    if (needsNoArgConstructor(info) || needsConstructors(info)) (_, _) => true
    else {
      val methods                     = new BitSet
      val fields                      = new BitSet
      val a                           = info.annotations
      @tailrec def from(i: Int): Unit =
        if (i < a.length) {
          if (isKeptMember(a, i)) if (a.isOnMethod(i)) methods.set(a.memberAt(i)) else if (a.isOnField(i)) fields.set(a.memberAt(i))
          from(i + Width)
        }
      from(0)
      (kind, member) => member >= 0 && (if (kind == 0) methods.get(member) else fields.get(member))
    }

  /** Is there any rule about a class passed to a call? */
  def hasArgumentRules: Boolean = !beanCalls.isEmpty || !builtCalls.isEmpty || !constructedCalls.isEmpty

  /** Calls `f` with each class literal `info` passes to a call a `BeanArguments` rule names. */
  def beanTargets(info: ClassInfo)(f: ClassId => Unit): Unit = literalsTo(info, beanCalls)(f)

  /** Calls `f` with each class literal `info` passes to a call a `BuiltArguments` rule names. */
  def builtTargets(info: ClassInfo)(f: ClassId => Unit): Unit = literalsTo(info, builtCalls)(f)

  /** Calls `f` with each class literal `info` passes to a call a `ConstructedArguments` rule names. */
  def constructedTargets(info: ClassInfo)(f: ClassId => Unit): Unit = literalsTo(info, constructedCalls)(f)

  private def literalsTo(info: ClassInfo, calls: LongSet)(f: ClassId => Unit): Unit =
    if (!calls.isEmpty) {
      val a                           = info.literalArgs
      @tailrec def from(i: Int): Unit =
        if (i < a.length) {
          if (calls.contains(Keys.of(a(i), a(i + 1)))) f(ClassId(a(i + 2)))
          from(i + 3)
        }
      from(0)
    }

  /** Is there any `Overrides` rule? */
  def hasOverrideRules: Boolean = !overrideTypes.isEmpty

  /** Does a rule say a framework calls the overrides of subtypes of class `c`? */
  def isOverrideType(c: Int): Boolean = c >= 0 && overrideTypes.get(c)

  // ---- the small questions about one entry of the annotation table ----

  private def isKeptMember(a: Array[Int], i: Int): Boolean           = (a.isOnMethod(i) || a.isOnField(i)) && keepMembers.get(a.annotationAt(i))
  private def isKeptMethod(a: Array[Int], i: Int): Boolean           = a.isOnMethod(i) && keepMembers.get(a.annotationAt(i))
  private def marksNoArgConstructor(a: Array[Int], i: Int): Boolean  = (a.isOnClass(i) || a.isOnMethod(i)) && noArgMarkers.get(a.annotationAt(i))
  private def marksAllConstructors(a: Array[Int], i: Int): Boolean   = a.isOnClass(i) && constructorMarkers.get(a.annotationAt(i))
  private def isReferencedClassValue(a: Array[Int], i: Int): Boolean = a.isClassValue(i) && valueAnnotations.get(a.annotationAt(i))

  /** Is entry `i` a condition (`@ConditionalOnClass`) on `member` (-1 for the class) that names a class? */
  private def isConditionOn(a: Array[Int], i: Int, member: Int): Boolean =
    a.isClassNamedInside(i) && a.memberAt(i) == member && conditionMarkers.get(a.annotationAt(i))

  private def allConditionsHold(a: Array[Int], member: Int, present: ClassId => Boolean): Boolean = {
    @tailrec def from(i: Int): Boolean =
      i >= a.length || ((!isConditionOn(a, i, member) || present(ClassId(a.valueAt(i)))) && from(i + Width))
    from(0)
  }

  /** Does a framework consider the class (`member` -1) or its method `member`? Not if it carries a `RequiresClasses` annotation naming a class that is not
    * `present`. Only code a framework roots is gated: a class something references is loaded whatever it says.
    */
  def enabled(info: ClassInfo, member: Int, present: ClassId => Boolean): Boolean =
    conditionMarkers.isEmpty || allConditionsHold(info.annotations, member, present)

  /** Calls `f` with the index of each method of `info` that a rule keeps and the framework considers, and returns how many there were. */
  def keptMethods(info: ClassInfo, present: ClassId => Boolean)(f: Int => Unit): Int = {
    val a                                     = info.annotations
    @tailrec def from(i: Int, kept: Int): Int =
      if (i >= a.length) kept
      else if (isKeptMethod(a, i) && enabled(info, a.memberAt(i), present)) {
        f(a.memberAt(i))
        from(i + Width, kept + 1)
      } else from(i + Width, kept)
    from(0, 0)
  }

  /** Does a framework reflect on this class? It does if the class has a method, constructor or field a rule keeps, or an annotation that makes it constructed
    * by a no-argument constructor or by any constructor. Reflecting over a class loads the types of all its members. One pass over the annotations.
    */
  def reflectsOn(info: ClassInfo): Boolean = {
    val a                              = info.annotations
    @tailrec def from(i: Int): Boolean =
      i < a.length && (isKeptMember(a, i) || marksNoArgConstructor(a, i) || marksAllConstructors(a, i) || from(i + Width))
    from(0)
  }

  /** Calls `f` with each class a value annotation of `info` (or of one of its methods) names. */
  def referencedClasses(info: ClassInfo)(f: ClassId => Unit): Unit = {
    val a                           = info.annotations
    @tailrec def from(i: Int): Unit =
      if (i < a.length) {
        if (isReferencedClassValue(a, i)) f(ClassId(a.valueAt(i)))
        from(i + Width)
      }
    from(0)
  }

  // ---- annotations of annotations ----

  private val metaTargets = new java.util.HashMap[Int, Array[Int]]

  /** The annotation types on the annotation type `info` itself, put in front of `onto`. */
  private def annotationTypesOnClass(info: ClassInfo, onto: List[Int]): List[Int] = {
    val a                                                = info.annotations
    @tailrec def from(i: Int, acc: List[Int]): List[Int] =
      if (i >= a.length) acc
      else from(i + Width, if (a.isOnClass(i)) a.annotationAt(i) :: acc else acc)
    from(0, onto)
  }

  /** The classes named by value annotations on `start`, on the annotation types that annotate it, and so on; each annotation type is read once, so a cycle
    * ends.
    */
  private def valuesReachableFrom(start: Int, infoOf: ClassId => ClassInfo): Array[Int] = {
    val seen                                  = new BitSet
    val found                                 = Array.newBuilder[Int]
    @tailrec def drain(todo: List[Int]): Unit = todo match {
      case Nil                                        => ()
      case annotation :: rest if seen.get(annotation) => drain(rest)
      case annotation :: rest                         =>
        seen.set(annotation)
        infoOf(ClassId(annotation)) match {
          case null           => drain(rest)
          case annotationInfo =>
            referencedClasses(annotationInfo)(c => found += c.value)
            drain(annotationTypesOnClass(annotationInfo, rest))
        }
    }
    drain(List(start))
    found.result().distinct
  }

  /** The classes that a value annotation names on annotation type `ann` or, in turn, on the annotation types that annotate it (`@SpringBootApplication` is
    * `@EnableAutoConfiguration`, which is `@Import(AutoConfigurationImportSelector.class)`). Memoized whole: a walk is complete before it is remembered, so an
    * annotation cycle cannot leave a partial answer behind.
    */
  private def metaOf(ann: Int, infoOf: ClassId => ClassInfo): Array[Int] = {
    val known = metaTargets.get(ann)
    if (known != null) known
    else {
      val found = valuesReachableFrom(ann, infoOf)
      metaTargets.put(ann, found)
      found
    }
  }

  /** Calls `f` with each class that a value annotation names on one of the annotation types `info` uses, however deeply they are annotated
    * (`@ConditionalOnProperty` is itself annotated `@Conditional(OnPropertyCondition.class)`, so a class using it gets that condition built by the framework).
    * `infoOf` reads an annotation type.
    */
  def metaReferencedClasses(info: ClassInfo, infoOf: ClassId => ClassInfo)(f: ClassId => Unit): Unit =
    if (!valueAnnotations.isEmpty) {
      val a                           = info.annotations
      @tailrec def from(i: Int): Unit =
        if (i < a.length) {
          if (a.isAnnotationUse(i)) metaOf(a.annotationAt(i), infoOf).foreach(t => f(ClassId(t)))
          from(i + Width)
        }
      from(0)
    }

  /** Does a rule say a framework builds this class through any of its constructors (an annotation on the class)? */
  def needsConstructors(info: ClassInfo): Boolean = {
    val a                              = info.annotations
    @tailrec def from(i: Int): Boolean = i < a.length && (marksAllConstructors(a, i) || from(i + Width))
    from(0)
  }

  /** Does a rule give this class its no-argument constructor (an annotation on it or on one of its methods)? */
  def needsNoArgConstructor(info: ClassInfo): Boolean = {
    val a                              = info.annotations
    @tailrec def from(i: Int): Boolean = i < a.length && (marksNoArgConstructor(a, i) || from(i + Width))
    from(0)
  }
}

private[linkage] object KeepRules {

  /** The rules, with their annotation types interned in `syms`. */
  def from(syms: Symbols, rules: Seq[Keep]): KeepRules = {
    def bits(names: Seq[ClassName]): BitSet = {
      val b = new BitSet
      names.foreach(n => b.set(syms.classId(n.value).value))
      b
    }
    // `owner/Name.method`: the owner's class id and the method's name id, as one key.
    def calls(names: Seq[CallName]): LongSet = {
      val set = new LongSet
      names.foreach { call =>
        val n   = call.value
        val dot = n.lastIndexOf('.')
        if (dot > 0) set.add(Keys.of(syms.classId(n.substring(0, dot)).value, syms.nameId(n.substring(dot + 1)).value))
      }
      set
    }
    new KeepRules(
      keepMembers = bits(rules.flatMap { case Keep.Members(a) => a.toSeq; case _ => Nil }),
      valueAnnotations = bits(rules.flatMap { case Keep.ClassValues(a) => a.toSeq; case _ => Nil }),
      noArgMarkers = bits(rules.flatMap { case Keep.NoArgConstructor(a) => a.toSeq; case _ => Nil }),
      constructorMarkers = bits(rules.flatMap { case Keep.Constructors(a) => a.toSeq; case _ => Nil }),
      conditionMarkers = bits(rules.flatMap { case Keep.RequiresClasses(a) => a.toSeq; case _ => Nil }),
      overrideTypes = bits(rules.flatMap { case Keep.Overrides(t) => t.toSeq; case _ => Nil }),
      beanCalls = calls(rules.flatMap { case Keep.BeanArguments(c) => c.toSeq; case _ => Nil }),
      builtCalls = calls(rules.flatMap { case Keep.BuiltArguments(c) => c.toSeq; case _ => Nil }),
      constructedCalls = calls(rules.flatMap { case Keep.ConstructedArguments(c) => c.toSeq; case _ => Nil }),
      namedFiles = rules.flatMap { case Keep.ClassesNamedIn(f) => f.toSeq.map(_.value); case _ => Nil }.toSet,
      generatedSuffixes = rules.flatMap { case Keep.GeneratedClasses(s) => s.toSeq.map(_.value); case _ => Nil }.toSet
    )
  }
}
