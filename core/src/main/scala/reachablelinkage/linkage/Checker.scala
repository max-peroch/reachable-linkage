package reachablelinkage.linkage

import scala.annotation.tailrec
import java.util.BitSet
import scala.collection.mutable.ArrayBuffer

import org.objectweb.asm.Type
import reachablelinkage.classfile.{CallRef, ClassFileError, ClassInfo}
import reachablelinkage.classfile.AccessFlags._
import reachablelinkage.classpath.{ClassIndex, NamedInResources}
import reachablelinkage.symbols.{ClassId, DescId, NameId}

/** Class-precision linkage check: every reference made by a reachable, checked class must resolve. */
object Checker {

  /** The distinct conflicts, in order of first occurrence. */
  def check(index: ClassIndex, scope: Scope): Vector[Conflict] =
    findings(index, scope, Precision.Class).map(_.conflict).distinct

  /** `findings` at one precision, the other options as they are by default. */
  def findings(index: ClassIndex, scope: Scope, precision: Precision): Vector[Finding] =
    findings(index, scope, CheckOptions(precision))

  /** One finding per offending reference, so a report can say where each one is. The set of conflicts does not depend on `precision`; the label says how likely
    * each one is to be hit.
    */
  def findings(index: ClassIndex, scope: Scope, options: CheckOptions = CheckOptions()): Vector[Finding] = {
    val CheckOptions(precision, keep, loaded, _) = options
    val traversable                              = new BitSet()
    scope.traverse.foreach(traversable.set(_))
    // Classes a real run loaded are used, whatever the analysis can see: they start the walks too.
    val rules                       = KeepRules.from(index.syms, keep)
    val present: ClassId => Boolean = index.exists
    val targets                     = new FrameworkTargets(index, rules, Generated.of(index, rules), traversable)
    val loadedIds                   = loaded.iterator.map(index.syms.findClass).filterNot(_.isNone).filter(index.exists).toVector
    // Classes a configuration file names are loaded by the framework that reads it: roots too, like those a run loaded.
    val configured =
      if (rules.namedFiles.isEmpty) Vector.empty
      else
        NamedInResources
          .classes(index, rules.namedFiles, traversable)
          .filter(c =>
            index.get(c) match {
              case null => true
              case info => rules.enabled(info, -1, present)
            }
          )
    val used      = loadedIds ++ configured
    val roots     = scope.roots.iterator.flatMap(index.classesIn).toVector ++ used
    val reachable = Reachability.classes(index, roots, traversable, targets)
    val resolver  = new Resolver(index)
    val beans     = beanClasses(index, rules, reachable)
    val reflected = reflectedClasses(index, rules, reachable, present)
    reflected.or(beans)
    val access    = new Access(index, options.exportedPackages)
    val impls     = new Implementations(index)
    val overrides = new Overrides(index, access)
    val initName  = index.syms.nameId("<init>")
    val out       = ArrayBuffer.empty[Finding]
    scope.checked.foreach { s =>
      index
        .classesIn(s)
        .foreach { c =>
          if (reachable.get(c.value)) checkClass(index, resolver, access, impls, overrides, initName, reflected, beans, rules, c, out)
        }
    }
    checkProviders(index, scope, traversable, reachable, out)
    def graphWith(dispatch: Dispatch): MethodGraph =
      MethodGraph.build(index, resolver, rules, dispatch, scope.app.iterator.flatMap(index.classesIn).toVector, used, traversable, targets)
    val graph = precision match {
      case Precision.Class     => None
      case Precision.Method    => Some(graphWith(Dispatch.Cha))
      case Precision.MethodRta => Some(graphWith(Dispatch.Rta))
    }
    val probed = ProbedClasses.of(index, reachable, out)

    // Unreached code breaks nothing, so Latent wins; then a handler (here or around every caller),
    // then a probe for the missing class; else the conflict stands.
    // Without a call graph an unimplemented abstract method can still be seen to be called by nobody: no checked class names it.
    lazy val named                      = NamedMethods.of(index, reachable)
    def neverNamed(f: Finding): Boolean =
      graph.isEmpty && (f.conflict match {
        case Conflict.AbstractMethodNotImplemented(_, declaring, name, desc) =>
          // A method the JDK declares can be called from the JDK, which is not walked.
          val source = index.sourceIndex(declaring)
          source >= 0 && traversable.get(source) && !named.isCalled(name, desc) && !named.isLookedUp(name)
        case _ => false
      })

    def labelOf(f: Finding): Label =
      if (graph.exists(_.label(index, f) == Label.Latent) || neverNamed(f)) Label.Latent
      else if (Guarding.isGuarded(index, f, graph.fold(0)(_.inheritedGuard(index, f)))) Label.Guarded
      else
        f.conflict match {
          case Conflict.MissingClass(missing, _) if probed.get(missing.value) => Label.Probed
          case _                                                              =>
            graph.fold[Reach](Reach.Direct)(_.reach(index, f)) match {
              case Reach.Direct          => Label.Reachable
              case Reach.Rule            => Label.Reflective
              case Reach.Deserialization => Label.Deserialized
            }
        }
    out.iterator.map(f => f.copy(label = labelOf(f))).toVector
  }

  /** A listed provider that is not on the classpath fails the moment its service is iterated, so it counts exactly when the service would have its providers
    * started: the interface is reached, or is a JDK interface. Only service files of checked entries are looked at.
    */
  private def checkProviders(
      index: ClassIndex,
      scope: Scope,
      traversable: BitSet,
      reachable: BitSet,
      out: ArrayBuffer[Finding]
  ): Unit = {
    val checked = scope.checked.toSet
    index.services.missing.foreach { m =>
      val loadable = reachable.get(m.service.value) ||
        (index.exists(m.service) && !traversable.get(index.sourceIndex(m.service)))
      if (checked.contains(m.entry) && loadable) out += Finding(Conflict.MissingProvider(m.service, m.provider, m.entry), RefKind.Service, -1)
    }
  }

  /** The reachable classes a framework reflects on: those a rule applies to, and those a class-value annotation names. */
  /** The classes some reached class hands to a call a `BeanArguments` or `BuiltArguments` rule names: a framework reads all their properties. */
  private def beanClasses(index: ClassIndex, rules: KeepRules, reachable: BitSet): BitSet = {
    val beans = new BitSet()
    if (rules.hasArgumentRules) {
      @tailrec def from(c: Int): Unit =
        if (c >= 0) {
          index.get(ClassId(c)) match {
            case null => ()
            case info =>
              rules.beanTargets(info)(t => if (index.exists(t)) beans.set(t.value))
              rules.builtTargets(info)(t => if (index.exists(t)) beans.set(t.value))
          }
          from(reachable.nextSetBit(c + 1))
        }
      from(reachable.nextSetBit(0))
    }
    beans
  }

  private def reflectedClasses(index: ClassIndex, rules: KeepRules, reachable: BitSet, present: ClassId => Boolean): BitSet = {
    val reflected                   = new BitSet()
    @tailrec def from(c: Int): Unit =
      if (c >= 0) {
        index.get(ClassId(c)) match {
          case null => ()
          case info =>
            if (rules.reflectsOn(info) && rules.enabled(info, -1, present)) reflected.set(c)
            rules.referencedClasses(info)(t => reflected.set(t.value))
        }
        from(reachable.nextSetBit(c + 1))
      }
    from(reachable.nextSetBit(0))
    reflected
  }

  private def checkClass(
      index: ClassIndex,
      r: Resolver,
      access: Access,
      impls: Implementations,
      overrides: Overrides,
      initName: NameId,
      reflected: BitSet,
      beans: BitSet,
      rules: KeepRules,
      id: ClassId,
      out: ArrayBuffer[Finding]
  ): Unit =
    index.get(id) match {
      case null => out += Finding(Conflict.Unreadable(id, index.failure(id).getOrElse(ClassFileError.Unavailable)), RefKind.Hierarchy, -1)
      case info => new ClassChecker(index, r, access, impls, overrides, initName, reflected.get(id.value), beans.get(id.value), rules, id, info, out).run()
    }

  /** The checks of one class: what it extends, then each type, method and field reference it makes. */
  private final class ClassChecker(
      index: ClassIndex,
      r: Resolver,
      access: Access,
      impls: Implementations,
      overrides: Overrides,
      initName: NameId,
      reflectedOn: Boolean,
      readsAllTypes: Boolean,
      rules: KeepRules,
      id: ClassId,
      info: ClassInfo,
      out: ArrayBuffer[Finding]
  ) {
    def run(): Unit = {
      checkHierarchy()
      if (reflectedOn) checkSignatures()
      checkTypeRefs(0, 0)
      checkMethodRefs(0)
      checkFieldRefs(0)
    }

    private def requireClass(c: ClassId, kind: RefKind, ref: Int): Boolean = {
      val ok = index.exists(c)
      if (!ok) out += Finding(Conflict.MissingClass(c, id), kind, ref)
      ok
    }

    // A class that exists but may not be named from here fails with IllegalAccessError.
    private def requireUsable(c: ClassId, kind: RefKind, ref: Int): Boolean =
      requireClass(c, kind, ref) && {
        val ok = access.classAccessible(id, c)
        if (!ok) out += Finding(Conflict.InaccessibleClass(c, id), kind, ref)
        ok && {
          val exported = access.classExported(c)
          if (!exported) out += Finding(Conflict.NotExported(c, id), kind, ref)
          exported
        }
      }

    /** Reflection over a class loads the types in every member's signature, so each must exist. */
    private def checkSignatures(): Unit = {
      def missingIn(desc: DescId): List[ClassId] = {
        val t     = Type.getType(index.syms.desc(desc))
        val types = if (t.getSort == Type.METHOD) t.getReturnType :: t.getArgumentTypes.toList else List(t)
        types
          .flatMap { x =>
            val element = if (x.getSort == Type.ARRAY) x.getElementType else x
            if (element.getSort == Type.OBJECT) List(index.syms.classId(element.getInternalName)) else Nil
          }
          .filterNot(index.exists)
          .distinct
      }
      info.methods.foreach { m =>
        missingIn(m.desc).foreach(t => out += Finding(Conflict.MissingSignatureType(id, m.name, m.desc, t), RefKind.Hierarchy, -1))
      }
      info.fields.foreach { f =>
        missingIn(f.desc).foreach(t => out += Finding(Conflict.MissingSignatureType(id, f.name, f.desc, t), RefKind.Hierarchy, -1))
      }
      // The classes only a generic signature names (`List<Missing>`), which reflection on a member's generic types loads, for the
      // members a framework reads those of.
      val generic = info.signatureTypes
      // A class handed to a bean framework is read whole (every property); otherwise only what a rule fires for.
      lazy val reads                         = if (readsAllTypes) (_: Int, _: Int) => true else rules.readsGenericTypes(info)
      @tailrec def fromGeneric(i: Int): Unit =
        if (i < generic.length) {
          val t = ClassId(generic(i + 2))
          if (!index.exists(t) && reads(generic(i), generic(i + 1))) {
            val m = if (generic(i) == 0) info.methods(generic(i + 1)) else info.fields(generic(i + 1))
            out += Finding(Conflict.MissingSignatureType(id, m.name, m.desc, t), RefKind.Hierarchy, -1)
          }
          fromGeneric(i + 3)
        }
      fromGeneric(0)
    }

    private def isFinalField(declaring: ClassId, member: Int): Boolean =
      index.get(declaring) match {
        case null => false
        case d    => d.fields(member).access.has(Final)
      }

    private def hierarchy(target: ClassId, change: HierarchyChange): Unit =
      out += Finding(Conflict.IncompatibleHierarchy(id, target, change), RefKind.Hierarchy, -1)

    private def checkHierarchy(): Unit = {
      if (!info.superName.isNone && requireUsable(info.superName, RefKind.Hierarchy, -1))
        index.get(info.superName) match {
          case null => ()
          case sup  =>
            if (sup.isInterface) hierarchy(info.superName, HierarchyChange.SuperclassIsInterface)
            else if (sup.access.has(Final)) hierarchy(info.superName, HierarchyChange.SuperclassIsFinal)
        }
      info.interfaces.foreach { i =>
        if (requireUsable(i, RefKind.Hierarchy, -1))
          index.get(i) match {
            case null => ()
            case t    => if (!t.isInterface) hierarchy(i, HierarchyChange.InterfaceIsClass)
          }
      }
      overrides
        .finalOverridden(info)
        .foreach { case (declaring, m) =>
          out += Finding(Conflict.OverridesFinalMethod(id, declaring, info.methods(m).name, info.methods(m).desc), RefKind.Hierarchy, -1)
        }
      impls
        .unimplemented(info)
        .foreach { case (declaring, method) =>
          out += Finding(Conflict.AbstractMethodNotImplemented(id, declaring, method.name, method.desc), RefKind.Hierarchy, -1)
        }
    }

    /** `n` is the position in `info.newRefs` of the next `new` at or after entry `i`: both lists are in order, so they merge. */
    @tailrec private def checkTypeRefs(i: Int, n: Int): Unit =
      if (i < info.classRefs.length) {
        val target = ClassId(info.classRefs(i))
        val isNew  = n < info.newRefs.length && info.newRefs(n) == i
        // `new` of a class that is now abstract or an interface throws InstantiationError.
        if (requireUsable(target, RefKind.Type, i) && isNew)
          index.get(target) match {
            case null => ()
            case t    =>
              if (t.isInterface) out += Finding(Conflict.AbstractInstantiated(target, id, Uninstantiable.Interface), RefKind.Type, i)
              else if (t.isAbstract) out += Finding(Conflict.AbstractInstantiated(target, id, Uninstantiable.Abstract), RefKind.Type, i)
          }
        checkTypeRefs(i + 1, if (isNew) n + 1 else n)
      }

    /** What a failed method lookup is: the method is gone, has the other static-ness, or the hierarchy has a hole. */
    private def reportMethod(res: Resolution, owner: ClassId, name: NameId, desc: DescId, ref: Int): Unit = res match {
      case Resolution.Found(_, _)     => ()
      case Resolution.NotFound        => out += Finding(Conflict.MissingMethod(owner, name, desc, id), RefKind.Method, ref)
      case Resolution.KindMismatch    => out += Finding(Conflict.MethodKindChanged(owner, name, desc, id), RefKind.Method, ref)
      case Resolution.Incomplete(gap) => out += Finding(Conflict.MissingClass(gap, id), RefKind.Method, ref)
    }

    /** As [[reportMethod]] for a field, where static-ness is not told apart. */
    private def reportField(res: Resolution, owner: ClassId, name: NameId, desc: DescId, ref: Int): Unit = res match {
      case Resolution.Found(_, _)                        => ()
      case Resolution.NotFound | Resolution.KindMismatch => out += Finding(Conflict.MissingField(owner, name, desc, id), RefKind.Field, ref)
      case Resolution.Incomplete(gap)                    => out += Finding(Conflict.MissingClass(gap, id), RefKind.Field, ref)
    }

    @tailrec private def checkMethodRefs(i: Int): Unit = {
      val m = info.methodRefs
      if (i < m.length) {
        val owner = ClassId(m(i))
        if (requireUsable(owner, RefKind.Method, i / 4)) {
          val name = NameId(m(i + 1))
          val desc = DescId(m(i + 2))
          // The constant says interface method or class method; the JVM checks it against the owner.
          // Constructors are left out: a changed supertype is reported once, as the hierarchy conflict.
          index.get(owner) match {
            case null => ()
            case o    =>
              if (name != initName && o.isInterface != CallRef.isInterface(m(i + 3))) {
                val now = if (o.isInterface) OwnerKind.Interface else OwnerKind.Class
                out += Finding(Conflict.InterfaceKindChanged(owner, name, desc, id, now), RefKind.Method, i / 4)
              }
          }
          val res = r.method(owner, name, desc, CallRef.kind(m(i + 3)))
          reportMethod(res, owner, name, desc, i / 4)
          res match {
            case Resolution.Found(declaring, member) if member != Resolution.NoMember =>
              if (!access.methodAccessible(info, declaring, member))
                out += Finding(Conflict.InaccessibleMethod(owner, name, desc, id), RefKind.Method, i / 4)
            case _ => ()
          }
        }
        checkMethodRefs(i + 4)
      }
    }

    @tailrec private def checkFieldRefs(i: Int): Unit = {
      val f = info.fieldRefs
      if (i < f.length) {
        val owner = ClassId(f(i))
        if (requireUsable(owner, RefKind.Field, i / 3)) {
          val name = NameId(f(i + 1))
          val desc = DescId(f(i + 2))
          val res  = r.field(owner, name, desc)
          reportField(res, owner, name, desc, i / 3)
          res match {
            case Resolution.Found(declaring, member) =>
              if (!access.fieldAccessible(info, declaring, member)) out += Finding(Conflict.InaccessibleField(owner, name, desc, id), RefKind.Field, i / 3)
              // A write needs the field to be non-final, or to be written by its own class.
              else if (declaring != id && info.isFieldWrite(i / 3) && isFinalField(declaring, member))
                out += Finding(Conflict.FinalFieldWritten(owner, name, desc, id), RefKind.Field, i / 3)
            case _ => ()
          }
        }
        checkFieldRefs(i + 3)
      }
    }
  }
}
