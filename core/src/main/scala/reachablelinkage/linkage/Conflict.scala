package reachablelinkage.linkage

import org.objectweb.asm.Type

import reachablelinkage.classfile.{ClassFileError, Guards}
import reachablelinkage.symbols.{ClassId, DescId, NameId, Symbols}

/** A reference that will not link. `from` is the class holding the reference (Class precision). */
enum Conflict:
  case MissingClass(missing: ClassId, from: ClassId)
  case MissingMethod(owner: ClassId, name: NameId, desc: DescId, from: ClassId)

  /** The member exists but is static where the call site expects instance, or the reverse. */
  case MethodKindChanged(owner: ClassId, name: NameId, desc: DescId, from: ClassId)
  case MissingField(owner: ClassId, name: NameId, desc: DescId, from: ClassId)

  /** `from` may not name class `target`: it is not public and is in another package. */
  case InaccessibleClass(target: ClassId, from: ClassId)

  /** The method exists, but `from` may not call it (it became private, protected or package-private). */
  case InaccessibleMethod(owner: ClassId, name: NameId, desc: DescId, from: ClassId)

  /** The field exists, but `from` may not use it. */
  case InaccessibleField(owner: ClassId, name: NameId, desc: DescId, from: ClassId)

  /** A service file of classpath entry `declaredIn` lists a provider that is not on the classpath, so iterating the service throws `ServiceConfigurationError`.
    * `from` is the service interface.
    */
  case MissingProvider(service: ClassId, provider: ClassId, declaredIn: Int)

  /** `cls` is concrete but neither it, its superclasses nor a default method implement the abstract method `name``desc` that `declaring` (a superclass or
    * interface) declares, so calling it on an instance throws `AbstractMethodError`. Typical when an interface gains a method and an older jar still has its
    * implementation (spotify #22).
    */
  case AbstractMethodNotImplemented(cls: ClassId, declaring: ClassId, name: NameId, desc: DescId)
  case Unreadable(cls: ClassId, reason: ClassFileError)

  /** `cls` extends or implements `target`, which is no longer the kind of type that allows it; loading `cls` throws `IncompatibleClassChangeError`. */
  case IncompatibleHierarchy(cls: ClassId, target: ClassId, change: HierarchyChange)

  /** `cls` overrides the method `name``desc` that `declaring`, a superclass, now declares `final`; loading `cls` throws `VerifyError`. */
  case OverridesFinalMethod(cls: ClassId, declaring: ClassId, name: NameId, desc: DescId)

  /** `from` calls `owner.name``desc` as a method of an interface (or of a class) but `owner` is now the other, so the call throws
    * `IncompatibleClassChangeError`. `now` is what `owner` is now.
    */
  case InterfaceKindChanged(owner: ClassId, name: NameId, desc: DescId, from: ClassId, now: OwnerKind)

  /** `from` builds `target` with `new`, but `target` is now abstract or an interface, so the `new` throws `InstantiationError`. */
  case AbstractInstantiated(target: ClassId, from: ClassId, why: Uninstantiable)

  /** `from` writes the field `owner.name``desc`, which is now `final` and declared in another class, so the write throws `IllegalAccessError`. (A final field
    * written by its own class outside a constructor is rejected by newer JVMs only, so it is not reported.)
    */
  case FinalFieldWritten(owner: ClassId, name: NameId, desc: DescId, from: ClassId)

  /** `cls` is a class a framework reflects on, and the type of its member `name``desc` (a parameter, a return value or a field) is `missing`: loading the
    * member's types, which reflection over the class does for all of them, throws `NoClassDefFoundError`.
    */
  case MissingSignatureType(cls: ClassId, name: NameId, desc: DescId, missing: ClassId)

  /** `from` names the JDK class `target`, whose module does not export its package to the classpath, so the JVM refuses the access (`IllegalAccessError`)
    * unless it runs with `--add-exports`.
    */
  case NotExported(target: ClassId, from: ClassId)

  /** The class that holds the offending reference. */
  def inClass: ClassId = describe.from

  def show(s: Symbols): String = describe.text(s)

  /** Everything the rest of the code needs to know about this conflict, in one place: a new conflict kind adds a case here. */
  private[reachablelinkage] lazy val describe: Conflict.Description = Conflict.describe(this)

object Conflict:
  /** Catching any of these catches every linkage error. */
  private val Broad: Int = Guards.Linkage | Guards.Error | Guards.Throwable

  /** What a conflict is, written once.
    *
    * @param from
    *   the class that holds the offending reference
    * @param category
    *   the facade category (the same name as `ConflictCategory`, which a test keeps in step)
    * @param stoppedBy
    *   the caught types that stop the error the JVM throws; 0 if there is no instruction to protect (the error comes from loading the class, or from a call
    *   elsewhere)
    * @param target
    *   the class the report names as broken
    * @param member
    *   the broken member of `target`, if there is one
    * @param existsIn
    *   the class whose classpath entry the report names as holding the thing, if it exists anywhere
    * @param declaredIn
    *   the classpath entry that ships the stale service file (a missing provider only)
    * @param text
    *   one line for people
    * @param reason
    *   the reason of the facade's conflict; the text unless the plugin contract words it differently
    */
  private[reachablelinkage] final case class Description(
      from: ClassId,
      category: Category,
      stoppedBy: Int,
      target: ClassId,
      member: Option[(NameId, DescId)],
      existsIn: Option[ClassId],
      declaredIn: Option[Int],
      text: Symbols => String,
      reason: Symbols => String
  )

  private def dot(internal: String): String = internal.replace('/', '.')

  private def cls(s: Symbols, c: ClassId): String = dot(s.className(c))

  private def args(desc: String): String = Type.getArgumentTypes(desc).map(_.getClassName).mkString(", ")

  /** `a.B.name(arg, arg)`, how the report names a method. */
  private def method(s: Symbols, o: ClassId, n: NameId, d: DescId): String = s"${cls(s, o)}.${s.name(n)}(${args(s.desc(d))})"

  private def describe(c: Conflict): Description =
    def of(
        from: ClassId,
        category: Category,
        stoppedBy: Int,
        target: ClassId,
        member: Option[(NameId, DescId)],
        existsIn: Option[ClassId],
        text: Symbols => String,
        reason: Option[Symbols => String] = None,
        declaredIn: Option[Int] = None
    ): Description = Description(from, category, stoppedBy, target, member, existsIn, declaredIn, text, reason.getOrElse(text))

    c match
      case MissingClass(m, f) =>
        of(
          f,
          Category.CLASS_NOT_FOUND,
          Guards.NoClassDef | Broad,
          m,
          None,
          None,
          s => s"${cls(s, f)}: class ${cls(s, m)} is missing",
          Some(s => s"Class not found: ${cls(s, m)}")
        )
      case MissingMethod(o, n, d, f) =>
        of(
          f,
          Category.METHOD_SIGNATURE_NOT_FOUND,
          Guards.NoSuchMethod | Guards.IncompatibleClassChange | Broad,
          o,
          Some((n, d)),
          Some(o),
          s => s"${cls(s, f)}: method ${cls(s, o)}.${s.name(n)}${s.desc(d)} is missing",
          Some(s => s"Method not found: ${method(s, o, n, d)}")
        )
      case MethodKindChanged(o, n, d, f) =>
        of(
          f,
          Category.METHOD_SIGNATURE_NOT_FOUND,
          Guards.IncompatibleClassChange | Broad,
          o,
          Some((n, d)),
          Some(o),
          s => s"${cls(s, f)}: method ${cls(s, o)}.${s.name(n)}${s.desc(d)} changed between static and instance",
          Some(s => s"Method changed between static and instance: ${method(s, o, n, d)}")
        )
      case MissingField(o, n, d, f) =>
        of(
          f,
          Category.FIELD_NOT_FOUND,
          Guards.NoSuchField | Guards.IncompatibleClassChange | Broad,
          o,
          Some((n, d)),
          Some(o),
          s => s"${cls(s, f)}: field ${cls(s, o)}.${s.name(n)}:${s.desc(d)} is missing",
          Some(s => s"Field not found: ${s.name(n)}")
        )
      case InaccessibleClass(t, f) =>
        of(
          f,
          Category.INACCESSIBLE_CLASS,
          Guards.IllegalAccess | Guards.IncompatibleClassChange | Broad,
          t,
          None,
          Some(t),
          s => s"${cls(s, f)}: class ${cls(s, t)} is not accessible",
          Some(s => s"Class ${cls(s, t)} is not accessible from ${cls(s, f)}")
        )
      case InaccessibleMethod(o, n, d, f) =>
        of(
          f,
          Category.INACCESSIBLE_MEMBER,
          Guards.IllegalAccess | Guards.IncompatibleClassChange | Broad,
          o,
          Some((n, d)),
          Some(o),
          s => s"${cls(s, f)}: method ${cls(s, o)}.${s.name(n)}${s.desc(d)} is not accessible",
          Some(s => s"Method ${method(s, o, n, d)} is not accessible from ${cls(s, f)}")
        )
      case InaccessibleField(o, n, d, f) =>
        of(
          f,
          Category.INACCESSIBLE_MEMBER,
          Guards.IllegalAccess | Guards.IncompatibleClassChange | Broad,
          o,
          Some((n, d)),
          Some(o),
          s => s"${cls(s, f)}: field ${cls(s, o)}.${s.name(n)}:${s.desc(d)} is not accessible",
          Some(s => s"Field ${cls(s, o)}.${s.name(n)} is not accessible from ${cls(s, f)}")
        )
      case MissingProvider(svc, p, declaredIn) =>
        // The conflict belongs to the entry that ships the stale service file, not to the interface's.
        of(
          svc,
          Category.MISSING_PROVIDER,
          0,
          p,
          None,
          None,
          s => s"${cls(s, svc)}: provider ${cls(s, p)} is missing (listed in META-INF/services)",
          Some(s => s"Provider ${cls(s, p)} is listed in META-INF/services/${cls(s, svc)} but is not on the classpath"),
          Some(declaredIn)
        )
      // The error comes from a call elsewhere, not from an instruction in this class.
      case AbstractMethodNotImplemented(cl, declaring, n, d) =>
        of(
          cl,
          Category.ABSTRACT_METHOD_NOT_IMPLEMENTED,
          0,
          declaring,
          Some((n, d)),
          Some(declaring),
          s => s"${cls(s, cl)}: abstract method ${cls(s, declaring)}.${s.name(n)}${s.desc(d)} is not implemented",
          Some(s => s"Abstract method ${method(s, declaring, n, d)} is not implemented by ${cls(s, cl)}")
        )
      case Unreadable(cl, r) =>
        of(
          cl,
          Category.UNREADABLE_CLASS,
          0,
          cl,
          None,
          Some(cl),
          s => s"${cls(s, cl)}: unreadable class file (${r.message})",
          Some(_ => s"Unreadable class file: $r")
        )
      // Found while loading the class, before any of its code runs: no handler in it can be in place.
      case IncompatibleHierarchy(cl, t, change) =>
        of(
          cl,
          Category.INCOMPATIBLE_HIERARCHY,
          0,
          t,
          None,
          Some(t),
          s =>
            val what = change match
              case HierarchyChange.SuperclassIsFinal     => s"cannot inherit from final class ${cls(s, t)}"
              case HierarchyChange.SuperclassIsInterface => s"has interface ${cls(s, t)} as its superclass"
              case HierarchyChange.InterfaceIsClass      => s"cannot implement ${cls(s, t)}, which is not an interface"
            s"${cls(s, cl)}: $what"
        )
      case OverridesFinalMethod(cl, declaring, n, d) =>
        of(
          cl,
          Category.OVERRIDES_FINAL_METHOD,
          0,
          declaring,
          Some((n, d)),
          Some(declaring),
          s => s"${cls(s, cl)}: overrides final method ${cls(s, declaring)}.${s.name(n)}${s.desc(d)}"
        )
      case InterfaceKindChanged(o, n, d, f, now) =>
        of(
          f,
          Category.INTERFACE_CLASS_MISMATCH,
          Guards.IncompatibleClassChange | Broad,
          o,
          Some((n, d)),
          Some(o),
          s =>
            val was = if now == OwnerKind.Interface then "a class" else "an interface"
            s"${cls(s, f)}: method ${cls(s, o)}.${s.name(n)}${s
                .desc(d)} is called as a method of $was, but the owner is now ${if now == OwnerKind.Interface then "an interface" else "a class"}"
        )
      case AbstractInstantiated(t, f, why) =>
        of(
          f,
          Category.ABSTRACT_INSTANTIATION,
          Guards.Instantiation | Guards.IncompatibleClassChange | Broad,
          t,
          None,
          Some(t),
          s => s"${cls(s, f)}: cannot instantiate ${cls(s, t)}, which is now ${if why == Uninstantiable.Abstract then "abstract" else "an interface"}"
        )
      case FinalFieldWritten(o, n, d, f) =>
        of(
          f,
          Category.FINAL_FIELD_WRITE,
          Guards.IllegalAccess | Guards.IncompatibleClassChange | Broad,
          o,
          Some((n, d)),
          Some(o),
          s => s"${cls(s, f)}: writes the field ${cls(s, o)}.${s.name(n)}, which is now final"
        )
      // Found by a framework reflecting over the class, not by an instruction in it.
      case MissingSignatureType(cl, n, d, m) =>
        of(
          cl,
          Category.MISSING_SIGNATURE_TYPE,
          0,
          m,
          Some((n, d)),
          None,
          s => s"${cls(s, cl)}: the signature of ${s.name(n)} names the class ${cls(s, m)}, which is missing"
        )
      case NotExported(t, f) =>
        of(
          f,
          Category.NOT_EXPORTED,
          Guards.IllegalAccess | Guards.IncompatibleClassChange | Broad,
          t,
          None,
          Some(t),
          s => s"${cls(s, f)}: ${cls(s, t)} is in a package its module does not export"
        )
