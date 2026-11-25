package reachablelinkage.classfile

import scala.annotation.tailrec
import reachablelinkage.classfile.AccessFlags.*
import reachablelinkage.symbols.ClassId

/**
 * What the checker needs from one class file. References are flat int arrays to keep the
 * per-class footprint small: `fieldRefs` has three ints per entry (owner ClassId, name NameId,
 * desc DescId); `methodRefs` has four (the same three, then 1 if the call site is static, else 0).
 * `classRefs` has one ClassId per entry. The three `*RefStart` arrays have one more entry than there
 * are methods: method `m` owns reference entries `start(m)` until `start(m + 1)` of the matching array
 * (entries, not ints), because references are recorded in method order. `guards` lists, for the
 * references inside a `try` range whose handlers catch a linkage error type, triples of (kind, entry,
 * [[Guards]] bits); it is empty for most classes. `probes` holds the internal names of the string
 * constants that look like class names (see [[ProbeNames]]). `annotations` lists, for every visible
 * annotation, quadruples of (kind, member index or -1, annotation
 * ClassId, class value or -1): kind 0 is an annotation on the class, 1 on a method, 3 on a field (the index is
 * the member's), 2 a class value found inside an annotation (on the class or on a method; none for a field). `newRefs` and `fieldWrites` list, in order, the
 * entries of `classRefs` that come from a `new` instruction and the entries of `fieldRefs` that are writes
 * (`putfield`, `putstatic`); the rest are other uses. `signatureTypes` holds, three ints per entry (0 for a method and 1 for a field, the member's
 * index, a ClassId), each class named in the generic `Signature` of a member (`List<Missing>` names `Missing`, which the erased descriptor does not):
 * reflection on a member's generic types loads them. `literalArgs` holds, three ints per entry (the owner of a call as a ClassId, the called
 * method's NameId, a ClassId), each class literal (`Foo.class`) passed to a call: a framework that is handed a class (`mapper.readValue(json, Foo.class)`)
 * builds it and calls its accessors, though no instruction does. `positions` is only set by a parse that
 * asked for it.
 */
private[reachablelinkage] final class ClassInfo(
    val id: ClassId,
    val access: Int,
    val superName: ClassId,
    val interfaces: Array[ClassId],
    val methods: Array[Member],
    val fields: Array[Member],
    val methodRefs: Array[Int],
    val fieldRefs: Array[Int],
    val classRefs: Array[Int],
    val methodRefStart: Array[Int],
    val fieldRefStart: Array[Int],
    val classRefStart: Array[Int],
    val guards: Array[Int],
    val encodedProbes: Array[String],
    val annotations: Array[Int],
    val newRefs: Array[Int],
    val fieldWrites: Array[Int],
    val signatureTypes: Array[Int],
    val literalArgs: Array[Int],
    val positions: Positions | Null = null
):
  /** What the code does with a name or a type at run time (see [[Probe]]); `encodedProbes` is how it is stored. */
  lazy val probes: Array[Probe] = if encodedProbes.isEmpty then Probe.NoProbes else encodedProbes.map(Probe.decode)

  /** Constant method names this class passes to reflective lookups (`getMethod("run")`). */
  lazy val reflectedMethodNames: Array[String] =
    probes.collect { case Probe.MethodName(name) => name }

  private lazy val staticIndex: LongIntMap   = indexOfMethods(CallKind.Static)
  private lazy val instanceIndex: LongIntMap = indexOfMethods(CallKind.Instance)
  private lazy val fieldIndexMap: LongIntMap = indexOfFields()

  private def indexOfMethods(kind: CallKind): LongIntMap =
    val wantStatic = kind == CallKind.Static
    val map        = new LongIntMap(methods.length)
    @tailrec def from(i: Int): Unit =
      if i < methods.length then
        val m = methods(i)
        if m.access.has(Static) == wantStatic then map.put(Keys.of(m.name.value, m.desc.value), i)
        from(i + 1)
    from(0)
    map

  /** The position in `newRefs` of the first `new` at or after `classRefs` entry `ref` (`newRefs.length` if none). */
  def newRefFrom(ref: Int): Int =
    val at = java.util.Arrays.binarySearch(newRefs, ref)
    if at >= 0 then at else -at - 1

  /** Is `fieldRefs` entry `ref` a write (`putfield` or `putstatic`)? */
  def isFieldWrite(ref: Int): Boolean = java.util.Arrays.binarySearch(fieldWrites, ref) >= 0

  /** Index into `methods` of the declared method with this key and kind, or -1. */
  def methodIndex(key: Long, kind: CallKind): Int =
    (if kind == CallKind.Static then staticIndex else instanceIndex).get(key)

  /** Indices of the abstract instance methods this class declares (an interface's, usually). */
  lazy val abstractMethods: Array[Int] =
    methods.indices.filter(i => methods(i).access.has(Abstract) && !methods(i).access.has(Static)).toArray

  private def indexOfFields(): LongIntMap =
    val map = new LongIntMap(fields.length)
    @tailrec def from(i: Int): Unit =
      if i < fields.length then
        map.put(Keys.of(fields(i).name.value, fields(i).desc.value), i)
        from(i + 1)
    from(0)
    map

  /** Index into `fields` of the declared field with this key, or -1. */
  def fieldIndex(key: Long): Int = fieldIndexMap.get(key)

  def declaresMethod(key: Long, kind: CallKind): Boolean = methodIndex(key, kind) >= 0
  def declaresField(key: Long): Boolean = fieldIndex(key) >= 0

  /** [[Guards]] bits of the handlers protecting method-reference entry `ref`; 0 if none. */
  def guardOfMethodRef(ref: Int): Int = guardOf(0, ref)
  def guardOfFieldRef(ref: Int): Int  = guardOf(1, ref)
  def guardOfClassRef(ref: Int): Int  = guardOf(2, ref)

  private lazy val guardMap: LongIntMap =
    val map = new LongIntMap(guards.length / 3)
    @tailrec def from(i: Int): Unit =
      if i < guards.length then
        map.put((guards(i).toLong << 32) | guards(i + 1).toLong, guards(i + 2))
        from(i + 3)
    from(0)
    map

  private def guardOf(kind: Int, ref: Int): Int =
    if guards.length == 0 then 0
    else math.max(guardMap.get((kind.toLong << 32) | ref.toLong), 0)

  /** The method that owns method-reference entry `ref`, found by binary search over the starts. */
  def methodOfMethodRef(ref: Int): Int = ownerOf(methodRefStart, ref)
  def methodOfFieldRef(ref: Int): Int  = ownerOf(fieldRefStart, ref)
  def methodOfClassRef(ref: Int): Int  = ownerOf(classRefStart, ref)

  /** The largest method index whose range starts at or before `ref`; -1 if the class has no methods. */
  private def ownerOf(starts: Array[Int], ref: Int): Int =
    @tailrec def search(lo: Int, hi: Int): Int =
      if lo >= hi then lo
      else
        val mid = (lo + hi + 1) >>> 1
        if starts(mid) <= ref then search(mid, hi) else search(lo, mid - 1)
    if starts.length < 2 then -1 else search(0, starts.length - 2)

  def isInterface: Boolean = access.has(Interface)
  def isAbstract: Boolean  = access.has(Abstract)
