package reachablelinkage.classfile

import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, FieldVisitor, Handle, Label, MethodVisitor, Opcodes, Type}
import org.objectweb.asm.signature.{SignatureReader, SignatureVisitor}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.chaining.*

import reachablelinkage.symbols.{ClassId, Symbols}

/**
 * Parses one class file with the ASM visitor API. No ClassNode and no frames. Debug info is skipped
 * unless `detail` is [[ParseDetail.WithPositions]], which reporting uses to recover method and line of each reference.
 * Both modes run the same visitor, so reference `i` is the same instruction in either.
 */
private[reachablelinkage] object ClassParser:

  /**
   * Bump this whenever what `parse` produces for the same bytes changes (a new `ClassInfo` field, a new
   * kind of reference recorded, a different encoding). It is written into every cached jar summary, so
   * a bump makes every old summary stale instead of silently wrong.
   */
  val Version: Int = 17

  /** Highest class file major version we accept (Java 25). */
  val MaxMajor: Int = 69

  def parse(bytes: Array[Byte], syms: Symbols, detail: ParseDetail = ParseDetail.Lean): Either[ClassFileError, ClassInfo] =
    // Check the version first: ASM's ClassReader constructor throws on versions it does not know.
    val major = if bytes.length < 8 then -1 else ((bytes(6) & 0xff) << 8) | (bytes(7) & 0xff)
    if major < 0 then Left(ClassFileError.Truncated)
    else if major > MaxMajor then Left(ClassFileError.UnsupportedVersion(major))
    else
      try
        val v = new Collector(syms, detail)
        val flags = detail match
          case ParseDetail.WithPositions => ClassReader.SKIP_FRAMES
          case ParseDetail.Lean          => ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
        new ClassReader(bytes).accept(v, flags)
        Right(v.result)
      catch case e: RuntimeException => Left(ClassFileError.Malformed(String.valueOf(e.getMessage)))

  private final class Collector(syms: Symbols, detail: ParseDetail) extends ClassVisitor(Opcodes.ASM9):
    private val withPositions = detail == ParseDetail.WithPositions
    private var id: ClassId           = ClassId.None
    private var access                = 0
    private var superId: ClassId      = ClassId.None
    private var ifaces: Array[ClassId] = Array.empty
    private val methods              = ArrayBuffer.empty[Member]
    private val fields               = ArrayBuffer.empty[Member]
    private val mrefs                = new IntBuf
    private val frefs                = new IntBuf
    private val crefs                = new IntBuf
    private val mstart               = new IntBuf
    private val fstart               = new IntBuf
    private val cstart               = new IntBuf
    private val guards               = new IntBuf
    private val probes               = ArrayBuffer.empty[Probe]
    private val annotations          = new IntBuf
    private val newRefs              = new IntBuf
    private val fieldWrites          = new IntBuf
    private val signatureTypes       = new IntBuf
    private val literalArgs          = new IntBuf
    // `.pkg.Class` constants, which only count if this class loads a class by name; and whether it does.
    private val fragments            = scala.collection.mutable.LinkedHashSet.empty[String]
    private var loadsByName          = false
    private val handlers             = new HandlerTracker
    private val mpos                 = new IntBuf
    private val fpos                 = new IntBuf
    private val cpos                 = new IntBuf
    private var curMethod            = -1
    private var curLine              = 0
    // The last string constant loaded in this method that no call has used up (a reflective member name).
    private var lastConst: String | Null = null
    // The class literals (`Foo.class`) still on this method's operand stack, so a call can tell which are its arguments.
    private val literals = new LiteralStack

    /** The probes, and the fragments of class names this class builds if it loads a class by name at all. */
    private def withFragments: ArrayBuffer[Probe] =
      if loadsByName && fragments.nonEmpty then probes ++ fragments.map(Probe.NameTail(_)) else probes

    def result: ClassInfo =
      ClassInfo(id, access, superId, ifaces, methods.toArray, fields.toArray,
        mrefs.toArray, frefs.toArray, crefs.toArray,
        closed(mstart, mrefs.length / 4), closed(fstart, frefs.length / 3), closed(cstart, crefs.length),
        guards.toArray, withFragments.map(Probe.encode).toArray, annotations.toArray, newRefs.toArray, fieldWrites.toArray, signatureTypes.toArray, literalArgs.toArray,
        if withPositions then Positions(mpos.toArray, fpos.toArray, cpos.toArray) else null)

    /** The per-method start offsets plus the end of the last method. */
    private def closed(starts: IntBuf, end: Int): Array[Int] =
      val a = starts.toArray
      java.util.Arrays.copyOf(a, a.length + 1).tap(_(a.length) = end)

    /** Remembers which linkage-error handlers protect the reference just recorded (`kind`: 0 method, 1 field, 2 type). */
    private def guarded(kind: Int, entry: Int): Unit =
      if handlers.protectedBy != 0 then
        guards.add(kind); guards.add(entry); guards.add(handlers.protectedBy)
        handlers.protect(guards.length - 1)

    private def at(buf: IntBuf): Unit =
      if withPositions then
        buf.add(curMethod); buf.add(curLine)

    private def cls(internal: String): Int =
      // Array owners resolve against Object; element types are not link targets here.
      if internal.startsWith("[") then syms.classId("java/lang/Object").value
      else syms.classId(internal).value

    override def visit(version: Int, acc: Int, name: String, sig: String, sup: String, itfs: Array[String]): Unit =
      id = syms.classId(name)
      access = acc
      superId = if sup == null then ClassId.None else syms.classId(sup)
      ifaces = if itfs == null then Array.empty else itfs.map(syms.classId)

    private def addAnnotationEntry(kind: Int, member: Int, annotation: Int, value: Int): Unit =
      annotations.add(kind); annotations.add(member); annotations.add(annotation); annotations.add(value)

    /** A visible annotation, with the class values inside it; keep rules decide later which ones matter. */
    private final class Recorder(method: Int, annotation: Int) extends AnnotationVisitor(Opcodes.ASM9):
      override def visit(name: String, value: Any): Unit =
        value match
          case t: Type if t.getSort == Type.OBJECT =>
            addAnnotationEntry(AnnotationEntries.ClassValue, method, annotation, syms.classId(t.getInternalName).value)
          // A class named by a string (`@ConditionalOnClass(name = "a.B")`): kind 4, so a rule can ask whether it is present without loading it.
          case s: String =>
            val internal = ProbeNames.internalName(s)
            if internal != null then
              addAnnotationEntry(AnnotationEntries.NamedClassValue, method, annotation, syms.classId(internal.nn).value)
          case _ => ()
      override def visitArray(name: String): AnnotationVisitor = this
      override def visitAnnotation(name: String, desc: String): AnnotationVisitor = this

    /** Records an annotation on the class (`kind` 0, `member` -1), a method (1) or a field (3). */
    private def annotated(kind: Int, member: Int, desc: String, visible: Boolean): AnnotationVisitor | Null =
      if !visible then null
      else
        val id = syms.classId(Type.getType(desc).getInternalName).value
        addAnnotationEntry(kind, member, id, -1)
        // Class values inside an annotation on a field are filed under no method.
        Recorder(if kind == AnnotationEntries.OnMethod then member else -1, id)

    override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor | Null = annotated(AnnotationEntries.OnClass, -1, desc, visible)

    /** The annotations of the field just declared: one visitor serves every field, told which one it is on. */
    private object FieldBody extends FieldVisitor(Opcodes.ASM9):
      var field = -1
      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor | Null = annotated(AnnotationEntries.OnField, field, desc, visible)

    /** Records the classes a generic signature names (type arguments, bounds and the rest) for member `member` of `kind` (0 method, 1 field). */
    private def signature(kind: Int, member: Int, sig: String | Null): Unit =
      if sig != null then
        // This member's entries are the last ones added, and a signature names few classes, so a scan of them finds a repeat without boxing.
        val from = signatureTypes.length
        @tailrec def known(i: Int, c: Int): Boolean = i < signatureTypes.length && (signatureTypes(i + 2) == c || known(i + 3, c))
        def add(name: String): Unit =
          val c = syms.classId(name).value
          if !known(from, c) then { signatureTypes.add(kind); signatureTypes.add(member); signatureTypes.add(c) }
        // One visitor per class type: a type argument gets its own, so it cannot overwrite the outer class `Outer<A>.Inner<B>` is read through.
        final class Names extends SignatureVisitor(Opcodes.ASM9):
          private var current = ""
          override def visitClassType(name: String): Unit = { current = name; add(name) }
          // `Outer<A>.Inner<B>`: the inner class is `Outer$Inner`.
          override def visitInnerClassType(name: String): Unit = { current = current + "$" + name; add(current) }
          override def visitTypeArgument(wildcard: Char): SignatureVisitor = new Names
        new SignatureReader(sig.nn).accept(new Names)

    override def visitField(acc: Int, name: String, desc: String, sig: String, value: Any): FieldVisitor | Null =
      fields += Member(syms.nameId(name), syms.descId(desc), acc)
      signature(1, fields.length - 1, sig)
      FieldBody.field = fields.length - 1
      FieldBody

    override def visitMethod(acc: Int, name: String, desc: String, sig: String, exc: Array[String]): MethodVisitor =
      methods += Member(syms.nameId(name), syms.descId(desc), acc)
      curMethod = methods.length - 1
      signature(0, curMethod, sig)
      curLine = 0
      lastConst = null
      literals.clear()
      handlers.startMethod()
      mstart.add(mrefs.length / 4); fstart.add(frefs.length / 3); cstart.add(crefs.length)
      Body

    private object Body extends MethodVisitor(Opcodes.ASM9):
      private def record(owner: String, name: String, desc: String, code: Int): Unit =
        mrefs.add(cls(owner)); mrefs.add(syms.nameId(name).value); mrefs.add(syms.descId(desc).value)
        mrefs.add(code)
        at(mpos)
        guarded(0, mrefs.length / 4 - 1)

      /** A call made at this instruction. */
      private def callTo(owner: String, name: String, desc: String, call: CallKind, interface: Boolean): Unit =
        record(owner, name, desc, CallRef.direct(call, interface))

      /** A method handle for `invokedynamic`: its target runs later, so no `try` here protects it. */
      private def handleTo(owner: String, name: String, desc: String, call: CallKind, interface: Boolean): Unit =
        record(owner, name, desc, CallRef.handle(call, interface))

      /** What an operand-less instruction does to the stack of class literals; what cannot be followed exactly forgets them. */
      private def valueEffect(op: Int): Unit =
        import Opcodes.*
        if op == NOP then ()
        else if op >= ACONST_NULL && op <= DCONST_1 then literals.effect(0, 1)
        else if op >= IALOAD && op <= SALOAD then literals.effect(2, 1)
        else if op == AASTORE then literals.arrayStore()
        else if (op >= IASTORE && op <= SASTORE) then literals.effect(3, 0)
        else if op == POP then literals.effect(1, 0)
        else if op == DUP then literals.dup()
        else if (op >= IADD && op <= DREM) || (op >= ISHL && op <= LXOR) || (op >= LCMP && op <= DCMPG) then literals.effect(2, 1)
        else if (op >= INEG && op <= DNEG) || (op >= I2L && op <= I2S) || op == ARRAYLENGTH then literals.effect(1, 1)
        else if op == MONITORENTER || op == MONITOREXIT then literals.effect(1, 0)
        else literals.clear()   // pop2, dup2, dup_x, swap, returns, athrow: not followed

      private def callOf(isStatic: Boolean): CallKind = if isStatic then CallKind.Static else CallKind.Instance

      override def visitLineNumber(line: Int, start: Label): Unit = curLine = line

      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor | Null = annotated(AnnotationEntries.OnMethod, curMethod, desc, visible)

      override def visitMethodInsn(op: Int, owner: String, name: String, desc: String, itf: Boolean): Unit =
        handlers.methodInsn(name)
        callTo(owner, name, desc, callOf(op == Opcodes.INVOKESTATIC), itf)
        if !literals.isEmpty then
          val argc = Type.getArgumentTypes(desc).length
          val pops = if op == Opcodes.INVOKESTATIC then argc else argc + 1
          val result = Type.getReturnType(desc)
          // A constructor leaves the new object under its arguments, and an object result stands for what it was given.
          val carry  = name == "<init>" || result.getSort == Type.OBJECT || result.getSort == Type.ARRAY
          literals.call(pops, if result == Type.VOID_TYPE then 0 else 1, carry): lit =>
            literalArgs.add(cls(owner)); literalArgs.add(syms.nameId(name).value); literalArgs.add(lit)
        if ReflectionCalls.isClassLoad(owner, name) then loadsByName = true
        if lastConst != null && ReflectionCalls.isMemberLookup(owner, name) then probes += Probe.MethodName(lastConst.nn)
        // Building a `MethodType` between the name and the lookup does not separate them.
        if owner != "java/lang/invoke/MethodType" then lastConst = null
        if name == "<init>" then
          crefs.add(cls(owner)); at(cpos)
          guarded(2, crefs.length - 1)

      override def visitFieldInsn(op: Int, owner: String, name: String, desc: String): Unit =
        op match
          case Opcodes.GETSTATIC => literals.effect(0, 1)
          case Opcodes.PUTSTATIC => literals.effect(1, 0)
          case Opcodes.GETFIELD  => literals.effect(1, 1)
          case _                 => literals.effect(2, 0)
        handlers.other()
        frefs.add(cls(owner)); frefs.add(syms.nameId(name).value); frefs.add(syms.descId(desc).value)
        at(fpos)
        if op == Opcodes.PUTFIELD || op == Opcodes.PUTSTATIC then fieldWrites.add(frefs.length / 3 - 1)
        guarded(1, frefs.length / 3 - 1)

      override def visitTypeInsn(op: Int, tpe: String): Unit =
        if op == Opcodes.NEW then literals.effect(0, 1) else literals.effect(1, 1)   // anewarray, checkcast, instanceof
        handlers.typeInsn(op)
        if op != Opcodes.ANEWARRAY then
          crefs.add(cls(tpe)); at(cpos)
          if op == Opcodes.NEW then newRefs.add(crefs.length - 1)
          guarded(2, crefs.length - 1)

      // Class literals (`Foo.class`): loading the constant loads the class.
      override def visitLdcInsn(value: Object): Unit =
        handlers.other()
        value match
          case t: Type   =>
            typeRef(t)
            if t.getSort == Type.OBJECT then literals.push(cls(t.getInternalName)) else literals.effect(0, 1)
          case s: String =>
            literals.effect(0, 1)
            lastConst = s
            stringConstant(s)
          case _ => literals.effect(0, 1)

      /** A string constant the code holds: a class name (a probe), or the end of one (a fragment, which counts if the class loads by name). */
      private def stringConstant(s: String): Unit =
        val internal = ProbeNames.internalName(s)
        if internal != null then probes += Probe.ClassName(internal.nn)
        val suffix = ProbeNames.fragment(s)
        if suffix != null then fragments += suffix.nn

      // Exception types of catch handlers; ASM passes null for `finally`.
      override def visitTryCatchBlock(start: Label, end: Label, handler: Label, tpe: String): Unit =
        if tpe != null then
          crefs.add(cls(tpe)); at(cpos)
          val bit = Guards.bitOf(tpe)
          if bit != 0 then handlers.tryBlock(start, end, handler, bit)

      // A jump target or a try boundary: what comes after is not necessarily what came before, so a literal no longer reaches the next call.
      override def visitLabel(label: Label): Unit =
        literals.clear()
        handlers.label(label)

      // A load pushes a value above a pending literal; a store pops one.
      override def visitVarInsn(op: Int, v: Int): Unit =
        if op >= Opcodes.ILOAD && op <= Opcodes.ALOAD then literals.effect(0, 1)
        else if op >= Opcodes.ISTORE && op <= Opcodes.ASTORE then literals.effect(1, 0)
        else literals.clear()   // ret
        handlers.varInsn(op, v)

      override def visitInsn(op: Int): Unit =
        valueEffect(op)
        if op == Opcodes.ATHROW then handlers.athrow()
        else if op >= Opcodes.IRETURN && op <= Opcodes.RETURN then handlers.branch()
        else handlers.other()

      override def visitJumpInsn(op: Int, label: Label): Unit = { literals.clear(); handlers.branch() }
      override def visitTableSwitchInsn(min: Int, max: Int, dflt: Label, labels: Label*): Unit = { literals.clear(); handlers.branch() }
      override def visitLookupSwitchInsn(dflt: Label, keys: Array[Int], labels: Array[Label]): Unit = { literals.clear(); handlers.branch() }
      override def visitIntInsn(op: Int, operand: Int): Unit =
        if op == Opcodes.NEWARRAY then literals.effect(1, 1) else literals.effect(0, 1)   // bipush, sipush
        handlers.other()
      override def visitIincInsn(v: Int, inc: Int): Unit = handlers.other()

      /** A guard counts only if some block that was open at it does not rethrow what it catches. */
      override def visitEnd(): Unit = handlers.finish(guards.set)

      override def visitMultiANewArrayInsn(desc: String, dims: Int): Unit = { literals.clear(); typeRef(Type.getType(desc)) }

      /** A class constant or array element type; primitives and method types name no class. */
      private def typeRef(t: Type): Unit =
        val base = if t.getSort == Type.ARRAY then t.getElementType else t
        if base.getSort == Type.OBJECT then
          crefs.add(cls(base.getInternalName)); at(cpos)
          guarded(2, crefs.length - 1)

      override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, args: Object*): Unit =
        if !literals.isEmpty then literals.effect(Type.getArgumentTypes(desc).length, if Type.getReturnType(desc) == Type.VOID_TYPE then 0 else 1)
        handlers.other()
        // Linking the call site resolves its descriptor, which loads the functional interface it returns and the types it captures, and the
        // method types and class constants the bootstrap is given (a lambda's erased and instantiated signatures).
        methodTypeRefs(Type.getMethodType(desc))
        // `base + ".kqueue.KQueue"` compiled for Java 9 or later is an indy concatenation whose recipe holds the constant text (\u0001 marks an argument).
        if bsm.getOwner == "java/lang/invoke/StringConcatFactory" && args.nonEmpty then
          args.head match
            case recipe: String => recipe.split("[\u0001\u0002]").foreach(part => if part.nonEmpty then stringConstant(part))
            case _              => ()
        // A lambda or method reference makes an instance of its functional interface (a hidden class implementing it), which inherits the interface's default methods.
        if bsm.getOwner == "java/lang/invoke/LambdaMetafactory" then
          val made = Type.getReturnType(desc)
          if made.getSort == Type.OBJECT then probes += Probe.LambdaInterface(made.getInternalName)
        handle(bsm)
        args.foreach:
          case h: Handle => handle(h)
          case t: Type   => if t.getSort == Type.METHOD then methodTypeRefs(t) else typeRef(t)
          case _         => ()

      private def methodTypeRefs(t: Type): Unit =
        typeRef(t.getReturnType)
        t.getArgumentTypes.foreach(typeRef)

      private def handle(h: Handle): Unit =
        h.getTag match
          case Opcodes.H_GETFIELD | Opcodes.H_GETSTATIC | Opcodes.H_PUTFIELD | Opcodes.H_PUTSTATIC =>
            frefs.add(cls(h.getOwner)); frefs.add(syms.nameId(h.getName).value); frefs.add(syms.descId(h.getDesc).value)
            at(fpos)
            guarded(1, frefs.length / 3 - 1)
          case t => handleTo(h.getOwner, h.getName, h.getDesc, callOf(t == Opcodes.H_INVOKESTATIC), h.isInterface)
