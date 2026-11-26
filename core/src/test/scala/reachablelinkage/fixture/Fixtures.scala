package reachablelinkage.fixture

import java.nio.file.{Path, Paths}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

import reachablelinkage.classpath.ClassSource

/** Builds tiny class files in memory so tests do not need a compiler. */
object Fixtures:
  final class MemorySource(classes: Map[String, Array[Byte]]) extends ClassSource:
    val origin: Path                         = Paths.get("memory")
    def classNames: Iterator[String]         = classes.keysIterator
    def read(n: String): Option[Array[Byte]] = classes.get(n)
    def close(): Unit                        = ()

  type Body = MethodVisitor => Unit

  def cls(
      name: String,
      sup: String = "java/lang/Object",
      itfs: Seq[String] = Nil,
      access: Int = Opcodes.ACC_PUBLIC,
      methods: Seq[(String, String, Int, Option[Body])] = Nil,
      fields: Seq[(String, String)] = Nil
  ): (String, Array[Byte]) =
    val w = new ClassWriter(0)
    w.visit(Opcodes.V17, access, name, null, sup, itfs.toArray)
    fields.foreach((n, d) => w.visitField(Opcodes.ACC_PUBLIC, n, d, null, null).visitEnd())
    methods.foreach: (n, d, acc, body) =>
      val mv = w.visitMethod(acc, n, d, null, null)
      body.foreach: b =>
        mv.visitCode(); b(mv); mv.visitMaxs(8, 8)
      mv.visitEnd()
    w.visitEnd()
    name -> w.toByteArray

  def concrete(name: String, desc: String = "()V"): (String, String, Int, Option[Body]) =
    (name, desc, Opcodes.ACC_PUBLIC, Some(_.visitInsn(Opcodes.RETURN)))

  def calling(name: String)(emit: Body): (String, String, Int, Option[Body]) =
    (name, "()V", Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, Some(mv => { emit(mv); mv.visitInsn(Opcodes.RETURN) }))

  def invokeStatic(owner: String, n: String, d: String): Body =
    _.visitMethodInsn(Opcodes.INVOKESTATIC, owner, n, d, false)
  def invokeVirtual(owner: String, n: String, d: String): Body =
    mv => { mv.visitInsn(Opcodes.ACONST_NULL); mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, n, d, false) }
  def getStatic(owner: String, n: String, d: String): Body =
    mv => { mv.visitFieldInsn(Opcodes.GETSTATIC, owner, n, d); mv.visitInsn(Opcodes.POP) }

  def source(cs: (String, Array[Byte])*): ClassSource = MemorySource(cs.toMap)
