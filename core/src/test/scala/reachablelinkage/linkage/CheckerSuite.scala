package reachablelinkage.linkage

import reachablelinkage.fixture.Scopes
import org.objectweb.asm.Opcodes
import reachablelinkage.fixture.Fixtures.*

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.Fixtures
import reachablelinkage.symbols.Symbols

class CheckerSuite extends munit.FunSuite:

  /** Checks app (source 0) against the libs and the JDK; returns rendered conflicts. */
  private def run(app: ClassSource, libs: ClassSource*): List[String] =
    val syms  = new Symbols
    val index = ClassIndex(app +: (libs :+ ClassSource.jdk()).toIndexedSeq, syms)
    val libIdx = 1 to libs.length
    Checker.check(index, Scopes.of(index, Vector(0), Vector(0), Vector(0) ++ libIdx)).map(_.show(syms)).toList

  private val appCallingFoo =
    source(cls("app/Main", methods = Seq(calling("run")(invokeStatic("lib/Foo", "bar", "()V")))))

  test("a method present in the library links") {
    val lib = source(cls("lib/Foo", methods = Seq(("bar", "()V", Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, Some(_.visitInsn(Opcodes.RETURN))))))
    assertEquals(run(appCallingFoo, lib), Nil)
  }

  test("a method removed in the library is reported") {
    val lib = source(cls("lib/Foo"))
    assertEquals(run(appCallingFoo, lib), List("app.Main: method lib.Foo.bar()V is missing"))
  }

  test("a class removed from the library is reported") {
    assertEquals(run(appCallingFoo), List("app.Main: class lib.Foo is missing"))
  }

  test("a method inherited from a superclass resolves") {
    val app = source(cls("app/Main", methods = Seq(calling("run")(invokeVirtual("lib/Child", "hello", "()V")))))
    val lib = source(cls("lib/Base", methods = Seq(concrete("hello"))), cls("lib/Child", sup = "lib/Base"))
    assertEquals(run(app, lib), Nil)
  }

  test("a method inherited from a superinterface resolves") {
    val app = source(cls("app/Main", methods = Seq(calling("run")(invokeVirtual("lib/Impl", "hi", "()V")))))
    val lib = source(
      cls("lib/Api", access = Opcodes.ACC_PUBLIC | Opcodes.ACC_INTERFACE | Opcodes.ACC_ABSTRACT,
        methods = Seq(("hi", "()V", Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT, None))),
      cls("lib/Impl", access = Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT, itfs = Seq("lib/Api"))
    )
    assertEquals(run(app, lib), Nil)
  }

  test("a changed descriptor is reported") {
    val app = source(cls("app/Main", methods = Seq(calling("run")(invokeVirtual("lib/Foo", "hello", "()V")))))
    val lib = source(cls("lib/Foo", methods = Seq(concrete("hello", "(I)V"))))
    assertEquals(run(app, lib), List("app.Main: method lib.Foo.hello()V is missing"))
  }

  test("a missing field is reported, a present one links") {
    val app = source(cls("app/Main", methods = Seq(calling("run")(getStatic("lib/Foo", "X", "I")))))
    assertEquals(run(app, source(cls("lib/Foo"))), List("app.Main: field lib.Foo.X:I is missing"))
    assertEquals(run(app, source(cls("lib/Foo", fields = Seq(("X", "I"))))), Nil)
  }

  test("a missing superclass is reported") {
    val app = source(cls("app/Main", sup = "lib/Gone"))
    assertEquals(run(app), List("app.Main: class lib.Gone is missing"))
  }

  test("a lookup through a hierarchy with a hole reports the missing class, not the method") {
    val app = source(cls("app/Main", methods = Seq(calling("run")(invokeVirtual("lib/Child", "hello", "()V")))))
    val lib = source(cls("lib/Child", sup = "lib/Gone"))
    assertEquals(run(app, lib), List("app.Main: class lib.Gone is missing"))
  }

  test("earlier classpath entries shadow later ones") {
    val v2 = source(cls("lib/Foo"))
    val v1 = source(cls("lib/Foo", methods = Seq(("bar", "()V", Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, Some(_.visitInsn(Opcodes.RETURN))))))
    assertEquals(run(appCallingFoo, v2, v1), List("app.Main: method lib.Foo.bar()V is missing"))
  }

  test("JDK members resolve through the jrt source") {
    val app = source(cls("app/Main", methods = Seq(calling("run")(invokeVirtual("java/util/ArrayList", "size", "()I")))))
    assertEquals(run(app), Nil)
  }

  test("signature-polymorphic MethodHandle.invoke links with any descriptor") {
    val app = source(cls("app/Main", methods = Seq(calling("run")(invokeVirtual("java/lang/invoke/MethodHandle", "invoke", "(IJ)Ljava/lang/String;")))))
    assertEquals(run(app), Nil)
  }
