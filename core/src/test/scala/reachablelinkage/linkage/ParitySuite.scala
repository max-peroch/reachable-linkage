package reachablelinkage.linkage

import reachablelinkage.fixture.Scopes
import org.objectweb.asm.Opcodes
import reachablelinkage.fixture.Fixtures.*
import reachablelinkage.fixture.MissinglinkOracle.Key

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.{Fixtures, MissinglinkOracle, OracleSuite}
import reachablelinkage.symbols.Symbols

/** Parity: on every fixture our Class-precision conflicts must match missinglink-core's. Each fixture runs in App scope (check only the app) and Full scope
  * (check reachable dependencies).
  *
  * Known, intended deviations are listed per fixture in `extra` (we report more) and `viaGap` (ML says "method missing" where the hierarchy is broken; we name
  * the missing class instead).
  */
class ParitySuite extends OracleSuite:

  private val PubStatic = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC
  private val Iface     = Opcodes.ACC_PUBLIC | Opcodes.ACC_INTERFACE | Opcodes.ACC_ABSTRACT

  private def staticNoop(n: String) = (n, "()V", PubStatic, Some((mv: org.objectweb.asm.MethodVisitor) => mv.visitInsn(Opcodes.RETURN)))
  private def main(body: Body*)     = cls("app/Main", methods = Seq(calling("run")(mv => body.foreach(_(mv)))))

  private final case class Fixture(
      name: String,
      app: ClassSource,
      libs: Seq[ClassSource],
      extra: Set[Key] = Set.empty,
      viaGap: Boolean = false
  )

  private val fixtures: Seq[Fixture] = Seq(
    Fixture("method present", source(main(invokeStatic("lib/Foo", "bar", "()V"))), Seq(source(cls("lib/Foo", methods = Seq(staticNoop("bar")))))),
    Fixture("method removed", source(main(invokeStatic("lib/Foo", "bar", "()V"))), Seq(source(cls("lib/Foo")))),
    Fixture("class removed", source(main(invokeStatic("lib/Foo", "bar", "()V"))), Nil),
    Fixture(
      "descriptor changed",
      source(main(invokeVirtual("lib/Foo", "hello", "()V"))),
      Seq(source(cls("lib/Foo", methods = Seq(concrete("hello", "(I)V")))))
    ),
    Fixture("static became instance", source(main(invokeStatic("lib/Foo", "bar", "()V"))), Seq(source(cls("lib/Foo", methods = Seq(concrete("bar")))))),
    Fixture("instance became static", source(main(invokeVirtual("lib/Foo", "bar", "()V"))), Seq(source(cls("lib/Foo", methods = Seq(staticNoop("bar")))))),
    Fixture(
      "inherited from superclass",
      source(main(invokeVirtual("lib/Child", "hello", "()V"))),
      Seq(source(cls("lib/Base", methods = Seq(concrete("hello"))), cls("lib/Child", sup = "lib/Base")))
    ),
    Fixture(
      "removed from superclass",
      source(main(invokeVirtual("lib/Child", "hello", "()V"))),
      Seq(source(cls("lib/Base"), cls("lib/Child", sup = "lib/Base")))
    ),
    Fixture(
      "inherited from interface",
      source(main(invokeVirtual("lib/Impl", "hi", "()V"))),
      Seq(
        source(
          cls("lib/Api", access = Iface, methods = Seq(("hi", "()V", Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT, None))),
          cls("lib/Impl", access = Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT, itfs = Seq("lib/Api"))
        )
      )
    ),
    Fixture("field present", source(main(getStatic("lib/Foo", "X", "I"))), Seq(source(cls("lib/Foo", fields = Seq(("X", "I")))))),
    Fixture("field removed", source(main(getStatic("lib/Foo", "X", "I"))), Seq(source(cls("lib/Foo")))),
    Fixture("field type changed", source(main(getStatic("lib/Foo", "X", "I"))), Seq(source(cls("lib/Foo", fields = Seq(("X", "J")))))),
    Fixture(
      "first classpath entry wins",
      source(main(invokeStatic("lib/Foo", "bar", "()V"))),
      Seq(source(cls("lib/Foo")), source(cls("lib/Foo", methods = Seq(staticNoop("bar")))))
    ),
    // ML flags the missing parent only through the constructor call; we also flag it as a supertype.
    Fixture(
      "missing superclass",
      source(
        cls(
          "app/Main",
          sup = "lib/Gone",
          methods = Seq(
            (
              "<init>",
              "()V",
              Opcodes.ACC_PUBLIC,
              Some((mv: org.objectweb.asm.MethodVisitor) => {
                mv.visitVarInsn(Opcodes.ALOAD, 0)
                mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lib/Gone", "<init>", "()V", false)
                mv.visitInsn(Opcodes.RETURN)
              })
            )
          )
        )
      ),
      Nil
    ),
    Fixture("hole in hierarchy", source(main(invokeVirtual("lib/Child", "hello", "()V"))), Seq(source(cls("lib/Child", sup = "lib/Gone"))), viaGap = true),
    // Transitive: app -> libA -> libB, where libB lost the method. Only Full scope can see it.
    Fixture(
      "transitive break",
      source(main(invokeStatic("liba/A", "go", "()V"))),
      Seq(source(cls("liba/A", methods = Seq(calling("go")(invokeStatic("libb/B", "gone", "()V"))))), source(cls("libb/B")))
    ),
    // Broken call in a dependency class nothing reaches: must not be reported by either tool.
    Fixture(
      "unreachable dependency class",
      source(main(invokeStatic("liba/A", "ok", "()V"))),
      Seq(
        source(cls("liba/A", methods = Seq(staticNoop("ok"))), cls("liba/Dead", methods = Seq(calling("boom")(invokeStatic("libb/B", "gone", "()V"))))),
        source(cls("libb/B"))
      )
    ),
    Fixture("JDK call", source(main(invokeVirtual("java/util/ArrayList", "size", "()I"))), Nil),
    Fixture("JDK method missing", source(main(invokeVirtual("java/util/ArrayList", "nope", "()I"))), Nil),
    Fixture(
      "interface missing is also flagged by us",
      source(cls("app/Main", itfs = Seq("lib/Gone"))),
      Nil,
      extra = Set(Key("CLASS_NOT_FOUND", "app/Main", "lib/Gone", ""))
    )
  )

  for f <- fixtures; scan <- Seq(false, true) do
    test(s"${f.name} [${if scan then "Full" else "App"} scope]") {
      val expected = MissinglinkOracle.check(f.app, f.libs, scan)

      val syms  = new Symbols
      val index = ClassIndex((f.app +: f.libs :+ ClassSource.jdk()).toIndexedSeq, syms)
      val libs  = 1 to f.libs.length
      val scope = Scopes.of(index, Vector(0), if scan then Vector(0) ++ libs else Vector(0), Vector(0) ++ libs)
      val ours  = Checker.check(index, scope).flatMap(MissinglinkOracle.normalise(_, syms)).toSet

      val gapCovered =
        (k: Key) => f.viaGap && k.category == "METHOD_SIGNATURE_NOT_FOUND" && ours.exists(o => o.category == "CLASS_NOT_FOUND" && o.from == k.from)
      val missed = expected.filterNot(ours.contains).filterNot(gapCovered)
      val added  = (ours -- expected).filterNot(k => f.extra.contains(k) || (f.viaGap && k.category == "CLASS_NOT_FOUND"))
      assertEquals(missed, Set.empty[Key], "conflicts missinglink reports that we miss")
      assertEquals(added, Set.empty[Key], "conflicts we report that missinglink does not")
    }

  test("oracle sanity: missinglink reports real conflicts, and scope changes the result") {
    def run(name: String, scan: Boolean): Set[Key] =
      val f = fixtures.find(_.name == name).get
      MissinglinkOracle.check(f.app, f.libs, scan)
    assert(run("method removed", false).nonEmpty)
    assert(run("static became instance", false).nonEmpty)
    assert(run("transitive break", false).isEmpty)
    assert(run("transitive break", true).nonEmpty)
    assert(run("unreachable dependency class", true).isEmpty)
  }
