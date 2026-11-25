package reachablelinkage.api

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

import org.objectweb.asm.{ClassReader, ClassVisitor, Opcodes}
import reachablelinkage.fixture.{ScalaFixture, ScalaVersion}

class ScalaNamesSuite extends munit.FunSuite:

  test("methods: constructors, lambdas, extension methods, default arguments, operators") {
    val expected = List(
      "<init>"                      -> "constructor",
      "<clinit>"                    -> "static initializer",
      "$init$"                      -> "trait initializer",
      "$anonfun$bar$1"              -> "anonymous function in bar",
      "$anonfun$bar$1$adapted"      -> "anonymous function in bar",
      "$anonfun$main$2"             -> "anonymous function in main",
      "bar$$anonfun$1"              -> "anonymous function in bar",
      "lambda$run$0"                -> "lambda in run",
      "twice$extension"             -> "twice (extension method)",
      "apply$default$2"             -> "default argument 2 of apply",
      "$lessinit$greater$default$1" -> "default argument 1 of constructor",
      "$plus"                       -> "+",
      "$plus$plus"                  -> "++",
      "$colon$colon"                -> "::",
      "run"                         -> "run",
      "getSomething"                -> "getSomething"
    )
    expected.foreach((raw, shown) => assertEquals(ScalaNames.method(raw), shown, raw))
  }

  test("classes: objects, anonymous classes and functions; Java and plain Scala classes are left alone") {
    val expected = List(
      "app.Foo$"                -> "object app.Foo",
      "app.Foo$Bar$"            -> "object app.Foo.Bar",
      "Foo$"                    -> "object Foo",
      "app.Foo$$anon$1"         -> "anonymous class in app.Foo",
      "app.Foo$$anon$1$$anon$2" -> "anonymous class in app.Foo",
      "app.Foo$$anonfun$bar$1"  -> "anonymous function in app.Foo",
      "app.Foo"                 -> "app.Foo",
      "app.Outer$Inner"         -> "app.Outer$Inner",
      "java.util.Map$Entry"     -> "java.util.Map$Entry",
      "$"                       -> "$"
    )
    expected.foreach((raw, shown) => assertEquals(ScalaNames.clazz(raw), shown, raw))
  }

  test("an inline accessor is recognised") {
    assert(ScalaNames.isInlineAccessor("inline$foo"))
    assert(ScalaNames.isInlineAccessor("foo$inline$bar"))
    assert(!ScalaNames.isInlineAccessor("foo"))
  }

  // The names the real compilers generate, not the ones I remember.
  for v <- Seq(ScalaVersion.V212, ScalaVersion.V213, ScalaVersion.V3Next) do
    test(s"the names scalac ${v} generates are turned into source names") {
      val source = "package app\nobject Foo {\n  def bar(xs: List[Int]): List[Int] = xs.map(_ + 1)\n  def +(x: Int): Int = x\n" +
        "  def withDefault(a: Int = 3): Int = a\n  val anon = new Runnable { def run(): Unit = () }\n}\n" +
        "class Wrap(val v: Int) extends AnyVal {\n  def twice: Int = v * 2\n}\n"
      val dir   = ScalaFixture.compile(v, Map("app/Foo.scala" -> source), Nil)
      val s     = Files.walk(dir)
      val files = try s.iterator.asScala.filter(_.toString.endsWith(".class")).toList
      finally s.close()
      val shown = files.flatMap: p =>
        val reader  = new ClassReader(Files.readAllBytes(p))
        val methods = scala.collection.mutable.ListBuffer.empty[String]
        reader.accept(
          new ClassVisitor(Opcodes.ASM9) {
            override def visitMethod(a: Int, n: String, d: String, sig: String, e: Array[String]) = { methods += n; null }
          },
          ClassReader.SKIP_CODE
        )
        ScalaNames.clazz(reader.getClassName.replace('/', '.')) :: methods.toList.map(ScalaNames.method)
      assert(shown.contains("object app.Foo"), shown.toString)
      assert(shown.contains("anonymous class in app.Foo"), shown.toString)
      assert(shown.contains("anonymous function in bar"), shown.toString)
      assert(shown.contains("+"), shown.toString)
      assert(shown.contains("default argument 1 of withDefault"), shown.toString)
      assert(shown.contains("twice (extension method)"), shown.toString)
    }
