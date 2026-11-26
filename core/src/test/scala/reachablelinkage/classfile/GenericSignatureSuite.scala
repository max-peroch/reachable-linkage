package reachablelinkage.classfile

import java.nio.file.Files

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.symbols.{ClassId, Symbols}

/** The classes only a generic `Signature` names: `List<Missing>` is `List` in the descriptor, and `Missing` only in the signature, which reflection on the
  * member's generic types loads.
  */
class GenericSignatureSuite extends munit.FunSuite:

  private val dir = compile(
    List("Bound", "Arg", "InnerArg", "Wild", "MethodBound", "Key", "Val").map(n => s"p/$n.java" -> s"package p; public class $n { }").toMap ++ Map(
      "p/Outer.java"  -> "package p; public class Outer<A> { public class Inner<B> { } }",
      "p/Holder.java" ->
        """package p;
        |import java.util.*;
        |public class Holder<T extends p.Bound> {
        |  public Map<String, p.Arg> field;
        |  public Outer<String>.Inner<p.InnerArg> inner;
        |  public List<? extends p.Wild> wild;
        |  public int plain;
        |  public String erased;
        |  public <U extends p.MethodBound> List<U> method(Map.Entry<p.Key, p.Val> e, T t) { return null; }
        |  public void noGenerics(String s) { }
        |}
        |""".stripMargin
    )
  )

  private val syms = new Symbols
  private val info = ClassParser.parse(Files.readAllBytes(dir.resolve("p/Holder.class")), syms).fold(e => fail(e.message), identity)

  /** The classes recorded for the member `name` of `kind` (0 a method, 1 a field), by name. */
  private def namedFor(kind: Int, name: String): Set[String] =
    val member = (if kind == 0 then info.methods else info.fields).indexWhere(m => syms.name(m.name) == name)
    assert(member >= 0, s"no member $name")
    info.signatureTypes.grouped(3).collect { case Array(k, m, c) if k == kind && m == member => syms.className(ClassId(c)) }.toSet

  test("a field's type arguments are recorded, with the generic type itself") {
    assertEquals(namedFor(1, "field"), Set("java/util/Map", "java/lang/String", "p/Arg"))
  }

  test("the inner class of a generic outer class is Outer$Inner, and so are the arguments of both") {
    assertEquals(namedFor(1, "inner"), Set("p/Outer", "java/lang/String", "p/Outer$Inner", "p/InnerArg"))
  }

  test("a wildcard bound is recorded") {
    assertEquals(namedFor(1, "wild"), Set("java/util/List", "p/Wild"))
  }

  test("a method records its parameters, its result and the bounds of its type variables; a type variable names no class") {
    assertEquals(namedFor(0, "method"), Set("p/MethodBound", "java/util/List", "java/util/Map$Entry", "p/Key", "p/Val"))
  }

  test("a member with no generic signature records nothing (the erased descriptor already names its classes)") {
    assertEquals(namedFor(1, "plain"), Set.empty[String])
    assertEquals(namedFor(1, "erased"), Set.empty[String])
    assertEquals(namedFor(0, "noGenerics"), Set.empty[String])
  }

  test("a missing type argument is reported against the member that has it, for a field, a method and a constructor, and only for members a rule fires for") {
    import reachablelinkage.classpath.{ClassIndex, ClassSource}
    import reachablelinkage.fixture.{GoneLibrary, Scopes}
    import reachablelinkage.fixture.JavaFixture.source
    import reachablelinkage.linkage.{CheckOptions, Checker, Conflict, Keep, Precision}
    val (_, v2) = GoneLibrary.build(
      Map(
        "lib/Wired.java" -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Wired { }",
        "lib/Dep.java"   -> "package lib; public class Dep { }",
        "lib/Impl.java"  ->
          """package lib;
            |import java.util.List;
            |public class Impl {
            |  @Wired public List<Dep> field;
            |  public List<Dep> unwiredField;
            |  @Wired public Impl(List<Dep> constructorArg) { }
            |  @Wired public void method(List<Dep> methodArg) { }
            |  public void unwiredMethod(List<Dep> other) { }
            |}
            |""".stripMargin
      ),
      removed = Set("lib/Dep")
    )
    val syms  = new Symbols
    val index = ClassIndex(Vector(source(v2), ClassSource.jdk()), syms)
    val scope = Scopes.of(index, Vector(0), Vector(0), Vector(0))
    val named = Checker
      .findings(index, scope, CheckOptions(Precision.Class, Seq(Keep.Members(Keep.classNames("lib/Wired")))))
      .collect:
        case f if f.conflict.isInstanceOf[Conflict.MissingSignatureType] =>
          val Conflict.MissingSignatureType(_, name, _, missing) = f.conflict: @unchecked
          (syms.name(name), syms.className(missing))
    assertEquals(named.toSet, Set(("field", "lib/Dep"), ("<init>", "lib/Dep"), ("method", "lib/Dep")))
  }
