package reachablelinkage.classfile

import java.nio.file.Files

import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.symbols.{ClassId, NameId, Symbols}

/**
 * A class literal (`Foo.class`) that is still on the stack when a call is made is an argument of that call (or its receiver): a framework
 * that is handed the class (`mapper.readValue(json, Foo.class)`) builds it and calls its accessors. Only what really reaches the call counts.
 */
class LiteralArgumentsSuite extends munit.FunSuite:

  private val dir = compile(
    List("Foo", "Bar", "Baz", "A", "B", "Qux").map(n => s"p/$n.java" -> s"package p; public class $n { }").toMap ++ Map(
      "p/User.java" ->
        """package p;
          |public class User {
          |  static void sinkDirect(Class<?> c) { }
          |  static void sinkConstants(String s, Class<?> c, int n) { }
          |  static void sinkStored(Class<?> c) { }
          |  static void sinkConditional(Class<?> c) { }
          |  static void sinkAfterCall(Class<?> c) { }
          |  static void sinkPrimitive(Class<?> c) { }
          |  static void sinkArray(Class<?> c) { }
          |  static void sinkVarargs(Class<?>... cs) { }
          |  static void sinkFirst(Class<?> c, String s, int n) { }
          |  static void sinkNested(Class<?> c) { }
          |  static Class<?> wrap(Class<?> c) { return c; }
          |  static void sinkMiddle(String s, Class<?> c, Object o) { }
          |  static Object helper() { return null; }
          |  static void sinkId(Class<?> c, Object o) { }
          |  static String id(String s) { return s; }
          |  static void sinkBox(Object o) { }
          |  static void sinkLength(int n) { }
          |  static class Box { Box(Class<?> c) { } }
          |  static boolean cond() { return true; }
          |  void direct()        { sinkDirect(Foo.class); }
          |  void constants()     { sinkConstants("x", Bar.class, 3); }
          |  void receiver()      { String n = Qux.class.getName(); }
          |  void stored()        { Class<?> c = Baz.class; sinkStored(c); }
          |  void conditional()   { sinkConditional(cond() ? A.class : B.class); }
          |  void afterCall()     { Class<?> c = Foo.class; cond(); sinkAfterCall(null); }
          |  void primitive()     { sinkPrimitive(int.class); }
          |  void array()         { sinkArray(String[].class); }
          |  void varargs()       { sinkVarargs(Foo.class, Bar.class); }
          |  void first()         { sinkFirst(Foo.class, "x", 3); }
          |  void nested()        { sinkNested(wrap(Qux.class)); }
          |  void middle()        { sinkMiddle("x", Baz.class, helper()); }
          |  void inner()         { sinkId(Foo.class, id("x")); }
          |  void boxed()         { sinkBox(new Box(Bar.class)); }
          |  void notAnObject()   { sinkLength(Baz.class.getModifiers()); }
          |}
          |""".stripMargin))

  private val syms = new Symbols
  private val info = ClassParser.parse(Files.readAllBytes(dir.resolve("p/User.class")), syms).fold(e => fail(e.message), identity)

  /** The `(owner.method, literal)` pairs recorded for calls of the method `callee`, by name. */
  private def recordedFor(callee: String): Set[(String, String)] =
    info.literalArgs.grouped(3).collect:
      case Array(owner, method, literal) if syms.name(NameId(method)) == callee =>
        (s"${syms.className(ClassId(owner))}.$callee", syms.className(ClassId(literal)))
    .toSet

  test("a literal that is the last argument of a call is recorded against it") {
    assertEquals(recordedFor("sinkDirect"), Set(("p/User.sinkDirect", "p/Foo")))
  }

  test("constants after the literal do not separate it from the call") {
    assertEquals(recordedFor("sinkConstants"), Set(("p/User.sinkConstants", "p/Bar")))
  }

  test("a literal that is the receiver of a call is recorded too (the rule says which calls matter)") {
    assertEquals(recordedFor("getName"), Set(("java/lang/Class.getName", "p/Qux")))
  }

  test("a literal stored in a local first, or chosen by a conditional, is not tracked into the call") {
    assertEquals(recordedFor("sinkStored"), Set.empty[(String, String)])
    assertEquals(recordedFor("sinkConditional"), Set.empty[(String, String)])
  }

  test("another call between the literal and the call ends it") {
    assertEquals(recordedFor("sinkAfterCall"), Set.empty[(String, String)])
  }

  test("a primitive or array class literal is not a class argument") {
    assertEquals(recordedFor("sinkPrimitive"), Set.empty[(String, String)])
    assertEquals(recordedFor("sinkArray"), Set.empty[(String, String)])
  }

  test("a literal that is not the last argument still reaches its call") {
    assertEquals(recordedFor("sinkFirst"), Set(("p/User.sinkFirst", "p/Foo")))
  }

  test("literals stored in a varargs array are arguments of the call the array goes to") {
    assertEquals(recordedFor("sinkVarargs"), Set(("p/User.sinkVarargs", "p/Foo"), ("p/User.sinkVarargs", "p/Bar")))
  }

  test("a call in between takes only the values it pops: a literal under its arguments goes on to the next call") {
    // `helper()` is called after the literal and pushes a value above it; the literal is `sinkMiddle`'s, not `helper`'s.
    assertEquals(recordedFor("sinkMiddle"), Set(("p/User.sinkMiddle", "p/Baz")))
    assertEquals(recordedFor("helper"), Set.empty[(String, String)])
  }

  test("a call that returns an object stands for what it was given: the call that takes the result is told of the literal too") {
    assertEquals(recordedFor("wrap"), Set(("p/User.wrap", "p/Qux")))
    assertEquals(recordedFor("sinkNested"), Set(("p/User.sinkNested", "p/Qux")))
  }

  test("a constructor leaves the new object carrying its arguments") {
    assertEquals(recordedFor("<init>").filter(_._2 == "p/Bar"), Set(("p/User$Box.<init>", "p/Bar")))
    assertEquals(recordedFor("sinkBox"), Set(("p/User.sinkBox", "p/Bar")))
  }

  test("a primitive result is not about the literal: it does not carry") {
    assertEquals(recordedFor("getModifiers"), Set(("java/lang/Class.getModifiers", "p/Baz")))
    assertEquals(recordedFor("sinkLength"), Set.empty[(String, String)])
  }

  test("a constant loaded after the literal is a value too: the call that takes only it does not take the literal") {
    // Foo.class, then "x" for `id`: `id` pops one value (the string), so the literal is `sinkId`'s.
    assertEquals(recordedFor("id"), Set.empty[(String, String)])
    assertEquals(recordedFor("sinkId"), Set(("p/User.sinkId", "p/Foo")))
  }

