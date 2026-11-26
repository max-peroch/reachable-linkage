package reachablelinkage.linkage

import java.net.URLClassLoader
import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, Scopes}
import reachablelinkage.symbols.Symbols

/** A serializable lambda (every Scala 2.12+ function literal, and a Java one cast to `Serializable`) is rebuilt by the JDK through the class's
  * `$deserializeLambda$`, with no call to the code that created it. The check roots that method after the direct walk, so the lambda body is `Deserialized`: a
  * real path, but one that rests on deserialization, not on a call or a rule.
  */
class SerializedLambdaSuite extends munit.FunSuite:

  private val maker = "lib/Maker.java" ->
    "package lib; public class Maker { public static Runnable make() { return (Runnable & java.io.Serializable) () -> Gone.x(); } public static void unrelated() { } }"

  // The producer serialises the lambda; the consumer only reads and runs one, and names the class without calling make().
  private val producer =
    "package app; public class Main { public static byte[] save() throws Exception { var b = new java.io.ByteArrayOutputStream(); " +
      "try (var o = new java.io.ObjectOutputStream(b)) { o.writeObject(lib.Maker.make()); } return b.toByteArray(); } }"
  private def consumer(extra: String) =
    "package app; public class Main { public static void load(byte[] b) throws Exception { Object k = lib.Maker.class; " + extra +
      "try (var i = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(b))) { ((Runnable) i.readObject()).run(); } } }"

  private def labels(app: Path, lib: Path, precision: Precision): Vector[(String, Label)] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val locator = Locator(index)
    // javac names a serializable lambda `lambda$make$<hash>$1`, a plain one `lambda$make$0`: keep what they share.
    Checker.findings(index, scope, precision).map(f => (locator.locate(f).methodName.split('$').take(2).mkString("$"), f.label)).distinct.sortBy(_.toString)

  private def loader(paths: Path*) = new URLClassLoader(paths.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)

  test("a lambda body only deserialisation reaches is Deserialized, and the JVM really runs it after deserialising (the control works, the break fails)") {
    val (v1, v2)    = GoneLibrary.build(Map(maker))
    val producerApp = compile(Map("app/Main.java" -> producer), Seq(v1))
    val consumerApp = compile(Map("app/Main.java" -> consumer("")), Seq(v1))

    val bytes = {
      val l = loader(producerApp, v1);
      try l.loadClass("app.Main").getMethod("save").invoke(null).asInstanceOf[Array[Byte]]
      finally l.close()
    }
    def runLoad(lib: Path): Option[Throwable] =
      val l = loader(consumerApp, lib)
      try { l.loadClass("app.Main").getMethod("load", classOf[Array[Byte]]).invoke(null, bytes); None }
      catch case e: java.lang.reflect.InvocationTargetException => Some(e.getCause)
      finally l.close()
    assertEquals(runLoad(v1), None, "the control: against the library it was serialised with, it works")
    assert(runLoad(v2).exists(_.isInstanceOf[NoSuchMethodError]), s"expected NoSuchMethodError, got ${runLoad(v2)}")

    for precision <- Seq(Precision.Method, Precision.MethodRta) do
      assertEquals(labels(consumerApp, v2, precision), Vector(("lambda$make", Label.Deserialized)), precision.toString)
  }

  test("the same lambda called directly is Reachable, and unreached by anything is Latent") {
    val (_, v2) = GoneLibrary.build(Map(maker))
    val (v1, _) = GoneLibrary.build(Map(maker))
    val direct  = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Maker.make().run(); } }"), Seq(v1))
    assertEquals(labels(direct, v2, Precision.Method), Vector(("lambda$make", Label.Reachable)))
    // A class with no `$deserializeLambda$` (no serializable lambda) has nothing to root, so its lambda stays Latent.
    val plain    = "lib/Maker.java" -> "package lib; public class Maker { public static Runnable make() { return () -> Gone.x(); } }"
    val (p1, p2) = GoneLibrary.build(Map(plain))
    val naming   = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { Object k = lib.Maker.class; } }"), Seq(p1))
    assertEquals(labels(naming, p2, Precision.Method), Vector(("lambda$make", Label.Latent)))
  }

  test("the default FailOn does not fail on a Deserialized conflict, FailOn.ANY does, and the report ranks it") {
    import reachablelinkage.api.*
    val (v1, v2)    = GoneLibrary.build(Map(maker))
    val consumerApp = compile(Map("app/Main.java" -> consumer("")), Seq(v1))
    val r           = Linkage.check(
      LinkageRequest(
        java.util.List.of(consumerApp),
        java.util.List.of(ClasspathEntry("lib", reachablelinkage.fixture.Jars.of(v2))),
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.METHOD,
        java.util.Optional.empty[Path](),
        KeepRule.standard()
      )
    )
    assertEquals(r.conflicts.size, 1)
    assertEquals(r.conflicts.get(0).label, ConflictLabel.DESERIALIZED)
    assertEquals(r.failing().size, 0)
    assertEquals(r.failing(FailOn.DIRECT).size, 0)
    assertEquals(r.failing(FailOn.ANY).size, 1)
    assert(ConflictLabel.PROBED.severity() > ConflictLabel.DESERIALIZED.severity())
    assert(ConflictLabel.DESERIALIZED.severity() > ConflictLabel.GUARDED.severity())
  }
