package reachablelinkage.linkage

import java.nio.file.Path

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{GoneLibrary, RuntimeOracle, Scopes}
import reachablelinkage.linkage.RuleNames.*
import reachablelinkage.symbols.Symbols

/**
 * User-supplied keep rules on custom annotations, with a small reflective container in the library
 * standing in for a framework. Each scenario runs on the JVM: with the right rule a real failure is
 * Reachable; without it the same failure is missed, which is what a rule is for.
 */
class KeepRulesSuite extends munit.FunSuite:

  private type Row = (String, String, Label)
  private val Latent    = Label.Latent

  private val annotations = Map(
    "lib/Wired.java"  -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Wired { }",
    "lib/Entity.java" -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Entity { }",
    "lib/Uses.java"   -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Uses { Class<?> value(); }")

  // A framework in miniature: builds a class through its no-argument constructor and calls its @Wired methods.
  private val container = "lib/Container.java" ->
    """package lib;
      |public class Container {
      |  public static Object start(Class<?> type) {
      |    try {
      |      Object o = type.getDeclaredConstructor().newInstance();
      |      for (java.lang.reflect.Method m : type.getDeclaredMethods()) if (m.isAnnotationPresent(Wired.class)) m.invoke(o);
      |      return o;
      |    } catch (java.lang.reflect.InvocationTargetException e) {
      |      if (e.getCause() instanceof Error) throw (Error) e.getCause();
      |      throw new RuntimeException(e);
      |    } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }
      |  }
      |  public static Object startUsed(Class<?> api) { return start(api.getAnnotation(Uses.class).value()); }
      |}
      |""".stripMargin

  // A framework outside the classpath that calls the methods of a supertype: it builds the class and finds `handle` by a name it computes
  // at run time, so no constant and no call in the classpath names it. Only an `Overrides` rule can see that it is called.
  private val handlers = Map(
    "lib/Handler.java" -> "package lib; public abstract class Handler { public abstract void handle(); private void secret() { } }",
    "lib/Mid.java"     -> "package lib; public abstract class Mid extends Handler { public void helper() { Gone.x(); } }",
    "lib/HImpl.java"   -> "package lib; public class HImpl extends Mid { public HImpl() { Gone.x(); } public void handle() { Gone.x(); } public void other() { Gone.x(); } public void secret() { Gone.x(); } }",
    "lib/Server.java"  ->
      """package lib;
        |public class Server {
        |  public static void start(Class<?> type) {
        |    try {
        |      Object o = type.getDeclaredConstructor().newInstance();
        |      type.getMethod(String.join("", "han", "dle")).invoke(o);
        |    } catch (java.lang.reflect.InvocationTargetException e) {
        |      if (e.getCause() instanceof Error) throw (Error) e.getCause();
        |      throw new RuntimeException(e);
        |    } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }
        |  }
        |}
        |""".stripMargin)

  // A binder in miniature: handed a class, it builds it and calls its setters, by names it finds at run time. The application's own
  // call `Binder.bind(Dto.class)` is the only mention of the class; a rule says what that call means.
  private val beans = Map(
    "lib/Binder.java" ->
      """package lib;
        |public class Binder {
        |  public static Object bind(Class<?> type) {
        |    try {
        |      Object o = type.getDeclaredConstructor().newInstance();
        |      for (java.lang.reflect.Method m : type.getMethods()) if (m.getName().startsWith("set") && m.getParameterCount() == 1) m.invoke(o, (Object) null);
        |      return o;
        |    } catch (java.lang.reflect.InvocationTargetException e) {
        |      if (e.getCause() instanceof Error) throw (Error) e.getCause();
        |      throw new RuntimeException(e);
        |    } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }
        |  }
        |}
        |""".stripMargin,
    "lib/Base.java" -> "package lib; public class Base { public void setBase(String b) { Gone.x(); } }",
    "lib/Dto.java" ->
      """package lib;
        |public class Dto extends Base {
        |  public Dto() { Gone.x(); }
        |  public void setA(String a) { Gone.x(); }
        |  public String getA() { Gone.x(); return null; }
        |  public boolean isOn() { Gone.x(); return false; }
        |  public void other() { Gone.x(); }
        |  public static void setStatic(String s) { Gone.x(); }
        |  void setHidden(String s) { Gone.x(); }
        |  public void get() { Gone.x(); }
        |}
        |""".stripMargin)

  // A library that reads a class name from `reference.conf` and builds it: the name is a run-time value, so no instruction names the class.
  private val configured = Map(
    "lib/Loader.java" ->
      """package lib;
        |public class Loader {
        |  public static void loadConfigured() {
        |    try {
        |      String text = new String(Loader.class.getResourceAsStream("/reference.conf").readAllBytes());
        |      Class<?> c = Class.forName(text.split("\"")[1]);
        |      c.getDeclaredConstructor().newInstance();
        |    } catch (java.lang.reflect.InvocationTargetException e) {
        |      if (e.getCause() instanceof Error) throw (Error) e.getCause();
        |      throw new RuntimeException(e);
        |    } catch (Exception e) { throw new RuntimeException(e); }
        |  }
        |}
        |""".stripMargin,
    "lib/Plugin.java" -> "package lib; public class Plugin { public Plugin() { Gone.x(); } public void other() { Gone.x(); } }",
    "lib/Decoy.java" -> "package lib; public class Decoy { public Decoy() { Gone.x(); } }")
  private val confFile = Map("reference.conf" -> "plugin = \"lib.Plugin\"")

  // An actor system in miniature: handed a class and arguments, it picks the constructor that takes that many.
  private val spawner = Map(
    "lib/Spawner.java" ->
      """package lib;
        |public class Spawner {
        |  public static Object spawn(Class<?> type, Object... args) {
        |    try {
        |      for (java.lang.reflect.Constructor<?> c : type.getDeclaredConstructors()) if (c.getParameterCount() == args.length) return c.newInstance(args);
        |      throw new IllegalArgumentException("no constructor takes " + args.length);
        |    } catch (java.lang.reflect.InvocationTargetException e) {
        |      if (e.getCause() instanceof Error) throw (Error) e.getCause();
        |      throw new RuntimeException(e);
        |    } catch (ReflectiveOperationException e) { throw new RuntimeException(e); }
        |  }
        |}
        |""".stripMargin,
    // Only the constructor with an argument is broken: a framework that uses the no-argument one would not notice.
    "lib/Worker.java" -> "package lib; public class Worker { public Worker() { } public Worker(String s) { Gone.x(); } public void other() { Gone.x(); } }")

  // A loader that builds the name from its own package and a constant tail, the way netty's transports are chosen: no constant names the class.
  private val fragments = Map(
    "lib/Loader.java" ->
      """package lib;
        |public class Loader {
        |  public static void load() {
        |    try { Class.forName(Loader.class.getPackageName() + ".impl.Plugin").getDeclaredConstructor().newInstance(); }
        |    catch (java.lang.reflect.InvocationTargetException e) { if (e.getCause() instanceof Error) throw (Error) e.getCause(); throw new RuntimeException(e); }
        |    catch (Exception e) { throw new RuntimeException(e); }
        |  }
        |}
        |""".stripMargin,
    "lib/impl/Plugin.java" -> "package lib.impl; public class Plugin { public Plugin() { lib.Gone.x(); } public void other() { lib.Gone.x(); } }",
    // Ends with `impl/Plugin` but not with `/impl/Plugin`: the fragment must not name it.
    "lib/myimpl/Plugin.java" -> "package lib.myimpl; public class Plugin { public Plugin() { lib.Gone.x(); } }")

  private final class Scenario(
      val name: String,
      val lib: Map[String, String],
      val body: String,
      val keep: Seq[Keep],
      val expected: Seq[Row],
      val failsAtRuntime: Boolean,
      /** Does the analysis find the real failure (a Reflective row: only the rule reaches it)? False documents what is missed without the rule. */
      val findsFailure: Boolean,
      /** Files of the library (`reference.conf`), by path. */
      val resources: Map[String, String] = Map.empty
  )

  private val wiredSvc  = "lib/Svc.java" -> "package lib; public class Svc { public Svc() { } @Wired public void go() { Gone.x(); } public void other() { Gone.x(); } }"
  private val entity    = "lib/Item.java" -> "package lib; @Entity public class Item { public Item() { Gone.x(); } }"
  private val usedApi   = Map("lib/Api.java" -> "package lib; @Uses(Impl.class) public interface Api { }",
    "lib/Impl.java" -> "package lib; public class Impl { public Impl() { Gone.x(); } }")

  private val scenarios = Seq(
    Scenario("without a rule, a method only a framework calls is missed", annotations + container + wiredSvc,
      "lib.Container.start(lib.Svc.class);", Seq.empty,
      Seq(("lib.Svc", "go", Latent), ("lib.Svc", "other", Latent)), failsAtRuntime = true, findsFailure = false),

    Scenario("a rule on the annotation makes that method reachable, and only that one", annotations + container + wiredSvc,
      "lib.Container.start(lib.Svc.class);", Seq(Keep.Members(Keep.classNames("lib/Wired"))),
      Seq(("lib.Svc", "go", Label.Reflective), ("lib.Svc", "other", Latent)), failsAtRuntime = true, findsFailure = true),

    Scenario("a rule on another annotation changes nothing", annotations + container + wiredSvc,
      "lib.Container.start(lib.Svc.class);", Seq(Keep.Members(Keep.classNames("lib/Entity"))),
      Seq(("lib.Svc", "go", Latent), ("lib.Svc", "other", Latent)), failsAtRuntime = true, findsFailure = false),

    Scenario("without a rule, a constructor only a framework calls is missed", annotations + container + entity,
      "lib.Container.start(lib.Item.class);", Seq.empty,
      Seq(("lib.Item", "<init>", Latent)), failsAtRuntime = true, findsFailure = false),

    Scenario("a no-argument-constructor rule reaches the constructor of an annotated class", annotations + container + entity,
      "lib.Container.start(lib.Item.class);", Seq(Keep.NoArgConstructor(Keep.classNames("lib/Entity"))),
      Seq(("lib.Item", "<init>", Label.Reflective)), failsAtRuntime = true, findsFailure = true),

    Scenario("without a rule, a class only an annotation names is not examined", annotations ++ usedApi + container,
      "lib.Container.startUsed(lib.Api.class);", Seq.empty,
      Seq.empty, failsAtRuntime = true, findsFailure = false),

    Scenario("a class-value rule makes the named class and its constructor reachable", annotations ++ usedApi + container,
      "lib.Container.startUsed(lib.Api.class);", Seq(Keep.ClassValues(Keep.classNames("lib/Uses"))),
      Seq(("lib.Impl", "<init>", Label.Reflective)), failsAtRuntime = true, findsFailure = true),

    Scenario("without a rule, a method a framework calls on a subtype is missed", handlers,
      "lib.Server.start(lib.HImpl.class);", Seq.empty,
      Seq(("lib.HImpl", "<init>", Latent), ("lib.HImpl", "handle", Latent), ("lib.HImpl", "other", Latent), ("lib.HImpl", "secret", Latent), ("lib.Mid", "helper", Latent)), failsAtRuntime = true, findsFailure = false),

    Scenario("an overrides rule roots what overrides the type's methods, through an abstract class in between, and nothing else", handlers,
      "lib.Server.start(lib.HImpl.class);", Seq(Keep.Overrides(Keep.classNames("lib/Handler"))),
      Seq(("lib.HImpl", "<init>", Label.Reflective), ("lib.HImpl", "handle", Label.Reflective), ("lib.HImpl", "other", Latent), ("lib.HImpl", "secret", Latent), ("lib.Mid", "helper", Latent)),
      failsAtRuntime = true, findsFailure = true),

    // `handle` is declared by Handler, above the named type: it is still a method the named type's subtypes override.
    Scenario("naming an intermediate type keeps the methods it inherits from above, but not its own that nothing overrides", handlers,
      "lib.Server.start(lib.HImpl.class);", Seq(Keep.Overrides(Keep.classNames("lib/Mid"))),
      Seq(("lib.HImpl", "<init>", Label.Reflective), ("lib.HImpl", "handle", Label.Reflective), ("lib.HImpl", "other", Latent), ("lib.HImpl", "secret", Latent), ("lib.Mid", "helper", Latent)),
      failsAtRuntime = true, findsFailure = true),

    Scenario("an overrides rule on another type changes nothing", handlers,
      "lib.Server.start(lib.HImpl.class);", Seq(Keep.Overrides(Keep.classNames("lib/Unrelated"))),
      Seq(("lib.HImpl", "<init>", Latent), ("lib.HImpl", "handle", Latent), ("lib.HImpl", "other", Latent), ("lib.HImpl", "secret", Latent), ("lib.Mid", "helper", Latent)), failsAtRuntime = true, findsFailure = false),

    // ---- a class handed to a framework: the literal passed to a call a rule names ----
    Scenario("without a rule, the accessors of a class handed to a binder are missed", beans,
      "lib.Binder.bind(lib.Dto.class);", Seq.empty,
      Seq(("lib.Base", "setBase", Latent), ("lib.Dto", "<init>", Latent), ("lib.Dto", "setA", Latent), ("lib.Dto", "getA", Latent), ("lib.Dto", "isOn", Latent), ("lib.Dto", "other", Latent),
        ("lib.Dto", "setStatic", Latent), ("lib.Dto", "setHidden", Latent), ("lib.Dto", "get", Latent)),
      failsAtRuntime = true, findsFailure = false),

    Scenario("a bean-arguments rule roots the public accessors of the class and its ancestors, and nothing else", beans,
      "lib.Binder.bind(lib.Dto.class);", Seq(Keep.BeanArguments(Keep.callNames("lib/Binder.bind"))),
      Seq(("lib.Base", "setBase", Label.Reflective), ("lib.Dto", "<init>", Label.Reflective), ("lib.Dto", "setA", Label.Reflective), ("lib.Dto", "getA", Label.Reflective), ("lib.Dto", "isOn", Label.Reflective),
        ("lib.Dto", "other", Latent), ("lib.Dto", "setStatic", Latent), ("lib.Dto", "setHidden", Latent), ("lib.Dto", "get", Latent)),
      failsAtRuntime = true, findsFailure = true),

    Scenario("a rule on another call changes nothing", beans,
      "lib.Binder.bind(lib.Dto.class);", Seq(Keep.BeanArguments(Keep.callNames("lib/Binder.other"))),
      Seq(("lib.Base", "setBase", Latent), ("lib.Dto", "<init>", Latent), ("lib.Dto", "setA", Latent), ("lib.Dto", "getA", Latent), ("lib.Dto", "isOn", Latent), ("lib.Dto", "other", Latent),
        ("lib.Dto", "setStatic", Latent), ("lib.Dto", "setHidden", Latent), ("lib.Dto", "get", Latent)),
      failsAtRuntime = true, findsFailure = false),

    // ---- a class a configuration file names: the application reads the name from the file at run time ----
    Scenario("without a rule, a class only a configuration file names is not examined", configured,
      "lib.Loader.loadConfigured();", Seq.empty,
      Seq.empty, failsAtRuntime = true, findsFailure = false, resources = confFile),

    Scenario("a rule names the file: the class it names is built, and nothing else of it", configured,
      "lib.Loader.loadConfigured();", Seq(Keep.ClassesNamedIn(Keep.fileNames("reference.conf"))),
      Seq(("lib.Plugin", "<init>", Label.Reflective), ("lib.Plugin", "other", Latent)),
      failsAtRuntime = true, findsFailure = true, resources = confFile),

    Scenario("a rule for another file changes nothing", configured,
      "lib.Loader.loadConfigured();", Seq(Keep.ClassesNamedIn(Keep.fileNames("other.conf"))),
      Seq.empty, failsAtRuntime = true, findsFailure = false, resources = confFile),

    Scenario("the file may be anywhere in the path, but its name must match whole", configured,
      "lib.Loader.loadConfigured();", Seq(Keep.ClassesNamedIn(Keep.fileNames("reference.conf"))),
      Seq(("lib.Plugin", "<init>", Label.Reflective), ("lib.Plugin", "other", Latent)),
      failsAtRuntime = true, findsFailure = true, resources = Map("reference.conf" -> "plugin = \"lib.Plugin\"", "META-INF/old/xreference.conf" -> "x = \"lib.Decoy\"")),

    // ---- a class handed to a framework that picks the constructor by the arguments ----
    Scenario("without a rule, a constructor a framework picks by its arguments is missed", spawner,
      "lib.Spawner.spawn(lib.Worker.class, \"x\");", Seq.empty,
      Seq(("lib.Worker", "<init>", Latent), ("lib.Worker", "other", Latent)), failsAtRuntime = true, findsFailure = false),

    Scenario("a constructed-arguments rule roots every constructor, though the class is the first of two arguments", spawner,
      "lib.Spawner.spawn(lib.Worker.class, \"x\");", Seq(Keep.ConstructedArguments(Keep.callNames("lib/Spawner.spawn"))),
      Seq(("lib.Worker", "<init>", Label.Reflective), ("lib.Worker", "other", Latent)), failsAtRuntime = true, findsFailure = true),

    Scenario("a built-arguments rule roots only the no-argument constructor, which is not the one that is broken", spawner,
      "lib.Spawner.spawn(lib.Worker.class, \"x\");", Seq(Keep.BuiltArguments(Keep.callNames("lib/Spawner.spawn"))),
      Seq(("lib.Worker", "<init>", Latent), ("lib.Worker", "other", Latent)), failsAtRuntime = true, findsFailure = false),

    // ---- a class named by a constant tail and a prefix computed at run time ----
    Scenario("a class built from a computed prefix and a constant tail is reached, as a class a rule reaches", fragments,
      "lib.Loader.load();", Seq.empty,
      Seq(("lib.impl.Plugin", "<init>", Label.Reflective), ("lib.impl.Plugin", "other", Latent)), failsAtRuntime = true, findsFailure = true),

    Scenario("a tail only names a class that ends with all of it, from a separator", fragments,
      "lib.Loader.load();", Seq.empty,
      Seq(("lib.impl.Plugin", "<init>", Label.Reflective), ("lib.impl.Plugin", "other", Latent)), failsAtRuntime = true, findsFailure = true),

    Scenario("rules combine", annotations ++ usedApi + container + wiredSvc,
      "lib.Container.start(lib.Svc.class); lib.Container.startUsed(lib.Api.class);",
      Seq(Keep.Members(Keep.classNames("lib/Wired")), Keep.ClassValues(Keep.classNames("lib/Uses"))),
      Seq(("lib.Svc", "go", Label.Reflective), ("lib.Svc", "other", Latent), ("lib.Impl", "<init>", Label.Reflective)), failsAtRuntime = true, findsFailure = true)
  )

  private def analyse(app: Path, lib: Path, keep: Seq[Keep], precision: Precision = Precision.Method): Vector[Row] =
    val syms    = new Symbols
    val index   = ClassIndex(Vector(source(app), source(lib), ClassSource.jdk()), syms)
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), Vector(0, 1))
    val locator = Locator(index)
    Checker.findings(index, scope, CheckOptions(precision, keep)).map: f =>
      (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label)
    .distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val (v1, v2) = GoneLibrary.build(s.lib, s.resources)
      val app = compile(Map("app/Main.java" ->
        s"package app; public class Main { public static void run() { ${s.body} } }"), Seq(v1))
      val rows = analyse(app, v2, s.keep)
      assertEquals(rows, s.expected.sortBy(_.toString).toVector)
      val error = RuntimeOracle.run(Seq(app, v2))
      assertEquals(error.isDefined, s.failsAtRuntime, s"runtime outcome: $error")
      // With a rule the failure is found; a scenario without one documents what is missed.
      assertEquals(rows.exists(_._3 == Label.Reflective), s.findsFailure, s"rows: $rows")
      assertEquals(analyse(app, v2, s.keep, Precision.MethodRta).exists(_._3 == Label.Reflective), s.findsFailure, "under RTA")
    }

  test("the Servlet pack names only types the real Servlet API has") {
    import java.util.zip.ZipFile
    val props = new java.util.Properties()
    val in    = getClass.getResourceAsStream("/scala-compilers.properties")
    try props.load(in) finally in.close()
    val api = props.getProperty("jetty").split(java.io.File.pathSeparator).find(_.contains("javax.servlet-api")).getOrElse(fail("no servlet api jar"))
    val zip = new ZipFile(api)
    try
      val javax = Keep.Servlet.collect { case Keep.Overrides(types) => types.map(_.value) }.flatten.filter(_.startsWith("javax/"))
      assert(javax.nonEmpty)
      val missing = javax.filterNot(t => zip.getEntry(t + ".class") != null).toSet
      assertEquals(missing, Set.empty[String], "types the Servlet pack names that javax.servlet-api 3.1 does not have")
    finally zip.close()
  }

