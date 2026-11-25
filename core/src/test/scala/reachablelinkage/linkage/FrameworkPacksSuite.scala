package reachablelinkage.linkage

import java.nio.file.{Path, Paths}
import java.util.Properties

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.fixture.{ChildRun, GoneLibrary, Scopes}
import reachablelinkage.symbols.Symbols

/**
 * The Guice and Jackson packs: code a framework calls by reflection (an `@Inject` constructor, a
 * `@Provides` method, a `@JsonCreator`, a `@JsonDeserialize(using = ...)` class) is reachable though
 * nothing in the application names it. Every scenario builds an app against the real framework jars,
 * runs it on the JVM with a library whose `lib.Gone.x()` is gone, and requires that a real linkage
 * failure was labelled Reachable.
 */
class FrameworkPacksSuite extends munit.FunSuite:

  override def munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private type Row = (String, String, Label)
  private val Reachable = Label.Reachable
  private val Reflective = Label.Reflective
  /** What fails for real is reached by a call or, for a framework, by its rule: either way it must be labelled so. */
  private def counts(l: Label): Boolean = l == Reachable || l == Reflective
  private val Latent    = Label.Latent

  private def jars(key: String): Seq[Path] =
    val props = new Properties()
    val in    = getClass.getResourceAsStream("/scala-compilers.properties")
    try props.load(in) finally in.close()
    props.getProperty(key).split(java.io.File.pathSeparator).toSeq.map(Paths.get(_))

  private final class Scenario(
      val name: String,
      val framework: String,
      val lib: Map[String, String],
      val body: String,
      val expected: Seq[Row],
      val failsAtRuntime: Boolean,
      /** Classes (internal names) the library no longer has. */
      val removed: Set[String] = Set.empty
  )

  private val service = "lib/Service.java" -> "package lib; public interface Service { void run(); }"
  private def guiceMain(module: String, get: String) =
    s"com.google.inject.Guice.createInjector($module).getInstance($get)"

  /** Starts a Jetty on a free port with the servlet class at /x, requests it, and rethrows what the servlet saw (the container swallows it into a 500). */
  private def jettyMain(servlet: String): String =
    "try { org.eclipse.jetty.server.Server server = new org.eclipse.jetty.server.Server(0); " +
      "org.eclipse.jetty.servlet.ServletContextHandler h = new org.eclipse.jetty.servlet.ServletContextHandler(org.eclipse.jetty.servlet.ServletContextHandler.NO_SESSIONS); " +
      s"h.setContextPath(\"/\"); h.addServlet($servlet, \"/x\"); server.setHandler(h); server.start(); " +
      "try { int port = ((org.eclipse.jetty.server.ServerConnector) server.getConnectors()[0]).getLocalPort(); " +
      "java.net.HttpURLConnection c = (java.net.HttpURLConnection) new java.net.URL(\"http://127.0.0.1:\" + port + \"/x\").openConnection(); c.getResponseCode(); " +
      "Throwable seen = lib.Hello.seen; if (seen instanceof Error) throw (Error) seen; if (seen != null) throw new RuntimeException(seen); } finally { server.stop(); } } " +
      "catch (Error e) { throw e; } catch (RuntimeException e) { throw e; } catch (Exception e) { throw new RuntimeException(e); }"

  private val bindImpl =
    "new com.google.inject.AbstractModule() { protected void configure() { bind(lib.Service.class).to(lib.Impl.class); } }"

  private val scenarios = Seq(
    Scenario("Guice: an @Inject constructor of a bound class", "guice",
      Map(service, "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { Gone.x(); } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "<init>", Reflective)), failsAtRuntime = true),

    Scenario("Guice: a @Provides method of an installed module", "guice",
      Map("lib/Mod.java" -> "package lib; public class Mod extends com.google.inject.AbstractModule { @com.google.inject.Provides String provide() { Gone.x(); return \"\"; } String unused() { Gone.x(); return \"\"; } }"),
      guiceMain("new lib.Mod()", "String.class") + ";",
      Seq(("lib.Mod", "provide", Reflective), ("lib.Mod", "unused", Latent)), failsAtRuntime = true),

    // The details of Guice's extensions, checked against the real library: nothing more than `@Inject` is needed.
    Scenario("Guice: a Multibinder set member is built through its @Inject constructor", "guice",
      Map(service, "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { Gone.x(); } public void run() { } }",
        "lib/Mod.java" -> "package lib; public class Mod extends com.google.inject.AbstractModule { protected void configure() { com.google.inject.multibindings.Multibinder.newSetBinder(binder(), Service.class).addBinding().to(Impl.class); } }"),
      "com.google.inject.Guice.createInjector(new lib.Mod()).getInstance(com.google.inject.Key.get(new com.google.inject.TypeLiteral<java.util.Set<lib.Service>>() { }));",
      Seq(("lib.Impl", "<init>", Reflective)), failsAtRuntime = true),

    Scenario("Guice: a @Named parameter changes nothing about who is built", "guice",
      Map(service, "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl(@com.google.inject.name.Named(\"x\") String s) { Gone.x(); } public void run() { } }",
        "lib/Mod.java" -> "package lib; public class Mod extends com.google.inject.AbstractModule { protected void configure() { bindConstant().annotatedWith(com.google.inject.name.Names.named(\"x\")).to(\"v\"); bind(Service.class).to(Impl.class); } }"),
      guiceMain("new lib.Mod()", "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "<init>", Reflective)), failsAtRuntime = true),

    Scenario("Guice: @ImplementedBy names the implementation", "guice",
      Map("lib/Service.java" -> "package lib; @com.google.inject.ImplementedBy(Impl.class) public interface Service { void run(); }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { Gone.x(); } public void run() { } }"),
      guiceMain("new com.google.inject.AbstractModule() { protected void configure() { } }", "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "<init>", Reflective)), failsAtRuntime = true),

    Scenario("Guice: a module and an injectable class the app never mentions stay unexamined", "guice",
      Map(service, "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { Gone.x(); } public void run() { } }"),
      guiceMain("new com.google.inject.AbstractModule() { protected void configure() { } }", "Object.class") + ";",
      Seq.empty, failsAtRuntime = false),

    // "Instantiated" is "reached" until RTA: a class literal alone keeps its @Inject constructor, though
    // nothing here binds it. A known over-approximation, pinned so a change to it is deliberate.
    Scenario("Guice: a class named only by a literal keeps its @Inject constructor (no RTA yet)", "guice",
      Map(service, "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { Gone.x(); } public void run() { } }"),
      "Object o = lib.Impl.class;",
      Seq(("lib.Impl", "<init>", Reflective)), failsAtRuntime = false),

    // ---- a framework reflecting over a class loads the types in all its members' signatures ----
    Scenario("Guice: an @Inject constructor whose parameter type is gone", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl(Dep d) { } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    // The type argument is only in the generic signature: the erased parameter type (`Provider`) exists. Guice asks for the generic types
    // of the members it injects, so a type argument that is gone fails there with a TypeNotPresentException.
    Scenario("Guice: an @Inject constructor whose type argument is gone", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl(com.google.inject.Provider<Dep> d) { } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Guice: an @Inject field whose type argument is gone", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject com.google.inject.Provider<Dep> d; public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    // Not injected, so its generic types are never asked for: nothing fails, and nothing is reported.
    Scenario("Guice: a type argument that is gone in a method nobody injects is harmless", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { } public void other(com.google.inject.Provider<Dep> d) { } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq.empty, failsAtRuntime = false, removed = Set("lib/Dep")),

    Scenario("Guice: an unannotated method of an injected class has a parameter type that is gone", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { } public void other(Dep d) { } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Guice: a member whose parameter is an array of a type that is gone", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject public Impl() { } public void other(Dep[][] d) { } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Guice: an @Inject field whose type is gone", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { @javax.inject.Inject Dep dep; public Impl() { } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Guice: a @Provides method whose return type is gone", "guice",
      Map("lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Mod.java" -> "package lib; public class Mod extends com.google.inject.AbstractModule { @com.google.inject.Provides Dep provide() { return null; } }"),
      guiceMain("new lib.Mod()", "String.class") + ";",
      Seq(("lib.Mod", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Guice: a class that @ImplementedBy names has a member type that is gone", "guice",
      Map("lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Service.java" -> "package lib; @com.google.inject.ImplementedBy(Impl.class) public interface Service { void run(); }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { public Impl() { } public void other(Dep d) { } public void run() { } }"),
      guiceMain("new com.google.inject.AbstractModule() { protected void configure() { } }", "lib.Service.class") + ".run();",
      Seq(("lib.Impl", "", Reflective)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Jackson: a class marked by a class-level annotation has a member type that is gone", "jackson",
      Map("lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Bean.java" -> "package lib; @com.fasterxml.jackson.annotation.JsonInclude public class Bean { public Bean() { } public void setX(Dep d) { } }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{}\", lib.Bean.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Bean", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Guice: a class no framework reflects on may have a signature type that is gone", "guice",
      Map("lib/Dep.java" -> "package lib; public class Dep { }", "lib/Plain.java" -> "package lib; public class Plain { public void other(Dep d) { } }"),
      "new lib.Plain();",
      Seq.empty, failsAtRuntime = false, removed = Set("lib/Dep")),

    Scenario("Guice: a reflected class whose signature types all exist is fine", "guice",
      Map(service, "lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Impl.java" -> "package lib; public class Impl implements Service { Dep unused; @javax.inject.Inject public Impl() { } public void run() { } }"),
      guiceMain(bindImpl, "lib.Service.class") + ".run();",
      Seq.empty, failsAtRuntime = false),

    Scenario("Jackson: a @JsonCreator constructor whose parameter type is gone", "jackson",
      Map("lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Dto.java" -> "package lib; import com.fasterxml.jackson.annotation.*; public class Dto { @JsonCreator public Dto(@JsonProperty(\"a\") Dep a) { } }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{\\\"a\\\":null}\", lib.Dto.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Dto", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    // Jackson reads the generic types of every property of an annotated class, to build its deserializer.
    Scenario("Jackson: a property whose type argument is gone", "jackson",
      Map("lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Dto.java" -> "package lib; import com.fasterxml.jackson.annotation.*; public class Dto { @JsonProperty(\"a\") public java.util.List<Dep> a; }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{\\\"a\\\":null}\", lib.Dto.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Dto", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    // No annotation anywhere: the class is only handed to the mapper as a literal, and Jackson calls its setter by the bean naming convention.
    Scenario("Jackson: an unannotated bean handed to readValue has its setter called", "jackson",
      Map("lib/Dto.java" -> "package lib; public class Dto { public Dto() { } public void setA(String a) { Gone.x(); } public void other() { Gone.x(); } }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{\\\"a\\\":\\\"x\\\"}\", lib.Dto.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Dto", "setA", Reflective), ("lib.Dto", "other", Latent)), failsAtRuntime = true),

    Scenario("Jackson: a bean whose property type argument is gone, with no annotation, is still read whole", "jackson",
      Map("lib/Dep.java" -> "package lib; public class Dep { }",
        "lib/Dto.java" -> "package lib; public class Dto { public java.util.List<Dep> items; }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{\\\"items\\\":null}\", lib.Dto.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Dto", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    // ---- Gson: builds the class it is handed through its default constructor and reads its fields; no accessor is called ----
    Scenario("Gson: a class handed to fromJson is built by its default constructor", "gson",
      Map("lib/Dto.java" -> "package lib; public class Dto { public Dto() { Gone.x(); } public void setA(String a) { Gone.x(); } }"),
      "new com.google.gson.Gson().fromJson(\"{}\", lib.Dto.class);",
      Seq(("lib.Dto", "<init>", Reflective), ("lib.Dto", "setA", Latent)), failsAtRuntime = true),

    Scenario("Gson: the field types of a class handed to fromJson are loaded", "gson",
      Map("lib/Dep.java" -> "package lib; public class Dep { }", "lib/Dto.java" -> "package lib; public class Dto { public Dep dep; }"),
      "new com.google.gson.Gson().fromJson(\"{}\", lib.Dto.class);",
      Seq(("lib.Dto", "", Reachable)), failsAtRuntime = true, removed = Set("lib/Dep")),

    Scenario("Gson: an adapter class named by @JsonAdapter is built", "gson",
      Map("lib/Adapter.java" ->
          "package lib; import com.google.gson.*; import com.google.gson.stream.*; public class Adapter extends TypeAdapter<Dto> { public Adapter() { Gone.x(); } public void write(JsonWriter w, Dto d) { } public Dto read(JsonReader r) { return null; } }",
        "lib/Dto.java" -> "package lib; @com.google.gson.annotations.JsonAdapter(Adapter.class) public class Dto { }"),
      "new com.google.gson.Gson().fromJson(\"{}\", lib.Dto.class);",
      Seq(("lib.Adapter", "<init>", Reflective)), failsAtRuntime = true),

    // ---- a real servlet container (Jetty) running a servlet the library ships ----
    // With the container on the analysed classpath the graph already reaches doGet on its own: Jetty's threads (`Runnable.run`, a method the JDK
    // calls) lead to `Servlet.service` and, by dispatch, to the servlet. The `Servlet` pack is for a container that is not on the classpath.
    Scenario("Servlet: doGet of a servlet a real container runs is reached with no rule when the container is on the classpath", "jetty",
      Map("lib/Hello.java" ->
        """package lib;
          |import javax.servlet.http.*;
          |public class Hello extends HttpServlet {
          |  public static volatile Throwable seen;
          |  @Override protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws java.io.IOException {
          |    try { Gone.x(); } catch (Throwable t) { seen = t; throw t; }
          |    resp.getWriter().print("hello");
          |  }
          |}
          |""".stripMargin),
      jettyMain("lib.Hello.class"),
      Seq(("lib.Hello", "doGet", Reachable)), failsAtRuntime = true),

    // ---- pekko: the actor system builds the class it is given through the constructor that fits the arguments ----
    Scenario("Pekko: an actor class given to Props.create is built through its constructor", "pekko",
      Map("lib/Worker.java" ->
        """package lib;
          |public class Worker extends org.apache.pekko.actor.AbstractActor {
          |  public static volatile Throwable seen;
          |  public static volatile boolean ready;
          |  public Worker(String s) { try { Gone.x(); ready = true; } catch (Throwable t) { seen = t; throw t; } }
          |  public void other() { Gone.x(); }
          |  @Override public Receive createReceive() { return receiveBuilder().build(); }
          |}
          |""".stripMargin),
      "try { org.apache.pekko.actor.ActorSystem system = org.apache.pekko.actor.ActorSystem.create(\"t\"); " +
        "try { system.actorOf(org.apache.pekko.actor.Props.create(lib.Worker.class, \"x\")); " +
        "for (int i = 0; i < 100 && lib.Worker.seen == null && !lib.Worker.ready; i++) Thread.sleep(100); " +
        "Throwable seen = lib.Worker.seen; if (seen instanceof Error) throw (Error) seen; if (seen != null) throw new RuntimeException(seen); } " +
        "finally { system.terminate(); system.getWhenTerminated().toCompletableFuture().get(10, java.util.concurrent.TimeUnit.SECONDS); } } " +
        "catch (Error e) { throw e; } catch (RuntimeException e) { throw e; } catch (Exception e) { throw new RuntimeException(e); }",
      Seq(("lib.Worker", "<init>", Reflective), ("lib.Worker", "other", Latent)), failsAtRuntime = true),

    // The literal goes through a call that returns the type (`constructType`) before it reaches `readValue`.
    Scenario("Jackson: a class turned into a JavaType first is still the class readValue is given", "jackson",
      Map("lib/Dto.java" -> "package lib; public class Dto { public Dto() { } public void setA(String a) { Gone.x(); } }"),
      "try { com.fasterxml.jackson.databind.ObjectMapper m = new com.fasterxml.jackson.databind.ObjectMapper(); " +
        "m.readValue(\"{\\\"a\\\":\\\"x\\\"}\", m.getTypeFactory().constructType(lib.Dto.class)); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Dto", "setA", Reflective)), failsAtRuntime = true),

    // What `Props[Worker]` is in Scala: the class goes through `ClassTag.apply` and the ClassTag is what `Props.apply` is given.
    Scenario("Pekko: Props made from a ClassTag builds the actor class the tag is of", "pekko",
      Map("lib/Worker2.java" ->
        """package lib;
          |public class Worker2 extends org.apache.pekko.actor.AbstractActor {
          |  public static volatile Throwable seen;
          |  public static volatile boolean ready;
          |  public Worker2() { try { Gone.x(); ready = true; } catch (Throwable t) { seen = t; throw t; } }
          |  @Override public Receive createReceive() { return receiveBuilder().build(); }
          |}
          |""".stripMargin),
      "try { org.apache.pekko.actor.ActorSystem system = org.apache.pekko.actor.ActorSystem.create(\"t\"); " +
        "try { system.actorOf(org.apache.pekko.actor.Props$.MODULE$.apply(scala.reflect.ClassTag$.MODULE$.apply(lib.Worker2.class))); " +
        "for (int i = 0; i < 100 && lib.Worker2.seen == null && !lib.Worker2.ready; i++) Thread.sleep(100); " +
        "Throwable seen = lib.Worker2.seen; if (seen instanceof Error) throw (Error) seen; if (seen != null) throw new RuntimeException(seen); } " +
        "finally { system.terminate(); system.getWhenTerminated().toCompletableFuture().get(10, java.util.concurrent.TimeUnit.SECONDS); } } " +
        "catch (Error e) { throw e; } catch (RuntimeException e) { throw e; } catch (Exception e) { throw new RuntimeException(e); }",
      Seq(("lib.Worker2", "<init>", Reflective)), failsAtRuntime = true),

    Scenario("Jackson: a @JsonCreator constructor", "jackson",
      Map("lib/Dto.java" -> "package lib; import com.fasterxml.jackson.annotation.*; public class Dto { @JsonCreator public Dto(@JsonProperty(\"a\") int a) { Gone.x(); } }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{\\\"a\\\":1}\", lib.Dto.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Dto", "<init>", Reflective)), failsAtRuntime = true),

    Scenario("Jackson: the default constructor of a class with annotated members", "jackson",
      Map("lib/Bean.java" -> "package lib; import com.fasterxml.jackson.annotation.*; public class Bean { public Bean() { Gone.x(); } @JsonProperty(\"n\") public void setN(int n) { } }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{\\\"n\\\":1}\", lib.Bean.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Bean", "<init>", Reflective)), failsAtRuntime = true),

    Scenario("Jackson: a deserializer named by @JsonDeserialize(using = ...)", "jackson",
      Map(
        "lib/Dto.java" -> "package lib; @com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = Deser.class) public class Dto { }",
        "lib/Deser.java" -> "package lib; public class Deser extends com.fasterxml.jackson.databind.JsonDeserializer<Dto> { public Dto deserialize(com.fasterxml.jackson.core.JsonParser p, com.fasterxml.jackson.databind.DeserializationContext c) { Gone.x(); return new Dto(); } }"),
      "try { new com.fasterxml.jackson.databind.ObjectMapper().readValue(\"{}\", lib.Dto.class); } catch (java.io.IOException e) { throw new RuntimeException(e); }",
      Seq(("lib.Deser", "deserialize", Reflective)), failsAtRuntime = true),

    Scenario("Jackson: a class the app never mentions stays unexamined", "jackson",
      Map("lib/Dto.java" -> "package lib; import com.fasterxml.jackson.annotation.*; public class Dto { @JsonCreator public Dto(@JsonProperty(\"a\") int a) { Gone.x(); } }"),
      "new com.fasterxml.jackson.databind.ObjectMapper();",
      Seq.empty, failsAtRuntime = false)
  )

  private def analyse(app: Path, lib: Path, framework: Seq[Path], precision: Precision): Vector[Row] =
    val syms    = new Symbols
    val sources = Vector(source(app), source(lib)) ++ framework.map(j => ClassSource.open(j).toOption.get) :+ ClassSource.jdk()
    val index   = ClassIndex(sources, syms)
    val all     = (0 until sources.length - 1).toVector
    val scope   = Scopes.of(index, Vector(0), Vector(0, 1), all)
    val locator = Locator(index)
    Checker.findings(index, scope, precision).map: f =>
      (syms.className(f.conflict.inClass).replace('/', '.'), locator.locate(f).methodName, f.label)
    .distinct.sortBy(_.toString)

  for s <- scenarios do
    test(s.name) {
      val fw       = jars(s.framework)
      val (v1, v2) = GoneLibrary.build(s.lib, classpath = fw, removed = s.removed)
      val app = compile(Map("app/Main.java" ->
        s"package app; public class Main { public static void run() { ${s.body} } }"), Seq(v1) ++ fw)
      val rows = analyse(app, v2, fw, Precision.Method)
      assertEquals(rows, s.expected.sortBy(_.toString).toVector)

      // The control: the app runs against the library it was built against, in a child JVM that lets the frameworks
      // work on this JDK. If it failed, a "real failure" below could be the framework not working, and the scenario
      // would prove nothing about our check.
      assertEquals(ChildRun.run(Seq(app, v1) ++ fw), None, "the control run (against v1) must succeed")
      val failure = ChildRun.run(Seq(app, v2) ++ fw)
      assertEquals(failure.isDefined, s.failsAtRuntime, s"runtime outcome: $failure")
      failure.foreach: chain =>
        // The error these scenarios are about, not just any LinkageError (a framework failing to start throws one too).
        assert(chain.exists((c, _) => c == "java.lang.NoSuchMethodError" || c == "java.lang.NoClassDefFoundError" || c == "java.lang.TypeNotPresentException"),
          s"expected the missing method or class (or a type argument that is gone) in the causes, got $chain")
        assert(rows.exists(r => counts(r._3)), s"a real failure had no Reachable or Reflective conflict: $rows")
        assert(analyse(app, v2, fw, Precision.MethodRta).exists(r => counts(r._3)), "a real failure had no Reachable or Reflective conflict under RTA")
    }
