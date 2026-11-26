package reachablelinkage.api

import java.nio.file.Path
import java.util.{Arrays, Collections, HashMap, List as JList, Map as JMap}

import scala.jdk.CollectionConverters.*

import reachablelinkage.fixture.GoneLibrary
import reachablelinkage.fixture.JavaFixture.compile

/** The bridge says what `Linkage.check` says, in nothing but `java.util` maps, lists, strings, integers and booleans. */
class LinkageBridgeSuite extends munit.FunSuite:

  // `Used.run` calls the broken method; `Util.unused` does too, but nothing calls it.
  private val lib = Map(
    "lib/Used.java" -> "package lib; public class Used { public static void run() { Gone.x(); } }",
    "lib/Util.java" -> "package lib; public class Util { public static void unused() { Gone.x(); } public static void used() { } }"
  )

  private def scenario(body: String): (Path, Path) =
    val (v1, v2) = GoneLibrary.build(lib)
    (compile(Map("app/Main.java" -> s"package app; public class Main { public static void run() { $body } }"), Seq(v1)), v2)

  private def request(app: Path, library: Path, extra: (String, AnyRef)*): JMap[String, AnyRef] =
    val map = new HashMap[String, AnyRef]()
    map.put("projectClasses", Collections.singletonList(app.toString))
    map.put("classpath", Collections.singletonList(Arrays.asList("lib", library.toString)))
    extra.foreach((k, v) => map.put(k, v))
    map

  private def conflicts(response: JMap[String, AnyRef]): List[JMap[String, AnyRef]] =
    response.get("conflicts").asInstanceOf[JList[JMap[String, AnyRef]]].asScala.toList

  test("the bridge reports the same conflicts as the facade, with the same labels") {
    val (app, v2) = scenario("lib.Used.run(); lib.Util.used();")
    val direct    = Linkage.check(
      LinkageRequest
        .builder()
        .withProjectClasses(Collections.singletonList(app))
        .withClasspath(Collections.singletonList(ClasspathEntry("lib", v2)))
        .build()
    )
    val bridged = conflicts(LinkageBridge.check(request(app, v2)))

    assertEquals(bridged.size, direct.conflicts.size)
    assertEquals(
      bridged.map(c => (c.get("category"), c.get("label"), c.get("fromClass"), c.get("fromMethodName"), c.get("targetClass"), c.get("targetMemberName"))).toSet,
      direct.conflicts.asScala
        .map(c => (c.category.name, c.label.name, c.dependency.fromClass, c.dependency.fromMethodName, c.dependency.targetClass, c.dependency.targetMemberName))
        .toSet
    )
  }

  test("a conflict says whether it fails the default fail level and whether missinglink-core would report it") {
    val (app, v2) = scenario("lib.Used.run(); lib.Util.used();")
    val byMethod  = conflicts(LinkageBridge.check(request(app, v2))).map(c => c.get("fromMethodName") -> c).toMap
    val reached   = byMethod("run")
    assertEquals(reached.get("label"), "REACHABLE")
    assertEquals(reached.get("failing"), java.lang.Boolean.TRUE)
    assertEquals(reached.get("reportedByMissinglink"), java.lang.Boolean.TRUE)
    val unreached = byMethod("unused")
    assertEquals(unreached.get("label"), "LATENT")
    assertEquals(unreached.get("failing"), java.lang.Boolean.FALSE)
    assertEquals(unreached.get("reportedByMissinglink"), java.lang.Boolean.TRUE, "latent, but the category is one missinglink-core reports")
  }

  test("enum values and class precision are read whatever their case, and class precision reaches every conflict") {
    val (app, v2) = scenario("lib.Used.run();")
    val response  = LinkageBridge.check(request(app, v2, "precision" -> "class", "scope" -> "APP"))
    assert(conflicts(response).forall(_.get("label") == "REACHABLE"), "at class precision every conflict is reachable")
  }

  test("a package filter removes conflicts and is reported with its kind and count") {
    val (app, v2) = scenario("lib.Used.run(); lib.Util.used();")
    val response  = LinkageBridge.check(request(app, v2, "ignoreDestinationPackages" -> Collections.singletonList(Arrays.asList("lib", "true"))))
    assertEquals(conflicts(response), Nil)
    val filtered = response.get("filtered").asInstanceOf[JList[JList[String]]].asScala.map(_.asScala.toList).toList
    assertEquals(filtered, List(List("IGNORE_DESTINATION_PACKAGES", "2")))
  }

  test("a request that cannot be used comes back as an error value, with nothing else") {
    val (app, v2)                                 = scenario("lib.Used.run();")
    def errorOf(extra: (String, AnyRef)*): String = LinkageBridge.check(request(app, v2, extra*)).get("error").asInstanceOf[String]

    assert(errorOf("scope" -> "Everywhere").contains("'scope' must be one of App, Full"))
    assert(errorOf("javaRelease" -> "eight").contains("'javaRelease' must be an integer"))
    assert(errorOf("excludedEntries" -> "lib").contains("'excludedEntries' must be a list of strings"))
    assert(
      errorOf(
        "ignoreSourcePackages" -> Collections.singletonList(Arrays.asList("a", "true")),
        "targetSourcePackages" -> Collections.singletonList(Arrays.asList("b", "true"))
      ).contains("cannot be defined together")
    )
    assert(errorOf("targetSourcePackages" -> Collections.singletonList(Arrays.asList("not a package", "true"))).contains("not a package"))
    // A value that is not "true" or "false" is an error value, not an exception out of the bridge.
    assert(errorOf("ignoreSourcePackages" -> Collections.singletonList(Arrays.asList("com.acme", "maybe"))).contains("\"true\" or \"false\""))
    assert(errorOf("classpath" -> Collections.singletonList(Arrays.asList("lib"))).contains("'classpath' must be a list of 2-string lists"))

    val empty = LinkageBridge.check(new HashMap[String, AnyRef]())
    assertEquals(empty.keySet.asScala.toSet, Set("error"))
    assert(empty.get("error").asInstanceOf[String].contains("projectClasses"))
  }

  // A framework in miniature: it calls the `@Wired` methods of a class by reflection, so only a rule says the method is called.
  private val wiredLib = Map(
    "lib/Wired.java"     -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Wired { }",
    "lib/Svc.java"       -> "package lib; public class Svc { public Svc() { } @Wired public void go() { Gone.x(); } }",
    "lib/Container.java" -> "package lib; public class Container { public static Object start(Class<?> type) { return null; } }",
    // Nothing in the project names it: only a class-load log of a real run does.
    "lib/Hidden.java" -> "package lib; public class Hidden { public Hidden() { Gone.x(); } }"
  )

  private def wiredScenario(): (Path, Path) =
    val (v1, v2) = GoneLibrary.build(wiredLib)
    (compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Container.start(lib.Svc.class); } }"), Seq(v1)), v2)

  private def writeFile(content: String): String =
    val file = java.nio.file.Files.createTempFile("bridge", ".txt")
    java.nio.file.Files.write(file, content.getBytes("UTF-8"))
    file.toString

  private def labelOf(response: JMap[String, AnyRef], fromClass: String, method: String): Option[AnyRef] =
    conflicts(response).find(c => c.get("fromClass") == fromClass && c.get("fromMethodName") == method).map(_.get("label"))

  test("a rules file adds to the built-in packs: the method it keeps is reached by a rule, not a call") {
    val (app, v2) = wiredScenario()
    assertEquals(labelOf(LinkageBridge.check(request(app, v2)), "lib.Svc", "go"), Some("LATENT"))
    val withRule = LinkageBridge.check(request(app, v2, "keepRulesFile" -> writeFile("keep-members lib.Wired\n")))
    assertEquals(labelOf(withRule, "lib.Svc", "go"), Some("REFLECTIVE"))
  }

  test("a rules file that cannot be read is an error value that says where") {
    val (app, v2) = wiredScenario()
    val error     = LinkageBridge.check(request(app, v2, "keepRulesFile" -> writeFile("keep-nothing lib.Wired\n"))).get("error").asInstanceOf[String]
    assert(error.contains("keep-nothing"), error)
  }

  test("a class-load log makes the classes of a real run used, so a break in one nothing names is found") {
    val (app, v2) = wiredScenario()
    assertEquals(labelOf(LinkageBridge.check(request(app, v2)), "lib.Hidden", "<init>"), None)
    val withLog = LinkageBridge.check(request(app, v2, "classLoadLog" -> writeFile("[0.05s][info][class,load] lib.Hidden source: file:/lib\n")))
    assertEquals(labelOf(withLog, "lib.Hidden", "<init>"), Some("REFLECTIVE"))
    assert(LinkageBridge.check(request(app, v2, "classLoadLog" -> writeFile("not a log\n"))).containsKey("error"))
  }

  test("nothing but java.util and java.lang types crosses the bridge, so no class of this library is shared with the caller") {
    val (app, v2)                             = scenario("lib.Used.run();")
    def classesIn(value: AnyRef): Set[String] = value match
      case null          => Set.empty
      case m: JMap[?, ?] =>
        m.asScala.toList.flatMap((k, v) => classesIn(k.asInstanceOf[AnyRef]) ++ classesIn(v.asInstanceOf[AnyRef])).toSet + m.getClass.getName
      case l: JList[?] => l.asScala.toList.flatMap(e => classesIn(e.asInstanceOf[AnyRef])).toSet + l.getClass.getName
      case other       => Set(other.getClass.getName)
    val seen = classesIn(LinkageBridge.check(request(app, v2))) ++ classesIn(LinkageBridge.check(new HashMap[String, AnyRef]()))
    assert(seen.forall(_.startsWith("java.")), s"types from outside the JDK: ${seen.filterNot(_.startsWith("java."))}")
  }
