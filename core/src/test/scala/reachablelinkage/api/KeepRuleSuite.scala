package reachablelinkage.api

import java.nio.file.{Files, Path}
import scala.compiletime.constValueTuple
import scala.deriving.Mirror
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.KeepRuleError.Problem
import reachablelinkage.fixture.JavaFixture.compile
import reachablelinkage.fixture.{GoneLibrary, Jars}
import reachablelinkage.linkage.Keep

class KeepRuleSuite extends munit.FunSuite:

  private def names(r: KeepRule): List[String] = r.names.asScala.toList

  private def file(text: String): Path =
    val f = Files.createTempFile("rules", ".txt"); Files.writeString(f, text); f

  test("a rule over well-formed class names is built, with duplicates dropped") {
    val rule = KeepRule.of(KeepRuleKind.KEEP_MEMBERS, List("javax.inject.Inject", "a.b.Outer$Inner", "javax.inject.Inject"))
    assertEquals(rule.map(r => (r.kind, names(r))), Right((KeepRuleKind.KEEP_MEMBERS, List("javax.inject.Inject", "a.b.Outer$Inner"))))
  }

  test("an empty rule, or a name that is not a class name, is rejected with the reason") {
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_MEMBERS, Nil), Left(Problem.NoNames))
    for bad <- List("not a name", "a..B", ".A", "a.", "1abc.X", "a-b.C", "") do
      assertEquals(KeepRule.of(KeepRuleKind.FOLLOW_CLASS_VALUES, List("ok.Name", bad)), Left(Problem.InvalidName(bad)), s"'$bad'")
  }

  test("each problem says what is wrong") {
    assertEquals(Problem.NoNames.message, "no names")
    assert(Problem.InvalidName("a..B").message.contains("'a..B'"))
    assert(Problem.UnknownDirective("keep-all").message.contains("keep-all"))
  }

  test("a rules file is read: directives, several names, comments, blank lines, CRLF") {
    val rules = KeepRule.parse(
      file(
        "# the framework's entry points\r\n\r\nkeep-members   lib.Wired  lib.Other   # two annotations\r\nkeep-no-arg-constructor lib.Entity\nfollow-class-values lib.Uses\n"
      )
    )
    assertEquals(
      rules.map(_.map(r => (r.kind, names(r)))),
      Right(
        List(
          (KeepRuleKind.KEEP_MEMBERS, List("lib.Wired", "lib.Other")),
          (KeepRuleKind.KEEP_NO_ARG_CONSTRUCTOR, List("lib.Entity")),
          (KeepRuleKind.FOLLOW_CLASS_VALUES, List("lib.Uses"))
        )
      )
    )
  }

  test("an empty file or one with only comments is no rules") {
    assertEquals(KeepRule.parse(file("")), Right(Nil))
    assertEquals(KeepRule.parse(file("# nothing here\n\n")), Right(Nil))
  }

  test("a bad line is reported with its number and the problem; the first one wins") {
    def problem(text: String): Option[(Int, Problem)] =
      KeepRule.parse(file(text)).left.toOption.collect { case KeepRuleError.BadLine(_, n, p) => (n, p) }
    assertEquals(problem("# c\nkeep-everything lib.A\n"), Some((2, Problem.UnknownDirective("keep-everything"))))
    assertEquals(problem("keep-members\n"), Some((1, Problem.NoNames)))
    assertEquals(problem("ok.X\n"), Some((1, Problem.UnknownDirective("ok.X"))))
    assertEquals(problem("keep-methods lib.A\n"), Some((1, Problem.UnknownDirective("keep-methods"))))
    assert(Problem.UnknownDirective("x").message.contains("keep-members"))
    assertEquals(problem("keep-members lib.A\nkeep-members lib..B\nnonsense\n"), Some((2, Problem.InvalidName("lib..B"))))
  }

  test("a file that is not there is Unreadable, and a bad line's message names the file and line") {
    val missing = Files.createTempDirectory("rules").resolve("nope.txt")
    assert(KeepRule.parse(missing).left.exists(_.isInstanceOf[KeepRuleError.Unreadable]))
    val bad = file("keep-members\n")
    val msg = KeepRule.parse(bad).left.toOption.get.message
    assert(msg.contains(bad.toString) && msg.contains("line 1"), msg)
  }

  test("the standard rules are exactly the built-in packs") {
    assertEquals(KeepRule.toKeep(KeepRule.standard()), Keep.standard)
    // Names are dotted class names, except for the kinds that name files or suffixes.
    val classKinds = KeepRule.standard().asScala.filterNot(r => r.kind == KeepRuleKind.KEEP_CLASSES_NAMED_IN || r.kind == KeepRuleKind.KEEP_GENERATED_CLASSES)
    assert(classKinds.forall(r => r.names.asScala.forall(_.contains('.'))), "dotted names")
  }

  // ---- through the facade ----

  private val lib = Map(
    "lib/Wired.java"     -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Wired { }",
    "lib/Svc.java"       -> "package lib; public class Svc { public Svc() { } @Wired public void go() { Gone.x(); } }",
    "lib/Container.java" -> "package lib; public class Container { public static Object start(Class<?> t) { return null; } }"
  )

  private def check(rules: java.util.List[KeepRule]): LinkageResult =
    val (v1, v2) = GoneLibrary.build(lib)
    val app = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Container.start(lib.Svc.class); } }"), Seq(v1))
    Linkage.check(
      LinkageRequest(
        java.util.List.of(app),
        java.util.List.of(ClasspathEntry("lib", Jars.of(v2))),
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.METHOD,
        java.util.Optional.empty[Path](),
        rules
      )
    )

  test("rules in the request decide what a framework is assumed to call") {
    val withRule = KeepRule.parse(file("keep-members lib.Wired\n")).map(_.asJava).fold(e => fail(e.message), identity)
    assertEquals(check(withRule).conflicts.asScala.map(c => (c.dependency.fromMethodName, c.label)).toList, List(("go", ConflictLabel.REFLECTIVE)))
    assertEquals(check(java.util.List.of()).conflicts.asScala.map(c => (c.dependency.fromMethodName, c.label)).toList, List(("go", ConflictLabel.LATENT)))
  }

  test("a request without rules uses the standard packs") {
    val (v1, v2) = GoneLibrary.build(lib)
    val app = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { lib.Container.start(lib.Svc.class); } }"), Seq(v1))
    val request = LinkageRequest(
      java.util.List.of(app),
      java.util.List.of(ClasspathEntry("lib", Jars.of(v2))),
      CheckScope.App,
      java.util.Set.of(),
      java.util.List.of(),
      CheckPrecision.METHOD,
      java.util.Optional.empty[Path]()
    )
    assertEquals(request.keepRules, KeepRule.standard())
  }

  test("keep-overrides is a directive, and a rule of that kind reaches the engine as supertypes") {
    val parsed = KeepRule.parse(file("keep-overrides javax.servlet.http.HttpServlet javax.servlet.Filter\n"))
    assertEquals(
      parsed.map(_.map(r => (r.kind, names(r)))),
      Right(List((KeepRuleKind.KEEP_OVERRIDES, List("javax.servlet.http.HttpServlet", "javax.servlet.Filter"))))
    )
    assertEquals(KeepRule.toKeep(parsed.toOption.get.asJava), List(Keep.Overrides(Keep.classNames("javax/servlet/http/HttpServlet", "javax/servlet/Filter"))))
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_OVERRIDES, Nil), Left(Problem.NoNames))
  }

  test("an overrides rule in the request makes a subtype's override reachable, end to end through the facade") {
    val handlerLib = Map(
      "lib/Handler.java" -> "package lib; public abstract class Handler { public abstract void handle(); }",
      "lib/HImpl.java"   -> "package lib; public class HImpl extends Handler { public HImpl() { } public void handle() { Gone.x(); } }"
    )
    val (v1, v2) = GoneLibrary.build(handlerLib)
    val app      = compile(Map("app/Main.java" -> "package app; public class Main { public static void run() { Object o = lib.HImpl.class; } }"), Seq(v1))
    def check(rules: java.util.List[KeepRule]) = Linkage.check(
      LinkageRequest(
        java.util.List.of(app),
        java.util.List.of(ClasspathEntry("lib", Jars.of(v2))),
        CheckScope.App,
        java.util.Set.of(),
        java.util.List.of(),
        CheckPrecision.METHOD,
        java.util.Optional.empty[Path](),
        rules
      )
    )
    def labels(r: LinkageResult) = r.conflicts.asScala.map(c => (c.dependency.fromMethodName, c.label)).toList
    val rule                     = KeepRule.of(KeepRuleKind.KEEP_OVERRIDES, List("lib.Handler")).fold(p => fail(p.message), identity)
    assertEquals(labels(check(java.util.List.of(rule))), List(("handle", ConflictLabel.REFLECTIVE)))
    assertEquals(labels(check(java.util.List.of())), List(("handle", ConflictLabel.LATENT)))
  }

  test("keep-bean-arguments names a call as owner.method, and a name without an owner is not one") {
    val parsed = KeepRule.parse(file("keep-bean-arguments com.fasterxml.jackson.databind.ObjectMapper.readValue com.google.gson.Gson.fromJson\n"))
    assertEquals(
      parsed.map(_.map(r => (r.kind, names(r)))),
      Right(List((KeepRuleKind.KEEP_BEAN_ARGUMENTS, List("com.fasterxml.jackson.databind.ObjectMapper.readValue", "com.google.gson.Gson.fromJson"))))
    )
    assertEquals(
      KeepRule.toKeep(parsed.toOption.get.asJava),
      List(Keep.BeanArguments(Keep.callNames("com/fasterxml/jackson/databind/ObjectMapper.readValue", "com/google/gson/Gson.fromJson")))
    )
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_BEAN_ARGUMENTS, List("readValue")), Left(Problem.InvalidName("readValue")))
    assert(KeepRule.of(KeepRuleKind.KEEP_BEAN_ARGUMENTS, List("a.B.c")).isRight)
  }

  test("the Jackson pack in the standard rules includes the calls that take a class") {
    val standard = KeepRule.standard().asScala.filter(_.kind == KeepRuleKind.KEEP_BEAN_ARGUMENTS).flatMap(_.names.asScala).toSet
    assert(standard.contains("com.fasterxml.jackson.databind.ObjectMapper.readValue"), standard.toString)
  }

  test("keep-built-arguments is a directive of its own, with the same owner.method names") {
    val parsed = KeepRule.parse(file("keep-built-arguments com.google.gson.Gson.fromJson\n"))
    assertEquals(parsed.map(_.map(r => (r.kind, names(r)))), Right(List((KeepRuleKind.KEEP_BUILT_ARGUMENTS, List("com.google.gson.Gson.fromJson")))))
    assertEquals(KeepRule.toKeep(parsed.toOption.get.asJava), List(Keep.BuiltArguments(Keep.callNames("com/google/gson/Gson.fromJson"))))
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_BUILT_ARGUMENTS, List("fromJson")), Left(Problem.InvalidName("fromJson")))
  }

  test("the standard rules include the Gson and Servlet packs") {
    val byKind = KeepRule.standard().asScala.groupBy(_.kind).view.mapValues(_.flatMap(_.names.asScala).toSet).toMap
    assert(byKind(KeepRuleKind.KEEP_BUILT_ARGUMENTS).contains("com.google.gson.Gson.fromJson"))
    assert(byKind(KeepRuleKind.FOLLOW_CLASS_VALUES).contains("com.google.gson.annotations.JsonAdapter"))
    val overrides = byKind(KeepRuleKind.KEEP_OVERRIDES)
    assert(
      Set("javax.servlet.Servlet", "javax.servlet.Filter", "jakarta.servlet.Servlet", "jakarta.servlet.http.HttpSessionListener").subsetOf(overrides),
      overrides.toString
    )
  }

  test("keep-classes-named-in names files, which keep their dots, and is not part of the standard packs") {
    val parsed = KeepRule.parse(file("keep-classes-named-in reference.conf META-INF/persistence.xml\n"))
    assertEquals(
      parsed.map(_.map(r => (r.kind, names(r)))),
      Right(List((KeepRuleKind.KEEP_CLASSES_NAMED_IN, List("reference.conf", "META-INF/persistence.xml"))))
    )
    assertEquals(KeepRule.toKeep(parsed.toOption.get.asJava), List(Keep.ClassesNamedIn(Keep.fileNames("reference.conf", "META-INF/persistence.xml"))))
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_CLASSES_NAMED_IN, List("/etc/x.conf")), Left(Problem.InvalidName("/etc/x.conf")))
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_CLASSES_NAMED_IN, Nil), Left(Problem.NoNames))
    // A file of a default config (`reference.conf`) names classes that a given application may never use, so it is opt-in. The only files in a pack
    // are Spring Boot's own lists of autoconfiguration, whose classes carry their conditions (`RequiresClasses`).
    val packFiles = KeepRule.standard().asScala.filter(_.kind == KeepRuleKind.KEEP_CLASSES_NAMED_IN).flatMap(_.names.asScala).toSet
    assertEquals(packFiles, Set("META-INF/spring.factories", "META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports"))
  }

  private inline def caseCount[T](using m: Mirror.SumOf[T]): Int = constValueTuple[m.MirroredElemLabels].size

  /** A well formed name in each form. */
  private def sampleName(form: NameForm): String = form match
    case NameForm.ClassNames  => "a.b.Name"
    case NameForm.CallNames   => "a.b.Owner.method"
    case NameForm.FileNames   => "META-INF/x.conf"
    case NameForm.SuffixNames => "_$x"

  test("every kind of rule is described once, and every engine rule is read back by exactly one kind") {
    val kinds = KeepRuleKind.values.toList
    assertEquals(kinds.map(RuleKinds.describe(_).directive).distinct.size, kinds.size, "one directive per kind")
    // The engine's rules and the facade's kinds are in step: a new case of either without the other fails here.
    assertEquals(caseCount[Keep], kinds.size, "a kind per engine rule")
    kinds.foreach: kind =>
      val form = RuleKinds.describe(kind).form
      val rule = KeepRule.of(kind, List(sampleName(form)))
      assert(rule.isRight, s"$kind rejects a well formed ${form}: $rule")
      val keeps = KeepRule.toKeep(java.util.List.of(rule.toOption.get))
      assertEquals(keeps.flatMap(RuleKinds.kindOf).map(_._1), List(kind), s"$kind reads back as itself")
  }

  test("names are checked in the form of the kind: calls need an owner, files no leading slash, suffixes no dots") {
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_BEAN_ARGUMENTS, List("method")), Left(Problem.InvalidName("method")))
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_CLASSES_NAMED_IN, List("/etc/x.conf")), Left(Problem.InvalidName("/etc/x.conf")))
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_GENERATED_CLASSES, List("a.b")), Left(Problem.InvalidName("a.b")))
    assert(KeepRule.of(KeepRuleKind.KEEP_GENERATED_CLASSES, List("_$logger")).isRight)
  }

  test("keep-generated-classes names suffixes, which are kept as they are, and the jboss-logging pack is standard") {
    val parsed = KeepRule.parse(file("keep-generated-classes _$logger _$bundle\n"))
    assertEquals(parsed.map(_.map(r => (r.kind, names(r)))), Right(List((KeepRuleKind.KEEP_GENERATED_CLASSES, List("_$logger", "_$bundle")))))
    assertEquals(KeepRule.toKeep(parsed.toOption.get.asJava), List(Keep.GeneratedClasses(Keep.suffixes("_$logger", "_$bundle"))))
    assert(KeepRule.standard().asScala.exists(_.kind == KeepRuleKind.KEEP_GENERATED_CLASSES))
  }

  test("keep-constructors and requires-classes are directives over annotation classes") {
    val parsed = KeepRule.parse(
      file("keep-constructors org.springframework.stereotype.Component\nrequires-classes org.springframework.boot.autoconfigure.condition.ConditionalOnClass\n")
    )
    assertEquals(
      parsed.map(_.map(r => (r.kind, names(r)))),
      Right(
        List(
          (KeepRuleKind.KEEP_CONSTRUCTORS, List("org.springframework.stereotype.Component")),
          (KeepRuleKind.REQUIRES_CLASSES, List("org.springframework.boot.autoconfigure.condition.ConditionalOnClass"))
        )
      )
    )
    assertEquals(
      KeepRule.toKeep(parsed.toOption.get.asJava),
      List(
        Keep.Constructors(Keep.classNames("org/springframework/stereotype/Component")),
        Keep.RequiresClasses(Keep.classNames("org/springframework/boot/autoconfigure/condition/ConditionalOnClass"))
      )
    )
    assert(KeepRule.standard().asScala.exists(_.kind == KeepRuleKind.KEEP_CONSTRUCTORS))
    assert(KeepRule.standard().asScala.exists(_.kind == KeepRuleKind.REQUIRES_CLASSES))
  }

  test("keep-constructed-arguments is a directive with owner.method names, and the Akka and Pekko packs are in the standard rules") {
    val parsed = KeepRule.parse(file("keep-constructed-arguments org.apache.pekko.actor.Props.create\n"))
    assertEquals(
      parsed.map(_.map(r => (r.kind, names(r)))),
      Right(List((KeepRuleKind.KEEP_CONSTRUCTED_ARGUMENTS, List("org.apache.pekko.actor.Props.create"))))
    )
    assertEquals(KeepRule.toKeep(parsed.toOption.get.asJava), List(Keep.ConstructedArguments(Keep.callNames("org/apache/pekko/actor/Props.create"))))
    assertEquals(KeepRule.of(KeepRuleKind.KEEP_CONSTRUCTED_ARGUMENTS, List("create")), Left(Problem.InvalidName("create")))
    val standard = KeepRule.standard().asScala.filter(_.kind == KeepRuleKind.KEEP_CONSTRUCTED_ARGUMENTS).flatMap(_.names.asScala).toSet
    assert(
      Set("akka.actor.Props.create", "akka.actor.Props$.apply", "org.apache.pekko.actor.Props.create", "org.apache.pekko.actor.Props$.apply").subsetOf(
        standard
      ),
      standard.toString
    )
  }
