package reachablelinkage.linkage

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.*
import reachablelinkage.fixture.{ChildRun, ScalaFixture}
import reachablelinkage.fixture.JavaFixture.compile

/** logback keeps its optional dependencies (Janino for conditional configuration, Jakarta Servlet and Mail for some appenders and filters) off the classpath
  * unless an application asks for them, so a check that treats every reference as a break reports dozens of them for an application that logs normally. The
  * application here logs through slf4j and runs: nothing it can reach is broken, and what logback itself guards with a `catch` is labelled so.
  */
class LogbackSuite extends munit.FunSuite:

  private lazy val jars: Seq[Path] = ScalaFixture.jars("logback")

  private lazy val app: Path = compile(
    Map(
      "app/Main.java" ->
        """package app;
        |public class Main {
        |  public static void run() {
        |    org.slf4j.LoggerFactory.getLogger("app").info("hello {}", "logback");
        |  }
        |}""".stripMargin
    ),
    jars
  )

  private def check(scope: CheckScope): List[LinkageConflict] =
    val request = LinkageRequest(
      java.util.List.of(app),
      jars.map(jar => ClasspathEntry(jar.getFileName.toString, jar)).asJava,
      scope,
      java.util.Set.of[String](),
      java.util.List.of()
    )
    val result = Linkage.check(request)
    assertEquals(result.warnings.size, 0)
    result.conflicts.asScala.toList

  test("control: the app runs with the logback jars alone") {
    assertEquals(ChildRun.run(app +: jars), None)
  }

  test("starting from the app, nothing fails and what logback guards is labelled guarded") {
    val found = check(CheckScope.App)
    assert(found.nonEmpty, "logback's guarded reference to Janino was not found")
    assertEquals(found.map(_.label).distinct, List(ConflictLabel.GUARDED))
    assert(
      found.exists(c =>
        c.dependency.fromClass == "ch.qos.logback.core.joran.conditional.PropertyEvalScriptBuilder" && c.dependency.targetClass
          .startsWith("org.codehaus.janino.")
      ),
      found.toString
    )
  }

  test("checking every class, the optional servlet and mail code is latent and nothing fails") {
    val found = check(CheckScope.Full)
    assertEquals(found.filter(c => c.label == ConflictLabel.REACHABLE || c.label == ConflictLabel.REFLECTIVE), Nil)
    val latent = found.filter(_.label == ConflictLabel.LATENT).map(_.dependency.targetClass)
    assert(latent.exists(_.startsWith("jakarta.servlet.")), latent.toString)
    assert(latent.exists(_.startsWith("jakarta.mail.")), latent.toString)
  }
