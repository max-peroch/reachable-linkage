package reachablelinkage.linkage

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.*
import reachablelinkage.fixture.{ChildRun, ScalaFixture, ScalaVersion}

/** A real application on sttp and tapir, built against circe 0.14.10 and run once with it and once with circe 0.14.1, which lacks the classes circe's `derives`
  * support needs. The run is the control: it works with the first and fails with `NoClassDefFoundError` on the second. The check must find nothing that fails
  * on the first and the break, `Reachable`, on the second.
  */
class SttpTapirSuite extends munit.FunSuite:

  override def munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private val missing = "io.circe.derivation.ConfiguredCodec$"

  private val source = Map(
    "app/Main.scala" ->
      """package app
        |
        |import io.circe.Codec
        |import sttp.client4.*
        |import sttp.tapir.*
        |import sttp.tapir.client.sttp4.SttpClientInterpreter
        |import sttp.tapir.json.circe.*
        |
        |final case class Greeting(message: String) derives Codec.AsObject, Schema
        |
        |object Main:
        |  private val hello = endpoint.get.in("hello" / path[String]("name")).out(jsonBody[Greeting])
        |
        |  def run(): Unit =
        |    val request = SttpClientInterpreter().toRequestThrowDecodeFailures(hello, Some(uri"http://localhost:1"))
        |    require(request("tapir").toCurl.nonEmpty)
        |""".stripMargin
  )

  private lazy val newJars: Seq[Path] = ScalaFixture.jars("sttp-tapir")
  private lazy val oldJars: Seq[Path] = ScalaFixture.jars("sttp-tapir-old-circe")
  private lazy val app: Path          = ScalaFixture.compile(ScalaVersion.V3Next, source, newJars)

  private def check(classpath: Seq[Path]): List[LinkageConflict] =
    val request = LinkageRequest(
      java.util.List.of(app),
      classpath.map(jar => ClasspathEntry(jar.getFileName.toString, jar)).asJava,
      CheckScope.App,
      java.util.Set.of[String](),
      java.util.List.of()
    )
    val result = Linkage.check(request)
    assertEquals(result.warnings.size, 0)
    result.conflicts.asScala.toList

  private def fails(conflict: LinkageConflict): Boolean =
    conflict.label == ConflictLabel.REACHABLE || conflict.label == ConflictLabel.REFLECTIVE

  test("control: the app runs with circe 0.14.10") {
    assertEquals(ChildRun.run(app +: newJars), None)
  }

  test("control: the app fails with NoClassDefFoundError on circe 0.14.1") {
    val failure = ChildRun.run(app +: oldJars)
    assert(
      failure.exists(_.exists((error, message) => error == "java.lang.NoClassDefFoundError" && message.contains(missing.replace('.', '/')))),
      failure.toString
    )
  }

  test("the check finds nothing that fails with circe 0.14.10") {
    assertEquals(check(newJars).filter(fails), Nil)
  }

  test("the check labels the break with circe 0.14.1 Reachable, in the code that failed") {
    val found = check(oldJars).filter(fails)
    assert(found.nonEmpty, "no conflict that fails was found")
    assert(found.forall(_.category == ConflictCategory.CLASS_NOT_FOUND), found.toString)
    assert(found.exists(c => c.dependency.targetClass == missing && c.dependency.fromClass == "app.Greeting$"), found.toString)
  }
