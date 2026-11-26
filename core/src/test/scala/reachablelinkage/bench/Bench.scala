package reachablelinkage.bench

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*

import com.spotify.missinglink.ConflictChecker
import com.spotify.missinglink.datamodel.{Artifact, ArtifactBuilder, ArtifactName, ClassTypeDescriptor, DeclaredClass, FieldDependency, MethodDependency}
import reachablelinkage.api.*
import reachablelinkage.classpath.ClassSource
import reachablelinkage.fixture.{MissinglinkOracle, ScalaFixture, ScalaVersion}

/** On-demand benchmark on a real classpath. Not a test suite: nothing here runs with `sbt test`. Each tool runs in its own JVM so peak memory can be measured
  * per tool.
  *
  *   - `app <classpath-file> <out-dir>`: compile a small app against the classpath with scalac 2.13
  *   - `app-src <classpath-file> <scala-file> <out-dir>`: compile any one-file app the same way
  *   - `app-src3 <classpath-file> <scala-file> <out-dir>`: the same with Scala 3
  *   - `ours <classpath-file> <app-dir> <class|method> <app|full> <out-file>`
  *   - `ml <classpath-file> <app-dir> <scan|noscan> <out-file>`
  *
  * The classpath file has one jar per line. Results go to `out-file` as tab-separated rows.
  */
object Bench:

  def main(args: Array[String]): Unit = args.toList match
    case "app" :: cp :: out :: Nil                          => makeApp(jars(cp), Paths.get(out))
    case "app-src" :: cp :: src :: out :: Nil               => compileApp(jars(cp), Files.readString(Paths.get(src)), Paths.get(out))
    case "app-src3" :: cp :: src :: out :: Nil              => compileApp(jars(cp), Files.readString(Paths.get(src)), Paths.get(out), ScalaVersion.V3Next)
    case "ours" :: cp :: app :: prec :: scope :: out :: Nil => ours(jars(cp), Paths.get(app), prec, scope, Paths.get(out))
    case "ml" :: cp :: app :: scan :: out :: Nil            => ml(jars(cp), Paths.get(app), scan == "scan", Paths.get(out))
    case other                                              => sys.error(s"usage: see Bench scaladoc; got $other")

  private def jars(file: String): Seq[Path] =
    Files.readAllLines(Paths.get(file)).asScala.map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toSeq

  private val appSource =
    """package app
      |
      |import play.api.routing.sird._
      |import play.api.mvc.Results
      |import play.core.server._
      |
      |object Main {
      |  def main(args: Array[String]): Unit = {
      |    val server = AkkaHttpServer.fromRouterWithComponents(ServerConfig(port = Some(0), mode = play.api.Mode.Test)) { components =>
      |      import components.{ defaultActionBuilder => Action }
      |      {
      |        case GET(p"/") => Action { Results.Ok("hello from play") }
      |      }
      |    }
      |    try {
      |      val port = server.httpPort.get
      |      println("response: " + scala.io.Source.fromURL(s"http://localhost:$port/").mkString)
      |    } finally server.stop()
      |  }
      |}
      |""".stripMargin

  private def makeApp(classpath: Seq[Path], out: Path): Unit = compileApp(classpath, appSource, out)

  private def compileApp(classpath: Seq[Path], source: String, out: Path, version: ScalaVersion = ScalaVersion.V213): Unit =
    val compiled = ScalaFixture.compile(version, Map("app/Main.scala" -> source), classpath)
    Files.createDirectories(out)
    val stream = Files.walk(compiled)
    try
      stream.iterator.asScala.foreach: p =>
        val target = out.resolve(compiled.relativize(p).toString)
        if Files.isDirectory(p) then Files.createDirectories(target) else Files.copy(p, target, StandardCopyOption.REPLACE_EXISTING)
    finally stream.close()
    println(s"compiled app to $out")

  private def ours(classpath: Seq[Path], app: Path, precision: String, scope: String, out: Path): Unit =
    val request = LinkageRequest(
      java.util.List.of(app),
      classpath.map(j => ClasspathEntry(j.getFileName.toString, j)).asJava,
      if scope == "full" then CheckScope.Full else CheckScope.App,
      java.util.Set.of(),
      java.util.List.of(),
      precision match
        case "method" => CheckPrecision.METHOD
        case "rta"    => CheckPrecision.METHOD_RTA
        case _        => CheckPrecision.CLASS,
      java.util.Optional.ofNullable(System.getProperty("bench.cache")).map(p => Paths.get(p)),
      // -Dbench.rules=<rules file>: more rules on top of the built-in packs
      (KeepRule.standard().asScala ++ Option(System.getProperty("bench.rules")).toList.flatMap(f =>
        KeepRule.parse(Paths.get(f)).fold(e => { Console.err.println(e.message); sys.exit(1) }, identity)
      )).asJava,
      // -Dbench.log=<class-load log of a real run>: the classes it loaded are roots too
      Option(System.getProperty("bench.log"))
        .map(f => ClassLoadLog.parse(Paths.get(f)).fold(e => { Console.err.println(e.message); sys.exit(1) }, _.classes))
        .getOrElse(java.util.Set.of())
    )
    val t0     = System.nanoTime
    val result = Linkage.check(request)
    val ms     = (System.nanoTime - t0) / 1_000_000
    val rows   = result.conflicts.asScala.map: c =>
      val d = c.dependency
      Seq(c.category, c.label, d.fromClass, d.fromMethodName, d.targetClass, d.targetMemberName, c.usedBy, c.existsIn, c.reason).mkString("\t")
    Files.write(out, rows.asJava)
    val byLabel = result.conflicts.asScala.groupBy(_.label).view.mapValues(_.size).toMap
    println(s"ours precision=$precision scope=$scope conflicts=${rows.size} byLabel=$byLabel warnings=${result.warnings.size} checkMs=$ms")

  private def ml(classpath: Seq[Path], app: Path, scan: Boolean, out: Path): Unit =
    val t0                = System.nanoTime
    val appSrc            = ClassSource.open(app).toOption.get
    val appArt            = MissinglinkOracle.load("app", appSrc)
    val libs              = classpath.map(j => MissinglinkOracle.load(j.getFileName.toString, ClassSource.open(j).toOption.get))
    val jdk               = MissinglinkOracle.jdk
    val loadedMs          = (System.nanoTime - t0) / 1_000_000
    val all               = (appArt +: libs) :+ jdk
    val project: Artifact =
      if scan then
        val merged = new java.util.HashMap[ClassTypeDescriptor, DeclaredClass]()
        (appArt +: libs).foreach(a => merged.putAll(a.classes()))
        new ArtifactBuilder().name(new ArtifactName("project")).classes(merged).build()
      else appArt
    val t1        = System.nanoTime
    val conflicts = ConflictChecker().check(project, (appArt +: libs).asJava, all.asJava).asScala
    val checkMs   = (System.nanoTime - t1) / 1_000_000
    val rows      = conflicts.map: c =>
      val member = c.dependency match
        case m: MethodDependency => m.targetMethod.name
        case f: FieldDependency  => f.fieldName
        case _                   => ""
      Seq(
        c.category.name,
        "-",
        c.dependency.fromClass.getClassName,
        "-",
        c.dependency.targetClass.getClassName,
        member,
        c.usedBy.name,
        c.existsIn.name,
        c.reason
      ).mkString("\t")
    Files.write(out, rows.asJava)
    println(s"ml scan=$scan conflicts=${rows.size} loadMs=$loadedMs checkMs=$checkMs")
