package reachablelinkage.fixture

import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.Properties

import reachablelinkage.classpath.ClassSource

/** Compiles Scala sources in-process. Each compiler lives in its own classloader whose parent is the platform loader, so 2.12 and 2.13 libraries never meet our
  * own Scala 3 classpath. The compiler classpaths come from the build (see `scala-compilers.properties`).
  */
object ScalaFixture:

  private lazy val classpaths: Properties =
    val props = new Properties()
    val in    = getClass.getResourceAsStream("/scala-compilers.properties")
    require(in != null, "scala-compilers.properties missing: run the tests through sbt")
    try props.load(in)
    finally in.close()
    props

  private def entries(v: ScalaVersion): Seq[Path] =
    classpaths.getProperty(v.key).split(java.io.File.pathSeparator).toSeq.map(Paths.get(_))

  /** The jars the build resolved for a test-only configuration (`scala-compilers.properties`), by key. */
  def jars(key: String): Seq[Path] = classpaths.getProperty(key).split(java.io.File.pathSeparator).toSeq.map(Paths.get(_))

  private val loaders                              = new java.util.concurrent.ConcurrentHashMap[ScalaVersion, ClassLoader]()
  private def loader(v: ScalaVersion): ClassLoader =
    loaders.computeIfAbsent(v, _ => new URLClassLoader(entries(v).map(_.toUri.toURL).toArray[URL], ClassLoader.getPlatformClassLoader))

  /** The Scala standard library jars fixtures compile against, and which the app runs with. */
  def library(v: ScalaVersion): Seq[Path] =
    entries(v).filter: p =>
      val n = p.getFileName.toString
      n.startsWith("scala-library-") || n.startsWith("scala3-library_3-")

  /** Compiles `sources` against `classpath` (the Scala library is added) and returns the output directory. */
  def compile(v: ScalaVersion, sources: Map[String, String], classpath: Seq[Path] = Nil): Path =
    val work  = FixtureDirs.fresh()
    val out   = Files.createDirectory(work.resolve("out"))
    val files = FixtureDirs.writeSources(work, sources)
    val cp    = (library(v) ++ classpath).map(_.toString).mkString(java.io.File.pathSeparator)
    val args  =
      if v.isScala3 then Seq("-d", out.toString, "-classpath", cp, "-color:never", "-nowarn")
      else Seq("-d", out.toString, "-classpath", cp, "-nowarn")
    val cl = loader(v)
    // One compiler instance per version shares a classloader and global state: compile one at a time.
    val ok = cl.synchronized:
      withContextLoader(cl):
        if v.isScala3 then
          val reporter = invokeMain(cl, "dotty.tools.dotc.Main$", (args ++ files).toArray)
          !reporter.getClass.getMethod("hasErrors").invoke(reporter).asInstanceOf[Boolean]
        else invokeMain(cl, "scala.tools.nsc.Main$", (args ++ files).toArray).asInstanceOf[Boolean]
    require(ok, s"scalac $v failed; compiler output is above")
    out

  /** Calls `process(args)` on the compiler's `Main` singleton. */
  private def invokeMain(cl: ClassLoader, module: String, args: Array[String]): AnyRef =
    val cls = Class.forName(module, true, cl)
    val obj = cls.getField("MODULE$").get(null)
    cls.getMethod("process", classOf[Array[String]]).invoke(obj, args)

  private def withContextLoader[A](cl: ClassLoader)(body: => A): A =
    val t   = Thread.currentThread
    val old = t.getContextClassLoader
    t.setContextClassLoader(cl)
    try body
    finally t.setContextClassLoader(old)

  def source(dir: Path): ClassSource                    = JavaFixture.source(dir)
  def librarySources(v: ScalaVersion): Seq[ClassSource] = library(v).map(JavaFixture.source)
