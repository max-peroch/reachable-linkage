package reachablelinkage.fixture

import java.nio.file.{Files, Path}
import javax.tools.ToolProvider

import reachablelinkage.classpath.ClassSource

/** Compiles real Java sources with javac, so fixtures contain the bytecode javac actually emits. */
object JavaFixture:
  private val compiler = ToolProvider.getSystemJavaCompiler

  /** Compiles `sources` (path -> content) against `classpath` and returns the output directory. */
  def compile(sources: Map[String, String], classpath: Seq[Path] = Nil, extraArgs: Seq[String] = Nil): Path =
    val work  = FixtureDirs.fresh()
    val out   = Files.createDirectory(work.resolve("out"))
    val files = FixtureDirs.writeSources(work, sources)
    val cp    = classpath.map(_.toString).mkString(java.io.File.pathSeparator)
    // --release and --add-exports cannot be combined, so extra arguments mean compiling against the running JDK's own classes.
    val args = Seq("-d", out.toString, "-cp", cp, "-proc:none", "-nowarn") ++ (if extraArgs.isEmpty then Seq("--release", "17") else extraArgs) ++ files
    val err  = new java.io.ByteArrayOutputStream()
    val rc   = compiler.run(null, null, err, args*)
    require(rc == 0, s"javac failed:\n${err.toString}")
    out

  def source(dir: Path): ClassSource = ClassSource.open(dir).fold(e => throw new IllegalStateException(e.message), identity)
