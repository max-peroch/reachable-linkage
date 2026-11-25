package reachablelinkage.api

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** The classes a real run loaded, read from a JVM log (`java -Xlog:class+load:file=run.log ...`, or `-verbose:class` on Java 8). Whatever the static analysis
  * cannot see (a class named by a computed string, built by a framework nobody modelled) shows up here, so each class in the log that is on the classpath is
  * treated as used: it is checked, its constructors are roots, and under `METHOD_RTA` it counts as instantiated. Only what the run did is covered, so use a run
  * that exercises the application.
  *
  * `classes` are dotted binary names; lambda and other hidden classes (`Foo$$Lambda/0x...`) are left out.
  */
final case class ClassLoadLog private (classes: java.util.Set[String])

object ClassLoadLog:
  // JDK 9+: any decorations in brackets (`[0.08s][info][class,load]`, `[0.08s]`, or none), then `name source: where`.
  private val Unified = raw"""^(?:\[[^\]]*\])*\s*([^\s\[\]]+) source: .*""".r
  // JDK 8 `-verbose:class`.
  private val Verbose = raw"""^\[Loaded (\S+) from .*""".r

  /** The class a log line says was loaded; none for any other line, and for a hidden class. */
  private[api] def loadedClass(line: String): Option[String] =
    val name = line match
      case Unified(n) => Some(n)
      case Verbose(n) => Some(n)
      case _          => None
    // Both patterns capture a token without whitespace; a `/` marks a hidden class.
    name.filterNot(_.contains('/'))

  /** The classes in log lines, in order of first load. */
  private[api] def fromLines(lines: List[String]): List[String] = lines.flatMap(loadedClass).distinct

  /** Reads a class-load log, or says why it cannot be used. */
  def parse(file: Path): Either[ClassLoadLogError, ClassLoadLog] =
    for
      lines <- ReadLines.of(file).left.map(f => ClassLoadLogError.Unreadable(f.file, f.cause))
      classes = fromLines(lines)
      _ <- Either.cond(classes.nonEmpty, (), ClassLoadLogError.NoClassesLogged(file))
    yield new ClassLoadLog(java.util.Collections.unmodifiableSet(new java.util.LinkedHashSet[String](classes.asJava)))
