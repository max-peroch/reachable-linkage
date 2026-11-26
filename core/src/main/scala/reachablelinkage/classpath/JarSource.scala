package reachablelinkage.classpath

import java.nio.file.Path
import java.util.jar.{JarEntry, JarFile}
import scala.jdk.CollectionConverters._
import scala.util.Using

import reachablelinkage.classfile.ClassInfo
import reachablelinkage.symbols.Symbols

/** A jar. With a cache directory its classes come from a summary parsed once and kept (see [[JarSummary]]). */
private[classpath] final class JarSource(val origin: Path, cacheDirectory: Option[Path], release: Int) extends ClassSource {
  private val jar = new JarFile(origin.toFile)

  // The entry each class comes from, for a JVM running Java `release` (see JarEntries): a multi-release jar's overlays count.
  private lazy val resolved: Vector[(String, JarEntry)] = JarEntries.resolve(jar, release)
  private lazy val entryOf: Map[String, JarEntry]       = resolved.toMap

  def classNames: Iterator[String] = resolved.iterator.map(_._1)

  def read(internalName: String): Option[Array[Byte]] =
    entryOf.get(internalName).map(e => Using.resource(jar.getInputStream(e))(in => Streams.readAll(in, e.getSize)))

  override def serviceFiles: Iterator[String] =
    jar.entries.asScala.filter(e => !e.isDirectory && e.getName.startsWith(ClassSource.ServicesDir)).map(_.getName)

  override def resourceNames: Iterator[String] =
    jar.entries.asScala.filter(e => !e.isDirectory && !e.getName.endsWith(".class") && !e.getName.startsWith("META-INF/versions/")).map(_.getName)

  override def readResource(path: String): Option[Array[Byte]] = readEntry(path)

  private def readEntry(name: String): Option[Array[Byte]] =
    Option(jar.getJarEntry(name)).map { e =>
      Using.resource(jar.getInputStream(e))(in => Streams.readAll(in, e.getSize))
    }

  // Opened on first use, because a jar that is never asked for a class needs no summary.
  private lazy val summary: (JarSummary.Reader, SummaryOutcome) = cacheDirectory match {
    case None      => (null, SummaryOutcome.Disabled)
    case Some(dir) => JarSummary.open(jar, resolved, dir)
  }

  /** What happened when the summary was first needed; for tests and tools. */
  private[classpath] def summaryOutcome: SummaryOutcome = summary._2

  override private[reachablelinkage] def summarized(internalName: String, syms: Symbols): ClassInfo =
    summary._1 match {
      case null   => null
      case reader =>
        // A damaged summary must not break a check: this class is parsed from the jar instead.
        try reader.info(internalName, syms)
        catch { case _: RuntimeException => null }
    }

  override lazy val fingerprint: Option[String] =
    try Some(JarSummary.fingerprint(resolved).map(b => f"${b & 0xff}%02x").mkString)
    catch { case _: RuntimeException => None }

  def close(): Unit = jar.close()
}
