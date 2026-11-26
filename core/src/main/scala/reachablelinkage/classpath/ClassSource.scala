package reachablelinkage.classpath

import java.nio.file.{Files, Path}

import reachablelinkage.classfile.ClassInfo
import reachablelinkage.symbols.Symbols

/** One classpath entry. Reads only the table of contents up front; bytes are read on demand. Open on purpose: jars, directories and the JDK implement it, and
  * tests add their own.
  */
trait ClassSource extends AutoCloseable {
  def origin: Path

  /** Internal names of all classes in this entry, e.g. `a/b/C`. */
  def classNames: Iterator[String]
  def read(internalName: String): Option[Array[Byte]]

  /** Can code on the classpath (the unnamed module) name this class? Only a JDK class can say no: its module may not export its package to everyone
    * (`sun.security.x509` in `java.base`). True for anything this entry cannot tell.
    */
  def isExported(internalName: String): Boolean = true

  /** What went wrong with parts of this entry that the check can carry on without, as warnings for the result (an unreadable JDK module, say). */
  def problems: List[String] = Nil

  /** Paths of the service files under `META-INF/services/` in this entry, e.g. `META-INF/services/a.b.Service`. */
  def serviceFiles: Iterator[String] = Iterator.empty

  /** The class parsed in an earlier run and kept in a cache, with ids of `syms`; null if this entry has no cached summary or it holds no record for the class,
    * in which case the caller parses `read`.
    */
  private[reachablelinkage] def summarized(internalName: String, syms: Symbols): ClassInfo = null

  /** A hex fingerprint of this entry's classes (see `JarSummary.fingerprint`), which changes exactly when the classes do, or none for a class directory: that
    * is what is being edited, so it is never treated as a dependency that changed.
    */
  def fingerprint: Option[String] = None

  /** Paths of the files in this entry that are not classes, e.g. `reference.conf` or `META-INF/persistence.xml` (a multi-release jar's overlays are left out).
    */
  def resourceNames: Iterator[String] = Iterator.empty

  /** The bytes of a non-class file in this entry, by path. */
  def readResource(path: String): Option[Array[Byte]] = None
}

object ClassSource {

  /** Where `ServiceLoader` looks on the classpath. */
  val ServicesDir: String = "META-INF/services/"

  /** The running JDK's classes. With a `cacheDirectory` the class listing is kept on disk, per JDK image, which saves 300 to 400 ms on a cold JVM; without one
    * nothing is written.
    */
  def jdk(cacheDirectory: Option[Path] = None): ClassSource = new JrtSource(cacheDirectory)

  /** The classes of the JVM that is running, as sources: its module image from Java 9 on, and on Java 8, which has no modules, the boot jars (`rt.jar` and the
    * rest of `sun.boot.class.path`). Nothing here touches the module system on Java 8.
    */
  def runningJdk(cacheDirectory: Option[Path] = None): List[ClassSource] =
    if (JavaRelease.running >= 9) List(jdk(cacheDirectory)) else bootJars()

  private def bootJars(): List[ClassSource] =
    Option(System.getProperty("sun.boot.class.path")).toList
      .flatMap(_.split(java.io.File.pathSeparator).toList)
      .filter(_.nonEmpty)
      .map(java.nio.file.Paths.get(_))
      .filter(java.nio.file.Files.exists(_))
      .flatMap(open(_).toOption)

  /** @param cacheDirectory
    *   where a jar keeps its parsed summary between runs; none means nothing is written. Class directories are never summarized: they are what is being edited.
    * @param release
    *   the Java release the application runs on: a multi-release jar's `META-INF/versions/N` entries with N at most this are the classes (the running JDK's
    *   release by default)
    */
  def open(path: Path, cacheDirectory: Option[Path] = None, release: Int = JavaRelease.running): Either[SourceError, ClassSource] =
    if (Files.isDirectory(path)) Right(new DirSource(path))
    else if (Files.isRegularFile(path))
      try Right(new JarSource(path, cacheDirectory, release))
      catch { case e: java.io.IOException => Left(SourceError.CannotOpen(path, String.valueOf(e.getMessage))) }
    else Left(SourceError.NotFound(path))
}
