package reachablelinkage.api

import reachablelinkage.symbols.Collections._
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.chaining._
import scala.util.control.NonFatal

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.linkage.{Category, CheckOptions, Checker, Finding, Label, Locator, Precision, Scope}
import reachablelinkage.symbols.{ClassId, Symbols}

/** Java-friendly entry point; the shape sbt-missinglink and a Gradle plugin call. */
object Linkage {
  private val ProjectName = "project"
  private val Unknown     = "<unknown>"

  /** The classpath entries of a request, opened, with what went wrong with the ones that could not be (the project's first, then the classpath, then the boot
    * entries).
    */
  private final case class Opened(project: List[(String, ClassSource)], classpath: List[(String, ClassSource)], boot: List[ClassSource], errors: List[String])

  /** Runs the check. It cannot fail: classpath entries that cannot be opened are skipped and reported in the result's `warnings`, and the rest is a pure
    * function of what was opened.
    */
  def check(request: LinkageRequest): LinkageResult = {
    val toClose = ArrayBuffer.empty[ClassSource]
    // The try starts before anything is opened, so a failure at any step still closes what was.
    try checkOpened(request, openEntries(request, toClose))
    finally
      toClose.foreach(source =>
        try source.close()
        catch { case NonFatal(_) => () }
      ) // one that fails to close must not hide the others
  }

  private def openEntries(request: LinkageRequest, toClose: ArrayBuffer[ClassSource]): Opened = {
    val cacheDirectory                                                                                 = request.cacheDirectory.toScala
    def open(entries: List[(String, java.nio.file.Path)]): (List[String], List[(String, ClassSource)]) = {
      val (errors, opened) =
        entries.map({ case (name, path) => ClassSource.open(path, cacheDirectory, request.javaRelease).map(name -> _).left.map(_.message) }).partitionEithers
      toClose ++= opened.map(_._2)
      (errors, opened)
    }

    val (projectErrors, project)     = open(request.projectClasses.asScala.toList.map(ProjectName -> _))
    val (classpathErrors, classpath) = open(request.classpath.asScala.toList.map(e => e.name -> e.path))
    val (bootErrors, bootEntries)    = request.bootClasspath.asScala.toList.map(path => ClassSource.open(path).left.map(_.message)).partitionEithers
    toClose ++= bootEntries
    // Without boot entries of its own, the request is checked against the running JDK.
    val boot = if (bootEntries.nonEmpty) bootEntries else { val jdk = ClassSource.runningJdk(cacheDirectory); toClose ++= jdk; jdk }
    Opened(project, classpath, boot, projectErrors ++ classpathErrors ++ bootErrors)
  }

  private def checkOpened(request: LinkageRequest, opened: Opened): LinkageResult = {
    val sources          = opened.project ++ opened.classpath
    val projectCount     = opened.project.length
    val traversableCount = sources.length

    val syms    = new Symbols
    val index   = new ClassIndex((sources.map(_._2) ++ opened.boot).toIndexedSeq, syms)
    val names   = sources.map(_._1).toVector
    val checked = names.indices.filterNot(i => request.excludedEntries.contains(names(i))).toVector
    // Reachability is always measured from the project's own classes, even when every class is scanned.
    val scope  = Scope.over(index, rootsOf(request.scope, projectCount, checked), checked, (0 until traversableCount).toVector, (0 until projectCount).toVector)
    val loaded = request.loadedClasses.asScala.iterator.map(_.replace('.', '/')).toSet
    val findings = Checker.findings(
      index,
      scope,
      CheckOptions(precisionOf(request.precision), KeepRule.toKeep(request.keepRules), loaded, request.exportedPackages.asScala.toSet)
    )
    def onClasspath(name: String): Boolean = {
      val c = index.syms.findClass(name)
      !c.isNone && index.exists(c) && index.sourceIndex(c) < traversableCount
    }
    val locator = new Locator(index)

    def entryOf(c: ClassId): String = {
      val i = index.sourceIndex(c)
      if (i >= 0 && i < names.length) names(i) else Unknown
    }

    val conflicts = findings.map(toConflict(_, syms, locator, entryOf, names)).distinct
    val entries   = sources.map({ case (name, source) => EntryInfo(name, source.fingerprint.getOrElse("")) })
    val filtered  = request.conflictFilter(conflicts.asJava)
    val warnings  = opened.errors ++ opened.boot.flatMap(_.problems) ++ logWarning(loaded, onClasspath)
    LinkageResult(filtered.conflicts, warnings.asJava, entries.asJava, filtered.effects)
  }

  /** The entries the walk starts from: the project's own classes, or, for a full scan, every checked entry. */
  private def rootsOf(scope: CheckScope, projectCount: Int, checked: Vector[Int]): Vector[Int] = scope match {
    case CheckScope.App  => (0 until projectCount).toVector
    case CheckScope.Full => checked
  }

  private def precisionOf(precision: CheckPrecision): Precision = precision match {
    case CheckPrecision.CLASS      => Precision.Class
    case CheckPrecision.METHOD     => Precision.Method
    case CheckPrecision.METHOD_RTA => Precision.MethodRta
  }

  /** A log from another program, or from a run that never reached the application, matches nothing. */
  private def logWarning(loaded: Set[String], onClasspath: String => Boolean): List[String] =
    if (loaded.isEmpty || loaded.exists(onClasspath)) Nil
    else List(s"none of the ${loaded.size} classes in the class-load log is on the classpath: is it a log of this application?")

  /** The facade's name for a category, checked by the compiler: a category without one does not compile, and nothing is looked up by name. */
  private def categoryOf(c: Category): ConflictCategory = c match {
    case Category.CLASS_NOT_FOUND                 => ConflictCategory.CLASS_NOT_FOUND
    case Category.METHOD_SIGNATURE_NOT_FOUND      => ConflictCategory.METHOD_SIGNATURE_NOT_FOUND
    case Category.FIELD_NOT_FOUND                 => ConflictCategory.FIELD_NOT_FOUND
    case Category.UNREADABLE_CLASS                => ConflictCategory.UNREADABLE_CLASS
    case Category.MISSING_PROVIDER                => ConflictCategory.MISSING_PROVIDER
    case Category.INACCESSIBLE_CLASS              => ConflictCategory.INACCESSIBLE_CLASS
    case Category.INACCESSIBLE_MEMBER             => ConflictCategory.INACCESSIBLE_MEMBER
    case Category.ABSTRACT_METHOD_NOT_IMPLEMENTED => ConflictCategory.ABSTRACT_METHOD_NOT_IMPLEMENTED
    case Category.INCOMPATIBLE_HIERARCHY          => ConflictCategory.INCOMPATIBLE_HIERARCHY
    case Category.OVERRIDES_FINAL_METHOD          => ConflictCategory.OVERRIDES_FINAL_METHOD
    case Category.INTERFACE_CLASS_MISMATCH        => ConflictCategory.INTERFACE_CLASS_MISMATCH
    case Category.ABSTRACT_INSTANTIATION          => ConflictCategory.ABSTRACT_INSTANTIATION
    case Category.FINAL_FIELD_WRITE               => ConflictCategory.FINAL_FIELD_WRITE
    case Category.MISSING_SIGNATURE_TYPE          => ConflictCategory.MISSING_SIGNATURE_TYPE
    case Category.NOT_EXPORTED                    => ConflictCategory.NOT_EXPORTED
  }

  private def dot(internal: String): String = internal.replace('/', '.')

  private def toConflict(
      f: Finding,
      syms: Symbols,
      locator: Locator,
      entryOf: ClassId => String,
      entryNames: scala.collection.Seq[String]
  ): LinkageConflict = {
    val loc  = locator.locate(f)
    val d    = f.conflict.describe
    val from = syms.className(d.from)
    LinkageConflict(
      categoryOf(d.category),
      Dependency(
        dot(from),
        loc.methodName,
        loc.methodDescriptor,
        loc.line,
        dot(syms.className(d.target)),
        d.member.fold("")({ case (n, _) => syms.name(n) }),
        d.member.fold("")({ case (_, desc) => syms.desc(desc) })
      ),
      // A missing provider belongs to the entry that ships the stale service file, not to the interface's.
      d.declaredIn.fold(entryOf(d.from))(entryNames),
      d.existsIn.fold(Unknown)(entryOf),
      d.reason(syms),
      f.label match {
        case Label.Reachable    => ConflictLabel.REACHABLE
        case Label.Latent       => ConflictLabel.LATENT
        case Label.Guarded      => ConflictLabel.GUARDED
        case Label.Probed       => ConflictLabel.PROBED
        case Label.Reflective   => ConflictLabel.REFLECTIVE
        case Label.Deserialized => ConflictLabel.DESERIALIZED
      }
    )
  }
}
