package reachablelinkage.fixture

import com.spotify.missinglink.{ConflictChecker, Conflict as MlConflict}
import com.spotify.missinglink.Conflict.ConflictCategory
import com.spotify.missinglink.datamodel.{Artifact, ArtifactBuilder, ArtifactName, ClassTypeDescriptor, DeclaredClass, FieldDependency, MethodDependency}
import java.io.ByteArrayInputStream
import scala.jdk.CollectionConverters.*

import reachablelinkage.classpath.ClassSource
import reachablelinkage.linkage.Conflict
import reachablelinkage.symbols.Symbols

/** Runs spotify/missinglink-core on the same class sources, and normalises its output. */
object MissinglinkOracle:

  /** Comparable form of a conflict. Descriptors are left out: fixtures use unique member names. */
  final case class Key(category: String, from: String, target: String, member: String)

  def load(name: String, src: ClassSource, include: String => Boolean = _ => true): Artifact =
    val classes = new java.util.HashMap[ClassTypeDescriptor, DeclaredClass]()
    src.classNames.filter(include).foreach: n =>
      src.read(n).foreach: bytes =>
        val dc = com.spotify.missinglink.ClassLoader.load(new ByteArrayInputStream(bytes))
        classes.put(dc.className, dc)
    new ArtifactBuilder().name(new ArtifactName(name)).classes(classes).build()

  // The whole runtime image, so both tools resolve exactly the same JDK classes.
  lazy val jdk: Artifact =
    val src = ClassSource.jdk()
    val t0  = System.nanoTime
    try load("jdk", src) finally
      src.close()
      println(s"[oracle] loaded JDK for missinglink in ${(System.nanoTime - t0) / 1_000_000} ms")

  // Standard libraries are large; load each jar once per test run.
  private val providedCache = new java.util.concurrent.ConcurrentHashMap[java.nio.file.Path, Artifact]()

  /**
   * @param provided classpath entries that resolve references but are never checked themselves
   *                 (the Scala library, for Scala fixtures)
   */
  def check(app: ClassSource, libs: Seq[ClassSource], scanLibs: Boolean, provided: Seq[ClassSource] = Nil): Set[Key] =
    val appArtifact      = load("app", app)
    val libArtifacts     = libs.zipWithIndex.map((l, i) => load(s"lib$i", l))
    val providedArtifacts = provided.map(p => providedCache.computeIfAbsent(p.origin, _ => load(p.origin.toString, p)))
    val toCheck          = if scanLibs then appArtifact +: libArtifacts else List(appArtifact)
    val all              = (appArtifact +: libArtifacts) ++ providedArtifacts :+ jdk
    ConflictChecker().check(appArtifact, toCheck.asJava, all.asJava).asScala.map(normalise).toSet

  /**
   * What sbt-missinglink does with `missinglinkScanDependencies := true`: the root artifact is every
   * class on the classpath, and every entry is checked.
   */
  def checkAll(entries: Seq[ClassSource]): Set[Key] =
    val artifacts = entries.map(e => providedCache.computeIfAbsent(e.origin, _ => load(e.origin.toString, e)))
    val merged    = new java.util.HashMap[ClassTypeDescriptor, DeclaredClass]()
    artifacts.foreach(a => merged.putAll(a.classes()))
    val project = new ArtifactBuilder().name(new ArtifactName("project")).classes(merged).build()
    ConflictChecker().check(project, artifacts.asJava, (artifacts :+ jdk).asJava).asScala.map(normalise).toSet

  private def internal(c: ClassTypeDescriptor): String = c.getClassName.replace('.', '/')

  private def normalise(c: MlConflict): Key =
    val from   = internal(c.dependency.fromClass)
    val target = internal(c.dependency.targetClass)
    // For a missing class ML also names the member that was being accessed; the class is the fact.
    val member = c.category match
      case ConflictCategory.CLASS_NOT_FOUND => ""
      case _ =>
        c.dependency match
          case m: MethodDependency => m.targetMethod.name
          case f: FieldDependency  => f.fieldName
          case _                   => ""
    Key(c.category.name, from, target, member)

  /** Our conflicts in the same normalised form. */
  def normalise(c: Conflict, s: Symbols): Option[Key] = c match
    case Conflict.MissingClass(m, f) =>
      Some(Key(ConflictCategory.CLASS_NOT_FOUND.name, s.className(f), s.className(m), ""))
    case Conflict.MissingMethod(o, n, _, f) =>
      Some(Key(ConflictCategory.METHOD_SIGNATURE_NOT_FOUND.name, s.className(f), s.className(o), s.name(n)))
    case Conflict.MethodKindChanged(o, n, _, f) =>
      Some(Key(ConflictCategory.METHOD_SIGNATURE_NOT_FOUND.name, s.className(f), s.className(o), s.name(n)))
    case Conflict.MissingField(o, n, _, f) =>
      Some(Key(ConflictCategory.FIELD_NOT_FOUND.name, s.className(f), s.className(o), s.name(n)))
    // missinglink-core checks neither access flags nor service files; these keys exist so tests can
    // state what we report.
    case Conflict.AbstractMethodNotImplemented(c, d, n, _) =>
      Some(Key("ABSTRACT_METHOD_NOT_IMPLEMENTED", s.className(c), s.className(d), s.name(n)))
    case Conflict.InaccessibleClass(t, f) =>
      Some(Key("INACCESSIBLE_CLASS", s.className(f), s.className(t), ""))
    case Conflict.InaccessibleMethod(o, n, _, f) =>
      Some(Key("INACCESSIBLE_MEMBER", s.className(f), s.className(o), s.name(n)))
    case Conflict.InaccessibleField(o, n, _, f) =>
      Some(Key("INACCESSIBLE_MEMBER", s.className(f), s.className(o), s.name(n)))
    case Conflict.MissingProvider(_, _, _) => None
    case Conflict.Unreadable(_, _) => None
    // Final and interface-versus-class changes are not checked by missinglink-core either.
    case Conflict.IncompatibleHierarchy(_, _, _)      => None
    case Conflict.OverridesFinalMethod(_, _, _, _)    => None
    case Conflict.InterfaceKindChanged(_, _, _, _, _) => None
    case Conflict.AbstractInstantiated(_, _, _)       => None
    case Conflict.FinalFieldWritten(_, _, _, _)       => None
    case Conflict.MissingSignatureType(_, _, _, _)    => None
    case Conflict.NotExported(_, _)                   => None
