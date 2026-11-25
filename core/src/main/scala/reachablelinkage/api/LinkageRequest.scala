package reachablelinkage.api

import java.nio.file.Path

import reachablelinkage.classpath.JavaRelease

/**
 * @param projectClasses class directories or jars of the project itself; they start the walk in `App` scope
 * @param classpath      the runtime classpath, in order (first entry wins, as in the JVM)
 * @param excludedEntries names of entries that still resolve references but are never checked
 * @param bootClasspath  JDK classes for Java 8 (`rt.jar`); empty means the running JDK
 * @param precision      `METHOD` (the default: labels each conflict reachable, latent, guarded or probed),
 *                       `CLASS` (every conflict, as sbt-missinglink reports them) or `METHOD_RTA`
 * @param cacheDirectory where to keep the running JDK's class listing between runs (a file per JDK
 *                       image); empty means nothing is written. Saves 300 to 400 ms on a cold JVM.
 * @param keepRules      what frameworks call by reflection (see [[KeepRule]]); the Guice and Jackson packs
 *                       by default. Listing rules replaces them, so include `KeepRule.standard()` to keep them.
 * @param loadedClasses  dotted names of classes a real run loaded (see [[ClassLoadLog]]); each one on the
 *                       classpath is treated as used. Empty by default.
 * @param conflictFilter which conflicts to keep by package (sbt-missinglink's ignore and target settings); none by default
 * @param exportedPackages dotted names of JDK packages the application's JVM exports with `--add-exports`, so naming them is not reported
 * @param javaRelease    the Java release the application runs on (the running JDK's by default): a multi-release jar's
 *                       `META-INF/versions/N` classes with N at most this one are the ones checked, as a JVM would load them
 */
final case class LinkageRequest(
    projectClasses: java.util.List[Path],
    classpath: java.util.List[ClasspathEntry],
    scope: CheckScope,
    excludedEntries: java.util.Set[String],
    bootClasspath: java.util.List[Path],
    precision: CheckPrecision,
    cacheDirectory: java.util.Optional[Path],
    keepRules: java.util.List[KeepRule] = KeepRule.standard(),
    loadedClasses: java.util.Set[String] = java.util.Collections.emptySet[String](),
    conflictFilter: ConflictFilter = ConflictFilter.none,
    javaRelease: Int = JavaRelease.running,
    exportedPackages: java.util.Set[String] = java.util.Collections.emptySet[String]()
):
  /** The standard keep rules. */
  def this(
      projectClasses: java.util.List[Path],
      classpath: java.util.List[ClasspathEntry],
      scope: CheckScope,
      excludedEntries: java.util.Set[String],
      bootClasspath: java.util.List[Path],
      precision: CheckPrecision,
      cacheDirectory: java.util.Optional[Path]
  ) = this(projectClasses, classpath, scope, excludedEntries, bootClasspath, precision, cacheDirectory, KeepRule.standard())

  /** No cache directory. */
  def this(
      projectClasses: java.util.List[Path],
      classpath: java.util.List[ClasspathEntry],
      scope: CheckScope,
      excludedEntries: java.util.Set[String],
      bootClasspath: java.util.List[Path],
      precision: CheckPrecision
  ) = this(projectClasses, classpath, scope, excludedEntries, bootClasspath, precision, java.util.Optional.empty[Path]())

  /** `METHOD` precision (the default) and no cache directory. */
  def this(
      projectClasses: java.util.List[Path],
      classpath: java.util.List[ClasspathEntry],
      scope: CheckScope,
      excludedEntries: java.util.Set[String],
      bootClasspath: java.util.List[Path]
  ) = this(projectClasses, classpath, scope, excludedEntries, bootClasspath, CheckPrecision.METHOD)

object LinkageRequest:
  /** For Java callers: start from the defaults and set what differs (see [[LinkageRequestBuilder]]). */
  def builder(): LinkageRequestBuilder = LinkageRequestBuilder.defaults

  def apply(
      projectClasses: java.util.List[Path],
      classpath: java.util.List[ClasspathEntry],
      scope: CheckScope,
      excludedEntries: java.util.Set[String],
      bootClasspath: java.util.List[Path],
      precision: CheckPrecision
  ): LinkageRequest =
    new LinkageRequest(projectClasses, classpath, scope, excludedEntries, bootClasspath, precision)

  def apply(
      projectClasses: java.util.List[Path],
      classpath: java.util.List[ClasspathEntry],
      scope: CheckScope,
      excludedEntries: java.util.Set[String],
      bootClasspath: java.util.List[Path]
  ): LinkageRequest =
    new LinkageRequest(projectClasses, classpath, scope, excludedEntries, bootClasspath)
