package reachablelinkage.api

import java.nio.file.{Path, Paths}
import java.util.{List as JList, Map as JMap}

import scala.jdk.CollectionConverters.*

/** Reads a bridge request, saying what is wrong with it if it cannot. */
private[api] object LinkageRequestDecoder:

  def decode(request: JMap[String, AnyRef]): Either[String, LinkageRequest] =
    for
      projectClasses    <- paths(request, "projectClasses")
      _                 <- Either.cond(projectClasses.nonEmpty, (), "'projectClasses' must list at least one path")
      classpath         <- classpathEntries(request)
      scope             <- enumValue(request, "scope", CheckScope.values(), CheckScope.App)
      precision         <- enumValue(request, "precision", CheckPrecision.values(), CheckPrecision.METHOD)
      excluded          <- strings(request, "excludedEntries")
      bootClasspath     <- paths(request, "bootClasspath")
      cacheDirectory    <- optionalString(request, "cacheDirectory")
      javaRelease       <- optionalInt(request, "javaRelease")
      conflictFilter    <- conflictFilter(request)
      keepRules         <- keepRules(request)
      loadedClasses     <- loadedClasses(request)
    yield
      val base = LinkageRequest.builder()
        .withProjectClasses(projectClasses.asJava)
        .withClasspath(classpath.asJava)
        .withScope(scope)
        .withPrecision(precision)
        .withExcludedEntries(excluded.toSet.asJava)
        .withBootClasspath(bootClasspath.asJava)
        .withConflictFilter(conflictFilter)
      withOptionals(base, cacheDirectory, javaRelease, keepRules, loadedClasses).build()

  /** The settings a request may leave out are added only when it gives them. */
  private def withOptionals(
      base: LinkageRequestBuilder,
      cacheDirectory: Option[String],
      javaRelease: Option[Int],
      keepRules: Option[JList[KeepRule]],
      loadedClasses: Option[java.util.Set[String]]
  ): LinkageRequestBuilder =
    val withCache   = cacheDirectory.fold(base)(dir => base.withCacheDirectory(Paths.get(dir)))
    val withRelease = javaRelease.fold(withCache)(withCache.withJavaRelease)
    val withRules   = keepRules.fold(withRelease)(withRelease.withKeepRules)
    loadedClasses.fold(withRules)(withRules.withLoadedClasses)

  private def value(request: JMap[String, AnyRef], key: String): Option[AnyRef] = Option(request.get(key))

  /** A flat list of strings. */
  private def strings(request: JMap[String, AnyRef], key: String): Either[String, List[String]] =
    value(request, key) match
      case None => Right(Nil)
      case Some(list: JList[?]) =>
        val items = list.asScala.toList.collect { case s: String => s }
        Either.cond(items.size == list.size, items, s"'$key' must be a list of strings")
      case Some(_) => Left(s"'$key' must be a list of strings")

  private def paths(request: JMap[String, AnyRef], key: String): Either[String, List[Path]] =
    strings(request, key).map(_.map(Paths.get(_)))

  private def optionalString(request: JMap[String, AnyRef], key: String): Either[String, Option[String]] =
    value(request, key) match
      case None              => Right(None)
      case Some(s: String)   => Right(Some(s))
      case Some(_)           => Left(s"'$key' must be a string")

  private def optionalInt(request: JMap[String, AnyRef], key: String): Either[String, Option[Int]] =
    value(request, key) match
      case None                    => Right(None)
      case Some(n: java.lang.Number) => Right(Some(n.intValue))
      case Some(_)                 => Left(s"'$key' must be an integer")

  /** A list of lists of strings, each exactly `width` long. */
  private def stringRows(request: JMap[String, AnyRef], key: String, width: Int): Either[String, List[List[String]]] =
    def row(candidate: AnyRef): Option[List[String]] = candidate match
      case inner: JList[?] =>
        val items = inner.asScala.toList.collect { case s: String => s }
        Option.when(items.size == inner.size && items.size == width)(items)
      case _ => None
    val problem = s"'$key' must be a list of $width-string lists"
    value(request, key) match
      case None => Right(Nil)
      case Some(rows: JList[?]) =>
        val decoded = rows.asScala.toList.map(row)
        Either.cond(decoded.forall(_.isDefined), decoded.flatten, problem)
      case Some(_) => Left(problem)

  /** The pairs `[name, path]` and `[package, subpackages]`. */
  private def pairs(request: JMap[String, AnyRef], key: String): Either[String, List[(String, String)]] =
    stringRows(request, key, 2).map(_.collect { case List(first, second) => (first, second) })

  private def classpathEntries(request: JMap[String, AnyRef]): Either[String, List[ClasspathEntry]] =
    pairs(request, "classpath").map(_.map((name, path) => ClasspathEntry(name, Paths.get(path))))

  private def enumValue[E <: java.lang.Enum[E]](request: JMap[String, AnyRef], key: String, all: Array[E], default: E): Either[String, E] =
    optionalString(request, key).flatMap:
      case None => Right(default)
      case Some(name) =>
        all.find(_.name.equalsIgnoreCase(name)).toRight(s"'$key' must be one of ${all.map(_.name).mkString(", ")}, not '$name'")

  /** The built-in packs plus the rules of the file `keepRulesFile` names, if there is one. */
  private def keepRules(request: JMap[String, AnyRef]): Either[String, Option[JList[KeepRule]]] =
    optionalString(request, "keepRulesFile").flatMap:
      case None       => Right(None)
      case Some(file) => KeepRule.parse(Paths.get(file)).left.map(_.message).map(own => Some((KeepRule.standard().asScala.toList ++ own).asJava))

  /** The classes the class-load log `classLoadLog` names, if there is one: each one on the classpath is treated as used. */
  private def loadedClasses(request: JMap[String, AnyRef]): Either[String, Option[java.util.Set[String]]] =
    optionalString(request, "classLoadLog").flatMap:
      case None       => Right(None)
      case Some(file) => ClassLoadLog.parse(Paths.get(file)).left.map(_.message).map(log => Some(log.classes))

  private def selectors(request: JMap[String, AnyRef], key: String): Either[String, List[PackageSelector]] =
    pairs(request, key).flatMap: named =>
      named.foldRight[Either[String, List[PackageSelector]]](Right(Nil)): (pair, acc) =>
        for
          rest        <- acc
          subpackages <- pair._2.toBooleanOption.toRight(s"'$key' must pair each package with \"true\" or \"false\", not '${pair._2}'")
          selector    <- PackageSelector.of(pair._1, subpackages).left.map(_.message)
        yield selector :: rest

  private def conflictFilter(request: JMap[String, AnyRef]): Either[String, ConflictFilter] =
    for
      ignoreSource      <- selectors(request, "ignoreSourcePackages")
      targetSource      <- selectors(request, "targetSourcePackages")
      ignoreDestination <- selectors(request, "ignoreDestinationPackages")
      targetDestination <- selectors(request, "targetDestinationPackages")
      filter            <- ConflictFilter.of(ignoreSource, targetSource, ignoreDestination, targetDestination).left.map(_.message)
    yield filter
