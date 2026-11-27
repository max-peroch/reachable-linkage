package reachablelinkage.api

import java.nio.file.Path

/** For Java callers: build a [[LinkageRequest]] from the defaults, setting what differs. It wraps a request, so the fields and their defaults are the request's
  * and nothing is listed twice. Every `with` method returns a new builder and leaves the old one alone, so a builder can be shared. A new request setting needs
  * only a `with` method here.
  */
final case class LinkageRequestBuilder private[api] (request: LinkageRequest) {
  def withProjectClasses(value: java.util.List[Path]): LinkageRequestBuilder      = set(_.copy(projectClasses = value))
  def withClasspath(value: java.util.List[ClasspathEntry]): LinkageRequestBuilder = set(_.copy(classpath = value))
  def withScope(value: CheckScope): LinkageRequestBuilder                         = set(_.copy(scope = value))
  def withExcludedEntries(value: java.util.Set[String]): LinkageRequestBuilder    = set(_.copy(excludedEntries = value))
  def withBootClasspath(value: java.util.List[Path]): LinkageRequestBuilder       = set(_.copy(bootClasspath = value))
  def withPrecision(value: CheckPrecision): LinkageRequestBuilder                 = set(_.copy(precision = value))
  def withCacheDirectory(value: Path): LinkageRequestBuilder                      = set(_.copy(cacheDirectory = java.util.Optional.of(value)))
  def withKeepRules(value: java.util.List[KeepRule]): LinkageRequestBuilder       = set(_.copy(keepRules = value))
  def withLoadedClasses(value: java.util.Set[String]): LinkageRequestBuilder      = set(_.copy(loadedClasses = value))
  def withConflictFilter(value: ConflictFilter): LinkageRequestBuilder            = set(_.copy(conflictFilter = value))
  def withJavaRelease(value: Int): LinkageRequestBuilder                          = set(_.copy(javaRelease = value))
  def withExportedPackages(value: java.util.Set[String]): LinkageRequestBuilder   = set(_.copy(exportedPackages = value))
  def withRoutes(value: Boolean): LinkageRequestBuilder                           = set(_.copy(routes = value))

  def build(): LinkageRequest = request

  private def set(change: LinkageRequest => LinkageRequest): LinkageRequestBuilder = copy(request = change(request))
}

private[api] object LinkageRequestBuilder {

  /** An empty request over the defaults the request itself declares (`METHOD`, the standard rules, no cache, no filter). */
  def defaults: LinkageRequestBuilder =
    LinkageRequestBuilder(
      LinkageRequest(
        java.util.Collections.emptyList[Path](),
        java.util.Collections.emptyList[ClasspathEntry](),
        CheckScope.App,
        java.util.Collections.emptySet[String](),
        java.util.Collections.emptyList[Path]()
      )
    )
}
