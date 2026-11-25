package reachablelinkage.api

import java.util.Map as JMap

/** `Linkage.check` for a caller that cannot link against this library's own types: a plugin in another classloader (sbt 1's has Scala 2.12, this library needs
  * the Scala 3 and 2.13 libraries) calls the one static method [[check]] by reflection, and everything that crosses is a `java.util` map, list, string, boolean
  * or integer, so no class of this library, of Scala or of ASM is shared between the two sides.
  *
  * A request is a map; every key is optional unless it says so:
  *   - `projectClasses`: a list of paths, the class directories or jars of the project itself (required)
  *   - `classpath`: a list of `[name, path]` pairs, in classpath order; `name` is what conflicts are attributed to
  *   - `scope`: `App` (the default) or `Full`
  *   - `precision`: `Method` (the default), `Class` or `METHOD_RTA`, as the names of `CheckPrecision` (case is not significant)
  *   - `excludedEntries`: the names of entries that resolve references but are not checked
  *   - `bootClasspath`: a list of paths, for checking against another JDK's `rt.jar`
  *   - `cacheDirectory`: a path
  *   - `javaRelease`: an integer
  *   - `keepRulesFile`: a path to a rules file (see [[KeepRule]]); its rules are added to the built-in packs
  *   - `classLoadLog`: a path to a class-load log of a real run (see [[ClassLoadLog]]); every class it names that is on the classpath counts as used
  *   - `ignoreSourcePackages`, `targetSourcePackages`, `ignoreDestinationPackages`, `targetDestinationPackages`: lists of `[package, "true" | "false"]` pairs
  *     (whether subpackages count), as sbt-missinglink's settings
  *
  * The response is a map too. A request that cannot be used gives `error` (a message) and nothing else; otherwise:
  *   - `warnings`: a list of strings
  *   - `filtered`: a list of `[kind, count]` pairs, one per package filter that was set (`kind` is a `FilterKind` name)
  *   - `conflicts`: a list of maps with `category`, `label`, `usedBy`, `existsIn`, `reason`, `describe`, `fromClass`, `fromMethodName`, `fromMethodDescriptor`,
  *     `fromLineNumber` (integer), `targetClass`, `targetMemberName`, `targetMemberDescriptor`, and two booleans: `failing` (the default fail level,
  *     `FailOn.REACHABLE`, fails on it) and `reportedByMissinglink` (its category is one of the three missinglink-core reports, the view a plugin in
  *     compatibility mode keeps)
  */
object LinkageBridge:

  private type Request  = JMap[String, AnyRef]
  private type Response = JMap[String, AnyRef]

  /** Checks as `request` says and says what was found; never throws for a request that is merely wrong. */
  def check(request: Request): Response =
    LinkageRequestDecoder.decode(request) match
      case Left(problem)  => LinkageResponseEncoder.error(problem)
      case Right(decoded) => LinkageResponseEncoder.encode(Linkage.check(decoded))
