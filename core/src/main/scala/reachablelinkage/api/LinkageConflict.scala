package reachablelinkage.api

/** One broken reference. `usedBy` and `existsIn` are classpath entry names; `existsIn` is `<unknown>` if absent. `label` says whether the application can reach
  * the method holding the reference.
  */
final case class LinkageConflict(
    category: ConflictCategory,
    dependency: Dependency,
    usedBy: String,
    existsIn: String,
    reason: String,
    label: ConflictLabel,
    /** The methods that lead from the application to the one holding the reference, as `pkg.Class.method`, first to last; empty unless the request asked for
      * routes and the conflict fails.
      */
    route: java.util.List[String] = java.util.Collections.emptyList[String]()
)
