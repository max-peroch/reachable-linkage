package reachablelinkage.api

/**
 * One broken reference. `usedBy` and `existsIn` are classpath entry names; `existsIn` is `<unknown>`
 * if absent. `label` says whether the application can reach the method holding the reference.
 */
final case class LinkageConflict(
    category: ConflictCategory,
    dependency: Dependency,
    usedBy: String,
    existsIn: String,
    reason: String,
    label: ConflictLabel
)
