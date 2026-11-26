package reachablelinkage.api

import scala.jdk.CollectionConverters._

/** `warnings` lists classpath entries that could not be opened; they are skipped. `entries` lists the entries that were checked, in classpath order, so a later
  * run can tell which dependencies changed.
  */
final case class LinkageResult(
    conflicts: java.util.List[LinkageConflict],
    warnings: java.util.List[String],
    entries: java.util.List[EntryInfo],
    filtered: java.util.List[FilterEffect] = java.util.Collections.emptyList[FilterEffect]()
) {

  /** How many conflicts there were before the package filters ran. */
  def conflictsBeforeFilters: Int = conflicts.size + filtered.asScala.map(_.removed).sum

  /** The conflicts of the categories missinglink-core reports (see [[ConflictCategory.reportedByMissinglink]]): what an existing build saw, for a plugin in
    * compatibility mode.
    */
  def missinglinkReports(): java.util.List[LinkageConflict] =
    conflicts.asScala.filter(_.category.reportedByMissinglink).toList.asJava

  /** The conflicts that should fail a build by default: the `REACHABLE` ones. */
  def failing(): java.util.List[LinkageConflict] = failing(FailOn.REACHABLE)

  /** The conflicts that should fail a build under `failOn`. */
  def failing(failOn: FailOn): java.util.List[LinkageConflict] =
    failOn match {
      case FailOn.ANY       => conflicts
      case FailOn.REACHABLE => conflicts.asScala.filter(c => ConflictLabel.counts(c.label)).toList.asJava
      case FailOn.DIRECT    => conflicts.asScala.filter(_.label == ConflictLabel.REACHABLE).toList.asJava
    }
}
