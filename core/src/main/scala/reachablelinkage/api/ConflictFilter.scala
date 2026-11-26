package reachablelinkage.api

import scala.jdk.CollectionConverters._
import reachablelinkage.api.FilterKinds._

/** Which conflicts to keep by the package of their two ends, with the semantics of sbt-missinglink's four settings (`missinglinkIgnoreSourcePackages`,
  * `missinglinkTargetSourcePackages`, `missinglinkIgnoreDestinationPackages`, `missinglinkTargetDestinationPackages`). Source is the package of the class
  * holding the broken reference, destination the package of the class it points at (an inner class counts as its outer class's package). An ignore list drops
  * what is in any of its packages; a target list keeps only what is in one of them; a side cannot have both. The four are applied in that order, each only if
  * it is not empty. They filter the finished list: reachability and everything else is unchanged. A class in the default package has the package `""`, which no
  * selector matches (sbt-missinglink's own code throws on such a class).
  */
final case class ConflictFilter private (
    ignoreSource: List[PackageSelector],
    targetSource: List[PackageSelector],
    ignoreDestination: List[PackageSelector],
    targetDestination: List[PackageSelector]
) {

  /** The selectors the filter of this kind carries. */
  private def selectorsOf(kind: FilterKind): List[PackageSelector] = kind match {
    case FilterKind.IGNORE_SOURCE_PACKAGES      => ignoreSource
    case FilterKind.TARGET_SOURCE_PACKAGES      => targetSource
    case FilterKind.IGNORE_DESTINATION_PACKAGES => ignoreDestination
    case FilterKind.TARGET_DESTINATION_PACKAGES => targetDestination
  }

  /** The conflicts that pass, in their order, and what each filter that was set removed. */
  def apply(conflicts: java.util.List[LinkageConflict]): ConflictFilter.Outcome = {
    val (kept, effects) = FilterKind.values.toList
      .filter(selectorsOf(_).nonEmpty)
      .foldLeft((conflicts.asScala.toList, List.empty[FilterEffect])) { case ((remaining, done), kind) =>
        val selectors                            = selectorsOf(kind)
        def matched(c: LinkageConflict): Boolean = selectors.exists(_.matches(ConflictFilter.packageOf(c, kind.side)))
        val passing                              = kind.mode match {
          case FilterMode.Ignore => remaining.filterNot(matched)
          case FilterMode.Target => remaining.filter(matched)
        }
        (passing, done :+ FilterEffect(kind, remaining.size - passing.size))
      }
    ConflictFilter.Outcome(kept.asJava, effects.asJava)
  }
}

object ConflictFilter {

  /** The conflicts that passed and what each filter removed. */
  final case class Outcome(conflicts: java.util.List[LinkageConflict], effects: java.util.List[FilterEffect])

  /** No filtering at all. */
  val none: ConflictFilter = new ConflictFilter(Nil, Nil, Nil, Nil)

  /** A filter, or why the lists cannot be used together. */
  def of(
      ignoreSource: List[PackageSelector] = Nil,
      targetSource: List[PackageSelector] = Nil,
      ignoreDestination: List[PackageSelector] = Nil,
      targetDestination: List[PackageSelector] = Nil
  ): Either[ConflictFilterError, ConflictFilter] =
    if (ignoreSource.nonEmpty && targetSource.nonEmpty) Left(ConflictFilterError.IgnoreAndTargetBoth(FilterSide.SOURCE))
    else if (ignoreDestination.nonEmpty && targetDestination.nonEmpty) Left(ConflictFilterError.IgnoreAndTargetBoth(FilterSide.DESTINATION))
    else Right(new ConflictFilter(ignoreSource, targetSource, ignoreDestination, targetDestination))

  /** The package of the conflict's class on this side. */
  private[api] def packageOf(conflict: LinkageConflict, side: FilterSide): String = side match {
    case FilterSide.SOURCE      => packageOf(conflict.dependency.fromClass)
    case FilterSide.DESTINATION => packageOf(conflict.dependency.targetClass)
  }

  /** The package of a dotted class name: everything before the last dot, `""` for the default package. */
  private[api] def packageOf(className: String): String = {
    val dot = className.lastIndexOf('.')
    if (dot < 0) "" else className.substring(0, dot)
  }
}
