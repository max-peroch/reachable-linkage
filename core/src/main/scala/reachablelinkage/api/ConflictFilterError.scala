package reachablelinkage.api

/** Why a [[ConflictFilter]] was rejected. */
sealed abstract class ConflictFilterError extends Product with Serializable {
  import ConflictFilterError._
  def ordinal: Int
  def message: String = this match {
    case IgnoreAndTargetBoth(FilterSide.SOURCE)      => "ignoreSourcePackages and targetSourcePackages cannot be defined together."
    case IgnoreAndTargetBoth(FilterSide.DESTINATION) => "ignoreDestinationPackages and targetDestinationPackages cannot be defined together."
    case InvalidPackageName(n)                       => s"'$n' is not a package name"
  }
}

object ConflictFilterError {

  /** A side has both an ignore list and a target list; sbt-missinglink refuses that too. */

  final case class IgnoreAndTargetBoth(side: FilterSide) extends ConflictFilterError { def ordinal: Int = 0 }

  /** Not a dotted package name such as `com.acme`. */

  final case class InvalidPackageName(name: String) extends ConflictFilterError { def ordinal: Int = 1 }
}
