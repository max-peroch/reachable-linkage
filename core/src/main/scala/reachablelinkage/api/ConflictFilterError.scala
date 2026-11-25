package reachablelinkage.api

/** Why a [[ConflictFilter]] was rejected. */
enum ConflictFilterError:
  /** A side has both an ignore list and a target list; sbt-missinglink refuses that too. */
  case IgnoreAndTargetBoth(side: FilterSide)
  /** Not a dotted package name such as `com.acme`. */
  case InvalidPackageName(name: String)

  def message: String = this match
    case IgnoreAndTargetBoth(FilterSide.SOURCE)      => "ignoreSourcePackages and targetSourcePackages cannot be defined together."
    case IgnoreAndTargetBoth(FilterSide.DESTINATION) => "ignoreDestinationPackages and targetDestinationPackages cannot be defined together."
    case InvalidPackageName(n)                       => s"'$n' is not a package name"
