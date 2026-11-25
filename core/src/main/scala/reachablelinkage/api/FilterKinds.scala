package reachablelinkage.api

/** What each of the four package filters looks at and does (the enum itself is Java, so these are extensions). */
extension (kind: FilterKind)

  /** Which end of a conflict the filter looks at. */
  def side: FilterSide = kind match
    case FilterKind.IGNORE_SOURCE_PACKAGES | FilterKind.TARGET_SOURCE_PACKAGES           => FilterSide.SOURCE
    case FilterKind.IGNORE_DESTINATION_PACKAGES | FilterKind.TARGET_DESTINATION_PACKAGES => FilterSide.DESTINATION

  /** Whether it drops what matches or keeps only what matches. */
  def mode: FilterMode = kind match
    case FilterKind.IGNORE_SOURCE_PACKAGES | FilterKind.IGNORE_DESTINATION_PACKAGES => FilterMode.Ignore
    case FilterKind.TARGET_SOURCE_PACKAGES | FilterKind.TARGET_DESTINATION_PACKAGES => FilterMode.Target
