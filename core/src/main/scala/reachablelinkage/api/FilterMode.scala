package reachablelinkage.api

/** What a package filter does with the conflicts that match it. */
enum FilterMode:
  /** Drop them. */
  case Ignore

  /** Keep only them. */
  case Target
