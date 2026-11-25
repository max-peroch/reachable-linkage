package reachablelinkage.classfile

/** How much `ClassParser` records. */
private[reachablelinkage] enum ParseDetail:
  /** References only, no debug info: the cheap parse every check uses. */
  case Lean
  /** Also the method and source line of each reference; used to report where a conflict is. */
  case WithPositions
