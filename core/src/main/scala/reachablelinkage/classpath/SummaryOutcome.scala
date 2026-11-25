package reachablelinkage.classpath

/** What happened when a jar's summary was first needed. */
private[classpath] enum SummaryOutcome:
  /** No cache directory was given, or the source is not a jar. */
  case Disabled
  /** A valid summary file was found and used: no class of the jar needs parsing. */
  case Loaded
  /** There was none (or an invalid one): every class was parsed once and the summary saved. */
  case Built
  /** Building or reading failed; classes are parsed from the jar as usual. */
  case Unavailable
