package reachablelinkage.classpath

/** What happened when a jar's summary was first needed. */
private[classpath] sealed abstract class SummaryOutcome extends Product with Serializable {
  def ordinal: Int
}

private[classpath] object SummaryOutcome {

  /** No cache directory was given, or the source is not a jar. */

  case object Disabled extends SummaryOutcome { def ordinal: Int = 0 }

  /** A valid summary file was found and used: no class of the jar needs parsing. */

  case object Loaded extends SummaryOutcome { def ordinal: Int = 1 }

  /** There was none (or an invalid one): every class was parsed once and the summary saved. */

  case object Built extends SummaryOutcome { def ordinal: Int = 2 }

  /** Building or reading failed; classes are parsed from the jar as usual. */

  case object Unavailable extends SummaryOutcome { def ordinal: Int = 3 }
  val values: Array[SummaryOutcome]             = Array(Disabled, Loaded, Built, Unavailable)
  def fromOrdinal(ordinal: Int): SummaryOutcome = values(ordinal)
}
