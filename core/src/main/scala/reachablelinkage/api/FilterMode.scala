package reachablelinkage.api

/** What a package filter does with the conflicts that match it. */
sealed abstract class FilterMode extends Product with Serializable {
  def ordinal: Int
}

object FilterMode {

  /** Drop them. */

  case object Ignore extends FilterMode { def ordinal: Int = 0 }

  /** Keep only them. */

  case object Target extends FilterMode { def ordinal: Int = 1 }
  val values: Array[FilterMode]             = Array(Ignore, Target)
  def fromOrdinal(ordinal: Int): FilterMode = values(ordinal)
}
