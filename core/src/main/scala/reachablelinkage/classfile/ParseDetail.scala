package reachablelinkage.classfile

/** How much `ClassParser` records. */
private[reachablelinkage] sealed abstract class ParseDetail extends Product with Serializable {
  def ordinal: Int
}

private[reachablelinkage] object ParseDetail {

  /** References only, no debug info: the cheap parse every check uses. */

  case object Lean extends ParseDetail { def ordinal: Int = 0 }

  /** Also the method and source line of each reference; used to report where a conflict is. */

  case object WithPositions extends ParseDetail { def ordinal: Int = 1 }
  val values: Array[ParseDetail]             = Array(Lean, WithPositions)
  def fromOrdinal(ordinal: Int): ParseDetail = values(ordinal)
}
