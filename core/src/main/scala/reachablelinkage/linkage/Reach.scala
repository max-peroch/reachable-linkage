package reachablelinkage.linkage

/** How the call graph reaches something: by the application's own calls, through a reflection or keep rule, or only through deserialization. */
private[linkage] sealed abstract class Reach extends Product with Serializable {
  def ordinal: Int
}

private[linkage] object Reach {
  case object Direct          extends Reach { def ordinal: Int = 0 }
  case object Rule            extends Reach { def ordinal: Int = 1 }
  case object Deserialization extends Reach { def ordinal: Int = 2 }
  val values: Array[Reach]             = Array(Direct, Rule, Deserialization)
  def fromOrdinal(ordinal: Int): Reach = values(ordinal)
}
