package reachablelinkage.linkage

/** What a method's owner is now, when that differs from what the call site was compiled against. */
sealed abstract class OwnerKind extends Product with Serializable {
  def ordinal: Int
}

object OwnerKind {
  case object Interface extends OwnerKind { def ordinal: Int = 0 }
  case object Class     extends OwnerKind { def ordinal: Int = 1 }
  val values: Array[OwnerKind]             = Array(Interface, Class)
  def fromOrdinal(ordinal: Int): OwnerKind = values(ordinal)
}
