package reachablelinkage.linkage

/** What a lookup is for. Static and instance methods resolve differently through interfaces. */
private[linkage] sealed abstract class MemberKind extends Product with Serializable {
  def ordinal: Int
}

private[linkage] object MemberKind {
  case object InstanceMethod extends MemberKind { def ordinal: Int = 0 }
  case object StaticMethod   extends MemberKind { def ordinal: Int = 1 }
  case object Field          extends MemberKind { def ordinal: Int = 2 }
  val values: Array[MemberKind]             = Array(InstanceMethod, StaticMethod, Field)
  def fromOrdinal(ordinal: Int): MemberKind = values(ordinal)
}
