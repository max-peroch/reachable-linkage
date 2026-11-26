package reachablelinkage.classfile

/** How a call site invokes its target. The ordinal is the encoding stored in `ClassInfo.methodRefs`. */
private[reachablelinkage] sealed abstract class CallKind extends Product with Serializable {
  def ordinal: Int
}

private[reachablelinkage] object CallKind {
  case object Instance extends CallKind { def ordinal: Int = 0 }
  case object Static   extends CallKind { def ordinal: Int = 1 }
  val values: Array[CallKind]             = Array(Instance, Static)
  def fromOrdinal(ordinal: Int): CallKind = values(ordinal)
}
