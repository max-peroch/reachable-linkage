package reachablelinkage.linkage

/** How finely reachability is tracked. */
sealed abstract class Precision extends Product with Serializable {
  def ordinal: Int
}

object Precision {

  /** A class is reachable if anything reachable names it; all its references count. */

  case object Class extends Precision { def ordinal: Int = 0 }

  /** A class's reference counts only if the method holding it is reachable in the call graph (class hierarchy analysis). */

  case object Method extends Precision { def ordinal: Int = 1 }

  /** As `Method`, but a call can only land in a class that is instantiated (see [[Dispatch.Rta]]). */

  case object MethodRta extends Precision { def ordinal: Int = 2 }
  val values: Array[Precision]             = Array(Class, Method, MethodRta)
  def fromOrdinal(ordinal: Int): Precision = values(ordinal)
}
