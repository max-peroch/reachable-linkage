package reachablelinkage.linkage

/** Why a class can no longer be built with `new`. */
sealed abstract class Uninstantiable extends Product with Serializable {
  def ordinal: Int
}

object Uninstantiable {

  /** It is now an `abstract` class. */

  case object Abstract extends Uninstantiable { def ordinal: Int = 0 }

  /** It is now an interface. */

  case object Interface extends Uninstantiable { def ordinal: Int = 1 }
  val values: Array[Uninstantiable]             = Array(Abstract, Interface)
  def fromOrdinal(ordinal: Int): Uninstantiable = values(ordinal)
}
