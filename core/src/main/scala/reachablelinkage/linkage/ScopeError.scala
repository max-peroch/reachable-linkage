package reachablelinkage.linkage

/** Why a [[Scope]] could not be built: one of its entry lists names an entry the index does not have. */
sealed abstract class ScopeError extends Product with Serializable {
  import ScopeError._
  def ordinal: Int
  def message: String = this match {
    case UnknownEntry(list, entry, entries) => s"${list.toString.toLowerCase} names entry $entry, but the index has $entries entries"
  }
}

object ScopeError {
  final case class UnknownEntry(list: ScopeError.List, entry: Int, entries: Int) extends ScopeError { def ordinal: Int = 0 }

  /** Which of the scope's entry lists. */
  sealed abstract class List extends Product with Serializable {
    def ordinal: Int
  }

  object List {
    case object Roots    extends List { def ordinal: Int = 0 }
    case object Checked  extends List { def ordinal: Int = 1 }
    case object Traverse extends List { def ordinal: Int = 2 }
    case object App      extends List { def ordinal: Int = 3 }
    val values: Array[List]             = Array(Roots, Checked, Traverse, App)
    def fromOrdinal(ordinal: Int): List = values(ordinal)
  }
}
