package reachablelinkage.linkage

/** Why a [[Scope]] could not be built: one of its entry lists names an entry the index does not have. */
enum ScopeError:
  case UnknownEntry(list: ScopeError.List, entry: Int, entries: Int)

  def message: String = this match
    case UnknownEntry(list, entry, entries) => s"${list.toString.toLowerCase} names entry $entry, but the index has $entries entries"

object ScopeError:
  /** Which of the scope's entry lists. */
  enum List:
    case Roots, Checked, Traverse, App
