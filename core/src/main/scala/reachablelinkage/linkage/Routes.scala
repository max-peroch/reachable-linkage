package reachablelinkage.linkage

/** Whether a check works out, for each conflict that fails, the methods that lead to it from the application. It costs a pass over the call graph's edges. */
sealed abstract class Routes extends Product with Serializable {
  def ordinal: Int
}

object Routes {

  /** Each conflict that fails carries its route. */
  case object Wanted extends Routes { def ordinal: Int = 0 }

  /** No routes: the default. */
  case object Skipped extends Routes { def ordinal: Int = 1 }

  val values: Array[Routes]             = Array(Wanted, Skipped)
  def fromOrdinal(ordinal: Int): Routes = values(ordinal)
}
