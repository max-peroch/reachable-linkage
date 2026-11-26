package reachablelinkage.linkage

/** How a class's declared superclass or interface no longer fits (the JVM throws `IncompatibleClassChangeError` loading the class). */
sealed abstract class HierarchyChange extends Product with Serializable {
  def ordinal: Int
}

object HierarchyChange {

  /** The superclass became `final`. */

  case object SuperclassIsFinal extends HierarchyChange { def ordinal: Int = 0 }

  /** The superclass became an interface. */

  case object SuperclassIsInterface extends HierarchyChange { def ordinal: Int = 1 }

  /** A type the class implements became a class. */

  case object InterfaceIsClass extends HierarchyChange { def ordinal: Int = 2 }
  val values: Array[HierarchyChange]             = Array(SuperclassIsFinal, SuperclassIsInterface, InterfaceIsClass)
  def fromOrdinal(ordinal: Int): HierarchyChange = values(ordinal)
}
