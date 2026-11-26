package reachablelinkage.linkage

/** Which classes a virtual or interface call can land in. */
sealed abstract class Dispatch extends Product with Serializable {
  def ordinal: Int
}

object Dispatch {

  /** Class hierarchy analysis: the override in any class that has been reached. */

  case object Cha extends Dispatch { def ordinal: Int = 0 }

  /** Rapid type analysis: only classes something has instantiated (`new` in reached code, a constructor reference, a class the application, a service file, a
    * keep rule or reflection by constant name builds), as the target is looked up from the instantiated class, so an inherited method counts. Fewer targets
    * than CHA, so more conflicts are Latent; it cannot see an instance made some way not listed (`Unsafe.allocateInstance`, a class built by a name computed at
    * run time).
    */

  case object Rta extends Dispatch { def ordinal: Int = 1 }
  val values: Array[Dispatch]             = Array(Cha, Rta)
  def fromOrdinal(ordinal: Int): Dispatch = values(ordinal)
}
