package reachablelinkage.linkage

/** Which reference array of a class a finding points into. */
sealed abstract class RefKind extends Product with Serializable {
  def ordinal: Int
}

object RefKind {
  case object Method extends RefKind { def ordinal: Int = 0 }
  case object Field  extends RefKind { def ordinal: Int = 1 }

  /** A class used by a type instruction, or by a constructor call. */

  case object Type extends RefKind { def ordinal: Int = 2 }

  /** The superclass or an interface: declared by the class, not by an instruction. */

  case object Hierarchy extends RefKind { def ordinal: Int = 3 }

  /** A provider named by a `META-INF/services` file: declared by a resource, not by an instruction. */

  case object Service extends RefKind { def ordinal: Int = 4 }
  val values: Array[RefKind]             = Array(Method, Field, Type, Hierarchy, Service)
  def fromOrdinal(ordinal: Int): RefKind = values(ordinal)
}
