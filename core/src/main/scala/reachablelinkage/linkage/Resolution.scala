package reachablelinkage.linkage

import reachablelinkage.symbols.ClassId

/** Outcome of looking a member up in a class hierarchy. */
private[linkage] sealed abstract class Resolution extends Product with Serializable {
  import Resolution._
  def ordinal: Int
  def isFound: Boolean = this match {
    case Found(_, _) => true
    case _           => false
  }
}

private[linkage] object Resolution {

  /** The member exists. `declaring` is the class that declares it and `member` its index in that class's `methods` (or `fields`, for a field lookup), or
    * [[Resolution.NoMember]] for a signature-polymorphic call, which has no single declaration to point at.
    */

  final case class Found(declaring: ClassId, member: Int) extends Resolution { def ordinal: Int = 0 }
  case object NotFound                                    extends Resolution { def ordinal: Int = 1 }

  /** A member of that name and descriptor exists but with the other static-ness. */

  case object KindMismatch extends Resolution { def ordinal: Int = 2 }

  /** The hierarchy has a hole, so the lookup cannot be decided; `missing` is the absent class. */

  final case class Incomplete(missing: ClassId) extends Resolution { def ordinal: Int = 3 }

  val NoMember: Int = -1
}
