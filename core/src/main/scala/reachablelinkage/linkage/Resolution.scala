package reachablelinkage.linkage

import reachablelinkage.symbols.ClassId

/** Outcome of looking a member up in a class hierarchy. */
private[linkage] enum Resolution:
  /**
   * The member exists. `declaring` is the class that declares it and `member` its index in that
   * class's `methods` (or `fields`, for a field lookup), or [[Resolution.NoMember]] for a
   * signature-polymorphic call, which has no single declaration to point at.
   */
  case Found(declaring: ClassId, member: Int)
  case NotFound
  /** A member of that name and descriptor exists but with the other static-ness. */
  case KindMismatch
  /** The hierarchy has a hole, so the lookup cannot be decided; `missing` is the absent class. */
  case Incomplete(missing: ClassId)

  def isFound: Boolean = this match
    case Found(_, _) => true
    case _           => false

private[linkage] object Resolution:
  val NoMember: Int = -1
