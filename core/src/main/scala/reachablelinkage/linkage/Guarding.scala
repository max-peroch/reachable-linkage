package reachablelinkage.linkage

import reachablelinkage.classpath.ClassIndex

/** Whether an enclosing `catch` handles the error a conflict would throw. */
private[linkage] object Guarding:
  /**
   * @param inherited the guard every call reaching the method holding the reference carries (0 at
   *                  Class precision, which has no call graph); it only counts for an instruction
   */
  def isGuarded(index: ClassIndex, f: Finding, inherited: Int): Boolean =
    val stops = f.conflict.describe.stoppedBy
    stops != 0 && {
      index.get(f.conflict.inClass) match
        case null => false
        case info =>
          val (caught, passedDown) = f.kind match
            case RefKind.Method => (info.guardOfMethodRef(f.refIndex), inherited)
            case RefKind.Field  => (info.guardOfFieldRef(f.refIndex), inherited)
            case RefKind.Type   => (info.guardOfClassRef(f.refIndex), inherited)
            case _              => (0, 0)
          ((caught | passedDown) & stops) != 0
    }
